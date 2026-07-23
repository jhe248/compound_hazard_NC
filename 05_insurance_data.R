# 05_insurance_data.R
# Insurance inputs shared by the insurance figures and tables:
#   1. joins the insurance spreadsheet to the RES1 building inventory;
#   2. recomputes per-storm wind / flood / surge losses on the priced sample;
#   3. checks how much of NC's compound-exposed stock the priced sample covers.
#
# In : input_data/insurance/Insured_wind_flood_surge_afford.xlsx
#      input_data/inventories/nc_building_inventory.qs
#      input_data/crosswalk/nhgis_blk2010_blk2020_ge_37.csv
#      input_data/damage_functions/wind_damage_func_hazus7.csv
#      input_data/damage_functions/flood_damage_func_hazus7.csv
#      output_data/nc_building_inventory_hazus7.qs
#      output_data/hazard_hurricanes_<chunk>.qs, damage_hurricanes_<chunk>.qs
# Out: output_data/insurance_building_merged.qs / .csv
#      output_data/damage_hurricanes_bldg_inssample.qs
#      output_files/tables/compound_hazard_sample_coverage.csv

library(data.table)
library(qs2)
library(readxl)
library(bit64)

ins_dir             <- file.path(input_dir, "insurance")
damage_function_dir <- file.path(input_dir, "damage_functions")
crosswalk_file      <- file.path(input_dir, "crosswalk", "nhgis_blk2010_blk2020_ge_37.csv")

# ---------------------------------------------------------------------------
# 1. Insurance spreadsheet x RES1 inventory
# ---------------------------------------------------------------------------
building <- as.data.table(qs_read(file.path(input_dir, "inventories", "nc_building_inventory.qs")))
bi <- building[grepl("^RES1", occtype),
               .(bid, x, y, cbfips, occtype, val_cont, val_struct,
                 popam, poppm, bldgtype, num_story)]
bi <- unique(bi)
bi <- bi[!duplicated(bid)]

ins_raw <- as.data.table(read_excel(file.path(ins_dir, "Insured_wind_flood_surge_afford.xlsx")))
ins_raw[, c("Hurricane_Probability", "...15") := NULL]
setnames(ins_raw, "Building ID", "bid")

ins_sample <- merge(ins_raw, bi, by = "bid")
qs_save(ins_sample, file.path(output_dir, "insurance_building_merged.qs"))
fwrite(ins_sample,  file.path(output_dir, "insurance_building_merged.csv"))
rm(building, bi, ins_raw); gc()

ins_sample[, val_total := val_struct + val_cont]

# ---------------------------------------------------------------------------
# 2. Inventory restricted to the priced sample
# ---------------------------------------------------------------------------
building_coords <- qs_read(file.path(output_dir, "nc_building_inventory_hazus7.qs"))
building_coords[, `:=`(x250m = round(x / 0.0025) * 0.0025,
                       y250m = round(y / 0.0025) * 0.0025)]
b0 <- building_coords[, c("bid", "cbfips", "occtype", "val_cont", "val_struct",
                          "num_story", "wind_damage_code",
                          "flood_damage_code_structure", "flood_damage_code_contents",
                          "x", "y", "x250m", "y250m")]
b0 <- b0[bid %in% ins_sample$bid]

cbxw_10_20 <- fread(crosswalk_file)
setnames(cbxw_10_20, "GEOID10", "cbfips")
cbxw_10_20[, cbfips := as.character(cbfips)]
b1 <- merge(b0, cbxw_10_20, all.x = TRUE, allow.cartesian = TRUE)
b1[, ctfips := substr(GEOID20, 1, 11)]
b1[, `:=`(WEIGHT = NULL, PAREA = NULL, cbfips = NULL, x = NULL, y = NULL)]
rm(b0, building_coords, cbxw_10_20); gc()

# ---------------------------------------------------------------------------
# 3. Hazus 7 damage-function lookup tables
# ---------------------------------------------------------------------------
df_wind0  <- fread(file.path(damage_function_dir, "wind_damage_func_hazus7.csv"))
df_flood0 <- fread(file.path(damage_function_dir, "flood_damage_func_hazus7.csv"))

setnames(df_wind0, "sbtName", "wind_damage_code")
df_wind1 <- dcast(df_wind0, wind_damage_code + windspeed ~ loss, value.var = "pc_damage")
setnames(df_wind1, c("Building", "Contents"), c("pc_damage_structure", "pc_damage_contents"))
df_wind1[, c("max_wind_terrain", "max_wind") := list(windspeed, windspeed)]

setnames(df_flood0, "damage_fn_id", "flood_damage_code")
df_flood0[, damage := damage / 100]

df_flood1_structure <- unique(
  df_flood0[loss == "structure", c("flood_damage_code", "depth_ft", "damage")],
  by = c("flood_damage_code", "depth_ft"))
setnames(df_flood1_structure, c("flood_damage_code_structure", "depth_ft", "pc_damage_structure"))
df_flood1_structure[, c("max_inundation_terrain", "max_surge", "max_surge_inundation_terrain") :=
                      list(depth_ft, depth_ft, depth_ft)]

df_flood1_contents <- unique(
  df_flood0[loss == "contents", c("flood_damage_code", "depth_ft", "damage")],
  by = c("flood_damage_code", "depth_ft"))
setnames(df_flood1_contents, c("flood_damage_code_contents", "depth_ft", "pc_damage_contents"))
df_flood1_contents[, c("max_inundation_terrain", "max_surge", "max_surge_inundation_terrain") :=
                     list(depth_ft, depth_ft, depth_ft)]

# ---------------------------------------------------------------------------
# 4. Per-chunk damages on the priced sample
# ---------------------------------------------------------------------------
dmg_list <- vector("list", n_chunk)

for (mychunk in seq_len(n_chunk)) {
  h1 <- qs_read(file.path(output_dir,
                          paste0("hazard_hurricanes_", chunk_zone[mychunk], ".qs")))

  b1_bbox <- b1[, .(xmin = min(x250m), xmax = max(x250m),
                    ymin = min(y250m), ymax = max(y250m))]
  h1_bbox <- h1[, .(xmin = min(x250m), xmax = max(x250m),
                    ymin = min(y250m), ymax = max(y250m))]

  h1 <- h1[x250m <= b1_bbox$xmax &
             y250m >= b1_bbox$ymin & y250m <= b1_bbox$ymax, ]

  bh0 <- merge(b1[x250m >= h1_bbox$xmin, ], h1,
               by = c("x250m", "y250m"), allow.cartesian = TRUE)
  rm(h1); gc()

  bhw0 <- merge(bh0,
                df_wind1[, c("wind_damage_code", "max_wind_terrain",
                             "pc_damage_structure", "pc_damage_contents")],
                by = c("wind_damage_code", "max_wind_terrain"), all.x = TRUE)
  setnames(bhw0, c("pc_damage_structure", "pc_damage_contents"),
           c("pc_wind_damage_structure", "pc_wind_damage_contents"))
  rm(bh0); gc()

  bhwi0 <- merge(bhw0,
                 df_flood1_structure[, c("flood_damage_code_structure",
                                         "max_inundation_terrain", "pc_damage_structure")],
                 by = c("flood_damage_code_structure", "max_inundation_terrain"), all.x = TRUE)
  setnames(bhwi0, "pc_damage_structure", "pc_max_inundation_terrain_damage_structure")
  rm(bhw0); gc()

  bhwi1 <- merge(bhwi0,
                 df_flood1_contents[, c("flood_damage_code_contents",
                                        "max_inundation_terrain", "pc_damage_contents")],
                 by = c("flood_damage_code_contents", "max_inundation_terrain"), all.x = TRUE)
  setnames(bhwi1, "pc_damage_contents", "pc_max_inundation_terrain_damage_contents")
  rm(bhwi0); gc()

  bhwi2 <- merge(bhwi1,
                 df_flood1_structure[, c("flood_damage_code_structure",
                                         "max_surge_inundation_terrain", "pc_damage_structure")],
                 by = c("flood_damage_code_structure", "max_surge_inundation_terrain"), all.x = TRUE)
  setnames(bhwi2, "pc_damage_structure", "pc_max_surge_inundation_terrain_damage_structure")
  rm(bhwi1); gc()

  bhwi3 <- merge(bhwi2,
                 df_flood1_contents[, c("flood_damage_code_contents",
                                        "max_surge_inundation_terrain", "pc_damage_contents")],
                 by = c("flood_damage_code_contents", "max_surge_inundation_terrain"), all.x = TRUE)
  setnames(bhwi3, "pc_damage_contents", "pc_max_surge_inundation_terrain_damage_contents")
  rm(bhwi2); gc()

  bhwi3[, `:=`(
    wind_loss  = fifelse(is.na(pc_wind_damage_structure), 0,
                         pc_wind_damage_structure * val_struct) +
                 fifelse(is.na(pc_wind_damage_contents),  0,
                         pc_wind_damage_contents  * val_cont),
    flood_loss = fifelse(is.na(pc_max_inundation_terrain_damage_structure), 0,
                         pc_max_inundation_terrain_damage_structure * val_struct) +
                 fifelse(is.na(pc_max_inundation_terrain_damage_contents), 0,
                         pc_max_inundation_terrain_damage_contents  * val_cont),
    surge_loss = fifelse(is.na(pc_max_surge_inundation_terrain_damage_structure), 0,
                         pc_max_surge_inundation_terrain_damage_structure * val_struct) +
                 fifelse(is.na(pc_max_surge_inundation_terrain_damage_contents), 0,
                         pc_max_surge_inundation_terrain_damage_contents  * val_cont)
  )]

  keep <- bhwi3[, .(bid, storm_id, wind_loss, flood_loss, surge_loss,
                    val_struct, val_cont)]
  keep[, total_loss := pmin(wind_loss + flood_loss + surge_loss,
                            val_struct + val_cont)]
  keep <- keep[total_loss > 0]

  dmg_list[[mychunk]] <- keep
  rm(bhwi3, keep); gc()
}

bldg_storm <- rbindlist(dmg_list); rm(dmg_list); gc()
qs_save(bldg_storm, file = file.path(output_dir, "damage_hurricanes_bldg_inssample.qs"))
rm(bldg_storm, b1, df_wind0, df_wind1, df_flood0,
   df_flood1_structure, df_flood1_contents); gc()

# ---------------------------------------------------------------------------
# 5. Sample coverage: full NC vs priced sample among compound-exposed buildings
# ---------------------------------------------------------------------------
cells_any <- data.table(x250m = numeric(0), y250m = numeric(0))
cells_ws  <- data.table(x250m = numeric(0), y250m = numeric(0))
cells_wsf <- data.table(x250m = numeric(0), y250m = numeric(0))

for (mychunk in seq_len(n_chunk)) {
  d0 <- qs_read(file.path(output_dir,
                          paste0("damage_hurricanes_", chunk_zone[mychunk], ".qs")))
  d0[, `:=`(
    w_ = fifelse(is.na(wind_damage_structure), 0, as.numeric(wind_damage_structure)) +
      fifelse(is.na(wind_damage_contents),  0, as.numeric(wind_damage_contents)),
    f_ = fifelse(is.na(inundation_terrain_damage_structure), 0, as.numeric(inundation_terrain_damage_structure)) +
      fifelse(is.na(inundation_terrain_damage_contents),  0, as.numeric(inundation_terrain_damage_contents)),
    s_ = fifelse(is.na(surge_inundation_terrain_damage_structure), 0, as.numeric(surge_inundation_terrain_damage_structure)) +
      fifelse(is.na(surge_inundation_terrain_damage_contents),  0, as.numeric(surge_inundation_terrain_damage_contents))
  )]
  d0[, n_perils := (w_ > 0) + (f_ > 0) + (s_ > 0)]

  cells_any <- unique(rbind(cells_any, unique(d0[n_perils >= 2, .(x250m, y250m)])))
  cells_ws  <- unique(rbind(cells_ws,  unique(d0[w_ > 0 & s_ > 0, .(x250m, y250m)])))
  cells_wsf <- unique(rbind(cells_wsf, unique(d0[w_ > 0 & s_ > 0 & f_ > 0, .(x250m, y250m)])))
  rm(d0); gc()
}

bld_all <- qs_read(file.path(output_dir, "nc_building_inventory_hazus7.qs"))
bld_all[, `:=`(x250m = round(x / 0.0025) * 0.0025,
               y250m = round(y / 0.0025) * 0.0025)]
bld_all <- bld_all[, .(bid, x250m, y250m)]
setkey(bld_all, x250m, y250m)

bids_in_cells <- function(cells) {
  setkey(cells, x250m, y250m)
  unique(bld_all[cells, on = c("x250m", "y250m"), nomatch = NULL]$bid)
}

n_sample <- nrow(ins_sample)
summarize_coverage <- function(label, bids) {
  n_full   <- length(bids)
  n_in_smp <- length(intersect(ins_sample$bid, bids))
  data.table(category = label,
             full_NC = n_full, in_sample = n_in_smp,
             sample_size_over_full = n_sample / n_full,
             sample_in_full_over_full = n_in_smp / n_full)
}

coverage <- rbind(
  summarize_coverage("any_compound (>=2 perils)", bids_in_cells(cells_any)),
  summarize_coverage("wind+surge (any)",          bids_in_cells(cells_ws)),
  summarize_coverage("wind+surge+flood (triple)", bids_in_cells(cells_wsf))
)
coverage[, insurance_sample_size := n_sample]

fwrite(coverage, file.path(table_dir, "compound_hazard_sample_coverage.csv"))

rm(bld_all, cells_any, cells_ws, cells_wsf, ins_sample); gc()
