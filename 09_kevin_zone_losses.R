# 09_kevin_zone_losses.R
# Standalone deliverable: building-level wind / flood / surge losses for the
# insurance pricing zones (coastal 1-mile and 1-to-2-mile bands), for every
# storm in the ensemble. Output files and columns match the 12-2025 deliverable.
#
# Not part of the pipeline. Run after 00 with
#   source("code/config.R"); source("code/09_kevin_zone_losses.R")
#
# In : input_data/insurance/zones_1mile.shp, zones_1to2miles.shp (+ sidecars)
#      input_data/damage_functions/wind_damage_func_hazus7.csv
#      input_data/damage_functions/flood_damage_func_hazus7.csv
#      input_data/crosswalk/nhgis_blk2010_blk2020_ge_37.csv
#      output_data/nc_building_inventory_hazus7.qs
#      output_data/hazard_hurricanes_<chunk>.qs
# Out: output_data/coastal_zones_building_losses_all_storms.qs / .csv
#      output_data/coastal_zones_building_summary_stats.qs / .csv

if (!exists("output_dir")) stop("source code/config.R first")

library(sf)
library(qs2)
library(data.table)
library(bit64)

ins_dir             <- file.path(input_dir, "insurance")
damage_function_dir <- file.path(input_dir, "damage_functions")
crosswalk_file      <- file.path(input_dir, "crosswalk", "nhgis_blk2010_blk2020_ge_37.csv")

# ---------------------------------------------------------------------------
# 1. Coastal pricing zones
# ---------------------------------------------------------------------------
zone_1mile     <- st_read(file.path(ins_dir, "zones_1mile.shp"),     quiet = TRUE)
zone_1to2miles <- st_read(file.path(ins_dir, "zones_1to2miles.shp"), quiet = TRUE)

zone_1mile$zone_type     <- "1mile"
zone_1to2miles$zone_type <- "1to2miles"

zone_1mile     <- st_transform(zone_1mile,     crs = 4326)
zone_1to2miles <- st_transform(zone_1to2miles, crs = 4326)

zones_combined <- rbind(
  zone_1mile[,     c("FUNCSTAT00", "ID_Order", "zone_type", "geometry")],
  zone_1to2miles[, c("FUNCSTAT00", "ID_Order", "zone_type", "geometry")]
)

# ---------------------------------------------------------------------------
# 2. Inventory restricted to the zones, with the 2010 -> 2020 block crosswalk
# ---------------------------------------------------------------------------
building_coords <- qs_read(file.path(output_dir, "nc_building_inventory_hazus7.qs"))

building_coords[, `:=`(x250m = round(x / 0.0025) * 0.0025,
                       y250m = round(y / 0.0025) * 0.0025)]

unique_coords <- unique(building_coords[, .(x250m, y250m)])

coords_sf    <- st_as_sf(unique_coords, coords = c("x250m", "y250m"), crs = 4326)
coords_zones <- st_join(coords_sf, zones_combined)
coords_zones <- as.data.table(coords_zones)
coords_zones[, geometry := NULL]

coords_in_zones <- coords_zones[!is.na(ID_Order)]
coords_xy <- st_coordinates(st_as_sf(unique_coords[coords_zones[!is.na(ID_Order), which = TRUE]],
                                     coords = c("x250m", "y250m"), crs = 4326))
coords_in_zones[, `:=`(x250m = coords_xy[, 1], y250m = coords_xy[, 2])]

building_coords_filtered <- merge(building_coords,
                                  coords_in_zones[, .(x250m, y250m, ID_Order, zone_type)],
                                  by = c("x250m", "y250m"))

cbxw_10_20 <- fread(crosswalk_file)
setnames(cbxw_10_20, "GEOID10", "cbfips")
cbxw_10_20[, cbfips := as.character(cbfips)]

b1 <- merge(building_coords_filtered, cbxw_10_20, by = "cbfips", all.x = TRUE, allow.cartesian = TRUE)
b1[, `:=`(WEIGHT = NULL, PAREA = NULL)]

rm(building_coords, building_coords_filtered, coords_zones, coords_in_zones, unique_coords)
gc()

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

df_flood1_structure <- unique(df_flood0[loss == "structure",
                                        c("flood_damage_code", "depth_ft", "damage")],
                              by = c("flood_damage_code", "depth_ft"))
setnames(df_flood1_structure, c("flood_damage_code_structure", "depth_ft", "pc_damage_structure"))
df_flood1_structure[, c("max_inundation_terrain", "max_surge", "max_surge_inundation_terrain") :=
                      list(depth_ft, depth_ft, depth_ft)]

df_flood1_contents <- unique(df_flood0[loss == "contents",
                                       c("flood_damage_code", "depth_ft", "damage")],
                             by = c("flood_damage_code", "depth_ft"))
setnames(df_flood1_contents, c("flood_damage_code_contents", "depth_ft", "pc_damage_contents"))
df_flood1_contents[, c("max_inundation_terrain", "max_surge", "max_surge_inundation_terrain") :=
                     list(depth_ft, depth_ft, depth_ft)]

# ---------------------------------------------------------------------------
# 4. Building x hazard chunk merge and per-storm losses
# ---------------------------------------------------------------------------
b1_bbox <- b1[, .(xmin = min(x250m), xmax = max(x250m),
                  ymin = min(y250m), ymax = max(y250m))]

all_losses <- list()

for (mychunk in seq_len(n_chunk)) {
  h1 <- qs_read(file.path(output_dir,
                          paste0("hazard_hurricanes_", chunk_zone[mychunk], ".qs")))

  h1_bbox <- h1[, .(xmin = min(x250m), xmax = max(x250m),
                    ymin = min(y250m), ymax = max(y250m))]

  if (h1_bbox$xmax < b1_bbox$xmin || h1_bbox$xmin > b1_bbox$xmax ||
      h1_bbox$ymax < b1_bbox$ymin || h1_bbox$ymin > b1_bbox$ymax) {
    rm(h1); gc()
    next
  }

  h1 <- h1[x250m <= b1_bbox$xmax & x250m >= b1_bbox$xmin &
             y250m >= b1_bbox$ymin & y250m <= b1_bbox$ymax, ]

  bh0 <- merge(b1, h1, by = c("x250m", "y250m"), allow.cartesian = TRUE)

  if (nrow(bh0) == 0) {
    rm(h1, bh0); gc()
    next
  }

  bhw0 <- merge(bh0,
                df_wind1[, c("wind_damage_code", "max_wind_terrain",
                             "pc_damage_structure", "pc_damage_contents")],
                by = c("wind_damage_code", "max_wind_terrain"), all.x = TRUE)
  setnames(bhw0, c("pc_damage_structure", "pc_damage_contents"),
           c("pc_wind_damage_structure", "pc_wind_damage_contents"))
  rm(bh0); gc()

  bhwi0 <- merge(bhw0,
                 df_flood1_structure[, c("flood_damage_code_structure", "max_inundation_terrain",
                                         "pc_damage_structure")],
                 by = c("flood_damage_code_structure", "max_inundation_terrain"), all.x = TRUE)
  setnames(bhwi0, "pc_damage_structure", "pc_max_inundation_terrain_damage_structure")
  rm(bhw0); gc()

  bhwi1 <- merge(bhwi0,
                 df_flood1_contents[, c("flood_damage_code_contents", "max_inundation_terrain",
                                        "pc_damage_contents")],
                 by = c("flood_damage_code_contents", "max_inundation_terrain"), all.x = TRUE)
  setnames(bhwi1, "pc_damage_contents", "pc_max_inundation_terrain_damage_contents")
  rm(bhwi0); gc()

  bhwi2 <- merge(bhwi1,
                 df_flood1_structure[, c("flood_damage_code_structure", "max_surge_inundation_terrain",
                                         "pc_damage_structure")],
                 by = c("flood_damage_code_structure", "max_surge_inundation_terrain"), all.x = TRUE)
  setnames(bhwi2, "pc_damage_structure", "pc_max_surge_inundation_terrain_damage_structure")
  rm(bhwi1); gc()

  bhwi3 <- merge(bhwi2,
                 df_flood1_contents[, c("flood_damage_code_contents", "max_surge_inundation_terrain",
                                        "pc_damage_contents")],
                 by = c("flood_damage_code_contents", "max_surge_inundation_terrain"), all.x = TRUE)
  setnames(bhwi3, "pc_damage_contents", "pc_max_surge_inundation_terrain_damage_contents")
  rm(bhwi2); gc()

  bhwi3[, `:=`(
    wind_loss  = (pc_wind_damage_structure * val_struct) + (pc_wind_damage_contents * val_cont),
    flood_loss = (pc_max_inundation_terrain_damage_structure * val_struct) +
                 (pc_max_inundation_terrain_damage_contents * val_cont),
    surge_loss = (pc_max_surge_inundation_terrain_damage_structure * val_struct) +
                 (pc_max_surge_inundation_terrain_damage_contents * val_cont)
  )]

  output_cols <- c("bid", "storm_id", "x", "y", "x250m", "y250m",
                   "GEOID20", "zone_type", "ID_Order",
                   "occtype", "bldgtype", "num_story", "med_yr_blt", "firmzone", "found_type",
                   "val_struct", "val_cont", "popam", "poppm",
                   "wind_damage_code", "flood_damage_code_structure", "flood_damage_code_contents",
                   "max_wind_terrain", "max_inundation_terrain", "max_surge_inundation_terrain",
                   "wind_loss", "flood_loss", "surge_loss")

  available_cols <- intersect(output_cols, names(bhwi3))
  all_losses[[mychunk]] <- bhwi3[, ..available_cols]

  rm(bhwi3, h1); gc()
}

# ---------------------------------------------------------------------------
# 5. Combine, restrict to RES1, write the deliverables
# ---------------------------------------------------------------------------
building_losses_all <- rbindlist(all_losses, use.names = TRUE, fill = TRUE)
rm(all_losses); gc()

building_losses_all <- building_losses_all[grepl("^RES1-", occtype)]

qs_save(building_losses_all,
        file = file.path(output_dir, "coastal_zones_building_losses_all_storms.qs"))
fwrite(building_losses_all,
       file = file.path(output_dir, "coastal_zones_building_losses_all_storms.csv"))

building_summary <- building_losses_all[, .(
  n_storms = .N,
  mean_wind_loss    = mean(wind_loss, na.rm = TRUE),
  median_wind_loss  = median(wind_loss, na.rm = TRUE),
  p95_wind_loss     = quantile(wind_loss, 0.95, na.rm = TRUE),
  mean_flood_loss   = mean(flood_loss, na.rm = TRUE),
  median_flood_loss = median(flood_loss, na.rm = TRUE),
  p95_flood_loss    = quantile(flood_loss, 0.95, na.rm = TRUE),
  mean_surge_loss   = mean(surge_loss, na.rm = TRUE),
  median_surge_loss = median(surge_loss, na.rm = TRUE),
  p95_surge_loss    = quantile(surge_loss, 0.95, na.rm = TRUE),
  pr_wind  = mean(wind_loss > 0, na.rm = TRUE),
  pr_flood = mean(flood_loss > 0, na.rm = TRUE),
  pr_surge = mean(surge_loss > 0, na.rm = TRUE),
  x = first(x),
  y = first(y),
  x250m = first(x250m),
  y250m = first(y250m),
  GEOID20 = first(GEOID20),
  zone_type = first(zone_type),
  ID_Order = first(ID_Order),
  occtype = first(occtype),
  bldgtype = first(bldgtype),
  num_story = first(num_story),
  med_yr_blt = first(med_yr_blt),
  firmzone = first(firmzone),
  found_type = first(found_type),
  val_struct = first(val_struct),
  val_cont = first(val_cont),
  popam = first(popam),
  poppm = first(poppm),
  wind_damage_code = first(wind_damage_code),
  flood_damage_code_structure = first(flood_damage_code_structure),
  flood_damage_code_contents = first(flood_damage_code_contents)
), by = .(bid)]

qs_save(building_summary,
        file = file.path(output_dir, "coastal_zones_building_summary_stats.qs"))
fwrite(building_summary,
       file = file.path(output_dir, "coastal_zones_building_summary_stats.csv"))
