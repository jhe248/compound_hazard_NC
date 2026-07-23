# 00_loss_calculation.R
# Author: Ian Sue Wing, Jiahang He
# Date: 4.24.2026
#
# Reads NC building inventory, 604 hurricane hazard CSVs, and the Hazus 7
# damage functions; computes per-building wind/flood/surge damages for every
# storm, then writes the merged damage table.
#
# Inputs (under input_data/):
#   inventories/nc_building_inventory.qs
#   damage_functions/wind_damage_func_hazus7.csv
#   damage_functions/flood_damage_func_hazus7.csv
#   crosswalk/nhgis_blk2010_blk2020_ge_37.csv
# Hazard CSVs are NOT under input_data/; they are read directly from the shared
# folder given by hazard_dir_csv in config.R (604 storms, 8 latitude bins).
#
# Outputs (under output_data/):
#   nc_building_inventory_hazus7.qs                 building x damage codes
#   hazard_hurricanes_<chunk>.qs                    per-chunk hazard table
#   merged_building_hazard_hurricanes_<chunk>.qs    per-chunk merged table
#   damage_hurricanes_<chunk>.qs                    per-chunk damages
#   damage_hurricanes.qs                            full building x storm damages
#   damage_hurricanes_agg_occtypes.qs               aggregated to grid x block
#   damage_hurricanes_agg_occtypes_block.qs         aggregated to block

require(sf)
require(qs2)
require(data.table)
require(bit64)

inventory_file       <- file.path(input_dir, "inventories", "nc_building_inventory.qs")
damage_function_dir  <- file.path(input_dir, "damage_functions")
crosswalk_file       <- file.path(input_dir, "crosswalk", "nhgis_blk2010_blk2020_ge_37.csv")

# ---------------------------------------------------------------------------
# 1. Build inventory with Hazus 7 wind / flood damage codes
# ---------------------------------------------------------------------------
building_coords <- qs_read(inventory_file)

building_coords[, wind_damage_code := "NA"]

building_coords[bldgtype == "W" & grepl("RES1", occtype) & num_story == 1,  wind_damage_code := "WSF1"]
building_coords[bldgtype == "W" & grepl("RES1", occtype) & num_story >= 2,  wind_damage_code := "WSF2"]
building_coords[bldgtype == "W" & grepl("RES2|RES3|RES4|RES5|RES6|AGR|COM|EDU|GOV|IND|REL", occtype) & num_story == 1,  wind_damage_code := "WMUH1"]
building_coords[bldgtype == "W" & grepl("RES2|RES3|RES4|RES5|RES6|AGR|COM|EDU|GOV|IND|REL", occtype) & num_story == 2,  wind_damage_code := "WMUH2"]
building_coords[bldgtype == "W" & grepl("RES2|RES3|RES4|RES5|RES6|AGR|COM|EDU|GOV|IND|REL", occtype) & num_story >= 3,  wind_damage_code := "WMUH3"]
building_coords[bldgtype == "M" & grepl("RES1", occtype) & num_story == 1,  wind_damage_code := "MSF1"]
building_coords[bldgtype == "M" & grepl("RES1", occtype) & num_story >= 2,  wind_damage_code := "MSF2"]
building_coords[bldgtype == "M" & grepl("RES3|RES4", occtype) & num_story == 1,  wind_damage_code := "MMUH1"]
building_coords[bldgtype == "M" & grepl("RES3|RES4", occtype) & num_story == 2,  wind_damage_code := "MMUH2"]
building_coords[bldgtype == "M" & grepl("RES3|RES4", occtype) & num_story >= 3,  wind_damage_code := "MMUH3"]
building_coords[bldgtype == "M" & grepl("RES2|RES5|RES6", occtype) & num_story %in% c(1:2), wind_damage_code := "MERBL"]
building_coords[bldgtype == "M" & grepl("RES2|RES5|RES6", occtype) & num_story %in% c(3:5), wind_damage_code := "MERBM"]
building_coords[bldgtype == "M" & grepl("RES2|RES5|RES6", occtype) & num_story >= 6, wind_damage_code := "MERBH"]
building_coords[bldgtype == "M" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story %in% c(1:2), wind_damage_code := "MECBL"]
building_coords[bldgtype == "M" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story %in% c(3:5), wind_damage_code := "MECBM"]
building_coords[bldgtype == "M" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story >= 6, wind_damage_code := "MECBH"]
building_coords[bldgtype == "M" & grepl("COM1", occtype) & num_story == 1, wind_damage_code := "MLRM1"]
building_coords[bldgtype == "M" & grepl("COM1", occtype) & num_story >= 2, wind_damage_code := "MLRM2"]
building_coords[bldgtype == "M" & grepl("COM2|IND", occtype), wind_damage_code := "MLRI"]
building_coords[bldgtype == "C" & grepl("RES", occtype) & num_story %in% c(1:2), wind_damage_code := "CERBL"]
building_coords[bldgtype == "C" & grepl("RES", occtype) & num_story %in% c(3:5), wind_damage_code := "CERBM"]
building_coords[bldgtype == "C" & grepl("RES", occtype) & num_story >= 6, wind_damage_code := "CERBH"]
building_coords[bldgtype == "C" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story %in% c(1:2), wind_damage_code := "CECBL"]
building_coords[bldgtype == "C" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story %in% c(3:5), wind_damage_code := "CECBM"]
building_coords[bldgtype == "C" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story >= 6, wind_damage_code := "CECBH"]
building_coords[bldgtype == "S" & grepl("RES", occtype) & num_story %in% c(1:2), wind_damage_code := "SERBL"]
building_coords[bldgtype == "S" & grepl("RES", occtype) & num_story %in% c(3:5), wind_damage_code := "SERBM"]
building_coords[bldgtype == "S" & grepl("RES", occtype) & num_story >= 6, wind_damage_code := "SERBH"]
building_coords[bldgtype == "S" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story %in% c(1:2), wind_damage_code := "SECBL"]
building_coords[bldgtype == "S" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story %in% c(3:5), wind_damage_code := "SECBM"]
building_coords[bldgtype == "S" & grepl("AGR|COM|EDU|GOV|IND|REL", occtype) & num_story >= 6, wind_damage_code := "SECBH"]
building_coords[bldgtype == "H" & grepl("RES", occtype) & med_yr_blt < 1976, wind_damage_code := "MHPHUD"]
building_coords[bldgtype == "H" & grepl("RES", occtype) & med_yr_blt %in% c(1976:1993), wind_damage_code := "MH76HUD"]
building_coords[bldgtype == "H" & grepl("RES", occtype) & med_yr_blt >= 1994, wind_damage_code := "MH94HUDIII"]

building_coords[, flood_damage_code_structure := 0]
building_coords[occtype == "RES1-1SNB", flood_damage_code_structure := 129]
building_coords[occtype == "RES1-1SWB", flood_damage_code_structure := 181]
building_coords[occtype == "RES1-2SNB" & is.na(firmzone), flood_damage_code_structure := 107]
building_coords[occtype == "RES1-2SWB" & is.na(firmzone), flood_damage_code_structure := 108]
building_coords[occtype == "RES1-3SNB" & is.na(firmzone), flood_damage_code_structure := 109]
building_coords[occtype == "RES1-3SWB" & is.na(firmzone), flood_damage_code_structure := 110]
building_coords[occtype == "RES2", flood_damage_code_structure := 189]
building_coords[grepl("RES3", occtype) & found_type != "B", flood_damage_code_structure := 204]
building_coords[grepl("RES3", occtype) & found_type == "B", flood_damage_code_structure := 205]
building_coords[occtype == "RES4", flood_damage_code_structure := 209]
building_coords[occtype == "RES5", flood_damage_code_structure := 214]
building_coords[occtype == "RES6", flood_damage_code_structure := 215]
building_coords[occtype == "COM1", flood_damage_code_structure := 217]
building_coords[occtype == "COM2", flood_damage_code_structure := 341]
building_coords[occtype == "COM3", flood_damage_code_structure := 375]
building_coords[occtype == "COM4", flood_damage_code_structure := 431]
building_coords[occtype == "COM5", flood_damage_code_structure := 467]
building_coords[occtype == "COM6", flood_damage_code_structure := 474]
building_coords[occtype == "COM7", flood_damage_code_structure := 475]
building_coords[occtype == "COM8", flood_damage_code_structure := 493]
building_coords[occtype == "COM9", flood_damage_code_structure := 532]
building_coords[occtype == "COM10", flood_damage_code_structure := 543]
building_coords[occtype == "IND1", flood_damage_code_structure := 545]
building_coords[occtype == "IND2", flood_damage_code_structure := 559]
building_coords[occtype == "IND3", flood_damage_code_structure := 575]
building_coords[occtype == "IND4", flood_damage_code_structure := 586]
building_coords[occtype == "IND5", flood_damage_code_structure := 591]
building_coords[occtype == "IND6", flood_damage_code_structure := 592]
building_coords[occtype == "AGR1", flood_damage_code_structure := 616]
building_coords[occtype == "REL1", flood_damage_code_structure := 624]
building_coords[occtype == "GOV1", flood_damage_code_structure := 631]
building_coords[occtype == "GOV2", flood_damage_code_structure := 640]
building_coords[occtype == "EDU1", flood_damage_code_structure := 643]
building_coords[occtype == "EDU2", flood_damage_code_structure := 652]
building_coords[occtype == "RES1-1SNB" & firmzone == "VE", flood_damage_code_structure := 113]
building_coords[occtype == "RES1-1SWB" & firmzone == "VE", flood_damage_code_structure := 114]
building_coords[occtype == "RES1-2SNB" & firmzone == "VE", flood_damage_code_structure := 115]
building_coords[occtype == "RES1-2SWB" & firmzone == "VE", flood_damage_code_structure := 116]
building_coords[occtype == "RES1-3SNB" & firmzone == "VE", flood_damage_code_structure := 117]
building_coords[occtype == "RES1-3SWB" & firmzone == "VE", flood_damage_code_structure := 118]
building_coords[occtype == "RES2" & firmzone == "VE", flood_damage_code_structure := 190]
building_coords[occtype == "RES3A" & num_story <= 2 & found_type != "B" & firmzone == "VE", flood_damage_code_structure := 204]
building_coords[occtype == "RES3B" & num_story <= 2 & found_type != "B" & firmzone == "VE", flood_damage_code_structure := 204]
building_coords[occtype == "RES1-2SNB" & grepl("A|AE|AH|AO", firmzone), flood_damage_code_structure := 107]
building_coords[occtype == "RES1-2SWB" & grepl("A|AE|AH|AO", firmzone), flood_damage_code_structure := 108]
building_coords[occtype == "RES1-3SNB" & grepl("A|AE|AH|AO", firmzone), flood_damage_code_structure := 109]
building_coords[occtype == "RES1-3SWB" & grepl("A|AE|AH|AO", firmzone), flood_damage_code_structure := 110]

building_coords[, flood_damage_code_contents := 0]
building_coords[occtype == "RES1-1SNB", flood_damage_code_contents := 45]
building_coords[occtype == "RES1-1SWB", flood_damage_code_contents := 69]
building_coords[occtype == "RES1-2SNB" & is.na(firmzone), flood_damage_code_contents := 23]
building_coords[occtype == "RES1-2SWB" & is.na(firmzone), flood_damage_code_contents := 24]
building_coords[occtype == "RES1-3SNB" & is.na(firmzone), flood_damage_code_contents := 25]
building_coords[occtype == "RES1-3SWB" & is.na(firmzone), flood_damage_code_contents := 26]
building_coords[occtype == "RES2", flood_damage_code_contents := 74]
building_coords[grepl("RES3", occtype) & found_type != "B", flood_damage_code_contents := 81]
building_coords[grepl("RES3", occtype) & found_type == "B", flood_damage_code_contents := 82]
building_coords[occtype == "RES4", flood_damage_code_contents := 85]
building_coords[occtype == "RES5", flood_damage_code_contents := 88]
building_coords[occtype == "RES6", flood_damage_code_contents := 89]
building_coords[occtype == "COM1", flood_damage_code_contents := 90]
building_coords[occtype == "COM2", flood_damage_code_contents := 195]
building_coords[occtype == "COM3", flood_damage_code_contents := 240]
building_coords[occtype == "COM4", flood_damage_code_contents := 280]
building_coords[occtype == "COM5", flood_damage_code_contents := 304]
building_coords[occtype == "COM6", flood_damage_code_contents := 309]
building_coords[occtype == "COM7", flood_damage_code_contents := 312]
building_coords[occtype == "COM8", flood_damage_code_contents := 322]
building_coords[occtype == "COM9", flood_damage_code_contents := 352]
building_coords[occtype == "COM10", flood_damage_code_contents := 357]
building_coords[occtype == "IND1", flood_damage_code_contents := 358]
building_coords[occtype == "IND2", flood_damage_code_contents := 384]
building_coords[occtype == "IND3", flood_damage_code_contents := 408]
building_coords[occtype == "IND4", flood_damage_code_contents := 433]
building_coords[occtype == "IND5", flood_damage_code_contents := 442]
building_coords[occtype == "IND6", flood_damage_code_contents := 443]
building_coords[occtype == "AGR1", flood_damage_code_contents := 460]
building_coords[occtype == "REL1", flood_damage_code_contents := 467]
building_coords[occtype == "GOV1", flood_damage_code_contents := 472]
building_coords[occtype == "GOV2", flood_damage_code_contents := 477]
building_coords[occtype == "EDU1", flood_damage_code_contents := 480]
building_coords[occtype == "EDU2", flood_damage_code_contents := 485]
building_coords[grepl("RES1-1S", occtype) & found_type != "B" & !is.na(firmzone), flood_damage_code_contents := 21]
building_coords[grepl("RES1-2S", occtype) & found_type != "B" & !is.na(firmzone), flood_damage_code_contents := 23]
building_coords[grepl("RES1-3S", occtype) & found_type != "B" & !is.na(firmzone), flood_damage_code_contents := 25]
building_coords[occtype == "RES1-1SWB" & !is.na(firmzone), flood_damage_code_contents := 30]
building_coords[occtype == "RES1-2SWB" & !is.na(firmzone), flood_damage_code_contents := 32]
building_coords[occtype == "RES1-3SWB" & !is.na(firmzone), flood_damage_code_contents := 34]
building_coords[grepl("RES2", occtype) & found_type != "B" & !is.na(firmzone), flood_damage_code_contents := 74]
building_coords[grepl("RES2", occtype) & found_type == "B" & !is.na(firmzone), flood_damage_code_contents := 75]

qs_save(building_coords, file = file.path(output_dir, "nc_building_inventory_hazus7.qs"))

# ---------------------------------------------------------------------------
# 2. Building cell coordinates + 2010->2020 block crosswalk
# ---------------------------------------------------------------------------
building_coords[, `:=`(x250m = round(x / 0.0025) * 0.0025,
                       y250m = round(y / 0.0025) * 0.0025)]

b0 <- building_coords[, c("bid", "cbfips", "occtype", "val_cont", "val_struct",
                          "popam", "poppm", "num_story",
                          "wind_damage_code", "flood_damage_code_structure",
                          "flood_damage_code_contents", "x250m", "y250m")]

cbxw_10_20 <- fread(crosswalk_file)
setnames(cbxw_10_20, "GEOID10", "cbfips")
cbxw_10_20[, cbfips := as.character(cbfips)]

b1 <- merge(b0, cbxw_10_20, all.x = TRUE, allow.cartesian = TRUE)
b1[, ctfips := substr(GEOID20, 1, 11)]
b1[, `:=`(WEIGHT = NULL, PAREA = NULL, ctfips = NULL, cbfips = NULL, bid = NULL)]

rm(b0, building_coords, cbxw_10_20); gc()

# ---------------------------------------------------------------------------
# 3. Hazard CSV -> per-chunk QS (unit conversions, 5-mph wind binning)
# ---------------------------------------------------------------------------
hazard_names <- c("storm_id", "x250m", "y250m",
                  "max_wind_terrain", "max_wind",
                  "max_inundation_terrain", "min_inundation_terrain",
                  "cumulative_precip",
                  "max_surge", "max_surge_inundation_terrain")

mps_to_mph    <- 2.23694
meters_to_ft  <- 3.28084

for (mychunk in seq_len(n_chunk)) {
  # 07-2026 package: 18 comment lines, data starts at line 19 (verified 7/7/2026;
  # the old skip = 19 dropped the first data row of each file).
  h0 <- fread(hazard_files[mychunk], skip = 18, header = FALSE, select = c(1:4, 6, 10),
              col.names = hazard_names[c(1:4, 6, 10)])
  h0[, `:=`(
    max_wind_terrain_raw             = round(1.66 * mps_to_mph * ifelse(max_wind_terrain != -9999, max_wind_terrain, NA), 2),
    max_surge_inundation_terrain_raw = round(meters_to_ft * ifelse(max_surge_inundation_terrain != -9999, max_surge_inundation_terrain, NA), 2),
    max_inundation_terrain_raw       = round(meters_to_ft * ifelse(max_inundation_terrain != -9999, max_inundation_terrain, NA), 2),
    max_wind_terrain                 = round(1.66 * mps_to_mph * ifelse(max_wind_terrain != -9999, max_wind_terrain, NA) / 5, 0) * 5,
    max_surge_inundation_terrain     = round(meters_to_ft * ifelse(max_surge_inundation_terrain != -9999, max_surge_inundation_terrain, NA), 0),
    max_inundation_terrain           = round(meters_to_ft * ifelse(max_inundation_terrain != -9999, max_inundation_terrain, NA), 0)
  )]
  qs_save(h0, file = file.path(output_dir,
                               paste0("hazard_hurricanes_", chunk_zone[mychunk], ".qs")))
  rm(h0); gc()
}

# ---------------------------------------------------------------------------
# 4. Building x hazard merge per chunk (bbox crop, then spatial join)
# ---------------------------------------------------------------------------
for (mychunk in seq_len(n_chunk)) {
  h1 <- qs_read(file.path(output_dir,
                          paste0("hazard_hurricanes_", chunk_zone[mychunk], ".qs")))

  b1_bbox <- b1[, .(xmin = min(x250m), xmax = max(x250m),
                    ymin = min(y250m), ymax = max(y250m))]
  h1_bbox <- h1[, .(xmin = min(x250m), xmax = max(x250m),
                    ymin = min(y250m), ymax = max(y250m))]

  h1 <- h1[x250m <= b1_bbox$xmax &
             y250m >= b1_bbox$ymin & y250m <= b1_bbox$ymax, ]
  gc()

  bh0 <- merge(b1[x250m >= h1_bbox$xmin, ], h1,
               by = c("x250m", "y250m"), allow.cartesian = TRUE)

  qs_save(bh0, file = file.path(output_dir,
                                paste0("merged_building_hazard_hurricanes_", chunk_zone[mychunk], ".qs")))
  rm(h1, bh0); gc()
}

if (exists("b1")) rm(b1)
gc()

# ---------------------------------------------------------------------------
# 5. Damage calculation per chunk (apply Hazus damage functions)
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

for (mychunk in seq_len(n_chunk)) {
  bh0 <- qs_read(file.path(output_dir,
                           paste0("merged_building_hazard_hurricanes_", chunk_zone[mychunk], ".qs")))
  bh0[, popam := NULL]; gc()

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
    wind_damage_structure = pc_wind_damage_structure * val_struct,
    wind_damage_contents  = pc_wind_damage_contents  * val_cont,
    inundation_terrain_damage_structure = pc_max_inundation_terrain_damage_structure * val_struct,
    inundation_terrain_damage_contents  = pc_max_inundation_terrain_damage_contents  * val_cont,
    surge_inundation_terrain_damage_structure = pc_max_surge_inundation_terrain_damage_structure * val_struct,
    surge_inundation_terrain_damage_contents  = pc_max_surge_inundation_terrain_damage_contents  * val_cont
  )]

  qs_save(bhwi3, file = file.path(output_dir,
                                  paste0("damage_hurricanes_", chunk_zone[mychunk], ".qs")))
  rm(bhwi3); gc()
}

rm(df_flood0, df_flood1_contents, df_flood1_structure, df_wind0, df_wind1); gc()

# ---------------------------------------------------------------------------
# 6. Aggregate per-chunk damages -> single building x storm table
# ---------------------------------------------------------------------------
ld <- vector("list", n_chunk)

for (mychunk in seq_len(n_chunk)) {
  d0 <- qs_read(file.path(output_dir,
                          paste0("damage_hurricanes_", chunk_zone[mychunk], ".qs")))
  d0[, `:=`(wind_damage_code = NULL,
            flood_damage_code_contents = NULL,
            flood_damage_code_structure = NULL)]
  gc()

  d1 <- d0[, .(
    wind_damage_structure = sum(wind_damage_structure, na.rm = TRUE),
    wind_damage_contents  = sum(wind_damage_contents,  na.rm = TRUE),
    inundation_terrain_damage_structure = sum(inundation_terrain_damage_structure, na.rm = TRUE),
    inundation_terrain_damage_contents  = sum(inundation_terrain_damage_contents,  na.rm = TRUE),
    surge_inundation_terrain_damage_structure = sum(surge_inundation_terrain_damage_structure, na.rm = TRUE),
    surge_inundation_terrain_damage_contents  = sum(surge_inundation_terrain_damage_contents,  na.rm = TRUE),
    val_struct = sum(val_struct, na.rm = TRUE),
    val_cont   = sum(val_cont,   na.rm = TRUE),
    poppm      = sum(poppm,      na.rm = TRUE),
    num_story  = weighted.mean(num_story, w = (val_struct + val_cont), na.rm = TRUE),
    max_surge_inundation_terrain     = max(max_surge_inundation_terrain,     na.rm = TRUE),
    max_inundation_terrain           = max(max_inundation_terrain,           na.rm = TRUE),
    max_wind_terrain                 = max(max_wind_terrain,                 na.rm = TRUE),
    max_wind_terrain_raw             = max(max_wind_terrain_raw,             na.rm = TRUE),
    max_surge_inundation_terrain_raw = max(max_surge_inundation_terrain_raw, na.rm = TRUE),
    max_inundation_terrain_raw       = max(max_inundation_terrain_raw,       na.rm = TRUE)
  ), by = c("x250m", "y250m", "storm_id", "GEOID20", "occtype")]
  rm(d0); gc()

  ld[[mychunk]] <- d1
  rm(d1); gc()
}

d0 <- rbindlist(ld); rm(ld); gc()
qs_save(d0, file = file.path(output_dir, "damage_hurricanes.qs"))

# ---------------------------------------------------------------------------
# 7. Block-level aggregations
# ---------------------------------------------------------------------------
occtype_resid <- c("RES1-2SNB", "RES1-2SWB", "RES1-3SNB", "RES1-3SWB",
                   "RES1-1SWB", "RES1-1SNB",
                   "RES2", "RES3A", "RES3C", "RES3B", "RES3D", "RES3F",
                   "RES4", "RES5", "RES6")

d0[, residential := ifelse(occtype %in% occtype_resid, 1, 0)]

d1 <- d0[, .(
  wind_loss  = sum(wind_damage_contents + wind_damage_structure, na.rm = TRUE),
  flood_loss = sum(inundation_terrain_damage_contents + inundation_terrain_damage_structure, na.rm = TRUE),
  surge_loss = sum(surge_inundation_terrain_damage_contents + surge_inundation_terrain_damage_structure, na.rm = TRUE),
  val   = sum(val_cont + val_struct, na.rm = TRUE),
  poppm = sum(poppm, na.rm = TRUE),
  num_story = weighted.mean(num_story, w = (val_cont + val_struct), na.rm = TRUE)
), by = c("storm_id", "x250m", "y250m", "GEOID20", "residential")]

qs_save(d1, file.path(output_dir, "damage_hurricanes_agg_occtypes.qs"))

d2 <- d1[, .(
  wind_loss  = sum(wind_loss,  na.rm = TRUE),
  flood_loss = sum(flood_loss, na.rm = TRUE),
  surge_loss = sum(surge_loss, na.rm = TRUE),
  val   = sum(val, na.rm = TRUE),
  poppm = sum(poppm, na.rm = TRUE),
  num_story = weighted.mean(num_story, w = val, na.rm = TRUE)
), by = c("storm_id", "GEOID20", "residential")]

qs_save(d2, file.path(output_dir, "damage_hurricanes_agg_occtypes_block.qs"))

rm(d0, d1, d2); gc()
