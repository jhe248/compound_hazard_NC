# 10_flood_diagnostics.R
# Standalone diagnostics for the inland-flood signal, checking the map patterns
# against known NC flood geography:
#   T1  county scoreboard over suspect and benchmark counties
#   T2  named-town check on documented repetitive-loss towns
#   T3  corridor ranking, regulated Roanoke vs unregulated Tar / Neuse / SE
#   T4  statewide top-30 flood-probability blocks
#   T5  extreme conditional loss ratios in small blocks
#   T6  old vs new per-county flood probability, if an old run is available
#
# Not part of the pipeline. Run after 00 with
#   source("code/config.R"); source("code/10_flood_diagnostics.R")
# Set old_damage_file to a pre-fix damage_hurricanes.qs to enable T6.
#
# In : output_data/damage_hurricanes.qs
# Out: output_files/tables/flood_diag_T1..T6*.csv

if (!exists("output_dir")) stop("source code/config.R first")

library(qs2)
library(data.table)
setDTthreads(0)

new_damage_file <- file.path(output_dir, "damage_hurricanes.qs")
old_damage_file <- "/projectnb/cheerbugrp/jhhe/hazard_final/output_data/damage_hurricanes.qs"

# ---------------------------------------------------------------------------
# 1. Reference geography
# ---------------------------------------------------------------------------
county_names <- c(
  "37015" = "Bertie",     "37017" = "Bladen",    "37041" = "Chowan",
  "37047" = "Columbus",   "37049" = "Craven",    "37061" = "Duplin",
  "37065" = "Edgecombe",  "37073" = "Gates",     "37083" = "Halifax",
  "37091" = "Hertford",   "37107" = "Lenoir",    "37117" = "Martin",
  "37127" = "Nash",       "37131" = "Northampton", "37141" = "Pender",
  "37147" = "Pitt",       "37155" = "Robeson",   "37185" = "Warren",
  "37187" = "Washington", "37191" = "Wayne"
)

corridors <- list(
  Roanoke_regulated = c("37185", "37083", "37131", "37117", "37015", "37187"),
  Chowan_fixedbasin = c("37091", "37073", "37041"),
  Tar_unregulated   = c("37127", "37065", "37147"),
  Neuse_unregulated = c("37191", "37107", "37049"),
  SE_blackwater     = c("37155", "37017", "37047", "37141", "37061")
)

towns <- data.table(
  town = c("Windsor (Cashie; Floyd/Matthew)", "Lumberton (Lumber; Matthew/Florence)",
           "Burgaw (NE Cape Fear; Florence)", "Wallace (NE Cape Fear; Florence)",
           "Princeville (Tar; Floyd/Matthew)", "Kinston (Neuse; Floyd/Matthew)",
           "Goldsboro (Neuse; Floyd/Matthew)", "Winton (Chowan; fixed basin)",
           "Scotland Neck (Roanoke below dams)", "Plymouth (lower Roanoke)",
           "Roanoke Rapids (below dam)", "Littleton (LAKE GASTON - suspect)"),
  lon = c(-76.9461, -79.0086, -77.9261, -77.9953, -77.5319, -77.5816,
          -77.9928, -76.9319, -77.4203, -76.7488, -77.6544, -77.9075),
  lat = c( 35.9985,  34.6182,  34.5527,  34.7357,  35.8863,  35.2627,
           35.3849,  36.3960,  36.1296,  35.8668,  36.4615,  36.5060)
)
radius_deg <- 0.04

# ---------------------------------------------------------------------------
# 2. Block-level flood statistics from a damage_hurricanes.qs file
# ---------------------------------------------------------------------------
block_flood_stats <- function(f) {
  d <- qs_read(f); setDT(d)
  d <- d[grepl("^RES1", occtype)]
  n_storms <- uniqueN(d$storm_id)
  d[, `:=`(
    flood_loss = fcoalesce(inundation_terrain_damage_structure, 0) +
                 fcoalesce(inundation_terrain_damage_contents, 0),
    val = val_struct + val_cont,
    max_flood_ft = fcoalesce(max_inundation_terrain, 0)
  )]
  bs <- d[, .(flood_loss = sum(flood_loss), val = sum(val),
              max_flood_ft = max(max_flood_ft),
              lon = mean(x250m), lat = mean(y250m)),
          by = .(GEOID20, storm_id)]
  rm(d); gc()
  blk <- bs[, .(
    n_storms_flooded = sum(flood_loss > 0),
    pr_flood  = sum(flood_loss > 0) / n_storms,
    lr_median = median(fifelse(flood_loss > 0, flood_loss / val, NA_real_), na.rm = TRUE),
    lr_p95    = quantile(fifelse(flood_loss > 0, flood_loss / val, NA_real_), 0.95, na.rm = TRUE),
    max_depth_ft = max(max_flood_ft),
    val = first(val), lon = mean(lon), lat = mean(lat)
  ), by = GEOID20]
  rm(bs); gc()
  blk[, ctfips := substr(GEOID20, 1, 5)]
  blk[, county := fcoalesce(county_names[ctfips], ctfips)]
  blk
}

blk <- block_flood_stats(new_damage_file)

# ---------------------------------------------------------------------------
# 3. T1: county scoreboard
# ---------------------------------------------------------------------------
t1 <- blk[ctfips %in% names(county_names), .(
  n_blocks          = .N,
  n_blocks_flooded  = sum(pr_flood > 0),
  mean_pr_flood     = round(mean(pr_flood), 4),
  p95_pr_flood      = round(quantile(pr_flood, 0.95), 4),
  max_pr_flood      = round(max(pr_flood), 4),
  med_cond_LR       = round(median(lr_median, na.rm = TRUE), 3),
  max_depth_ft      = max(max_depth_ft),
  val_B             = round(sum(val) / 1e9, 2)
), by = county][order(-max_pr_flood)]
fwrite(t1, file.path(table_dir, "flood_diag_T1_county_scoreboard.csv"))

# ---------------------------------------------------------------------------
# 4. T2: named-town check
# ---------------------------------------------------------------------------
t2 <- rbindlist(lapply(seq_len(nrow(towns)), function(i) {
  nb <- blk[abs(lon - towns$lon[i]) < radius_deg & abs(lat - towns$lat[i]) < radius_deg]
  data.table(town = towns$town[i], n_blocks = nrow(nb),
             max_pr_flood  = if (nrow(nb)) round(max(nb$pr_flood), 4) else NA_real_,
             mean_pr_flood = if (nrow(nb)) round(mean(nb$pr_flood), 4) else NA_real_,
             med_cond_LR   = if (nrow(nb)) round(median(nb$lr_median, na.rm = TRUE), 3) else NA_real_,
             max_depth_ft  = if (nrow(nb)) max(nb$max_depth_ft) else NA_real_)
}))
fwrite(t2, file.path(table_dir, "flood_diag_T2_town_check.csv"))

# ---------------------------------------------------------------------------
# 5. T3: corridor ranking
# ---------------------------------------------------------------------------
t3 <- rbindlist(lapply(names(corridors), function(cn) {
  cb <- blk[ctfips %in% corridors[[cn]] & pr_flood > 0]
  data.table(corridor = cn, n_flooded_blocks = nrow(cb),
             med_pr = round(median(cb$pr_flood), 4),
             p95_pr = round(quantile(cb$pr_flood, 0.95), 4),
             max_pr = round(max(cb$pr_flood), 4),
             med_cond_LR = round(median(cb$lr_median, na.rm = TRUE), 3))
}))
fwrite(t3, file.path(table_dir, "flood_diag_T3_corridor_ranking.csv"))

# ---------------------------------------------------------------------------
# 6. T4: statewide top-30 flood-probability blocks
# ---------------------------------------------------------------------------
t4 <- blk[order(-pr_flood)][1:30, .(GEOID20, county, pr_flood = round(pr_flood, 3),
                                    lr_median = round(lr_median, 3),
                                    max_depth_ft, val_M = round(val / 1e6, 2),
                                    lon = round(lon, 4), lat = round(lat, 4))]
fwrite(t4, file.path(table_dir, "flood_diag_T4_top30_blocks.csv"))

# ---------------------------------------------------------------------------
# 7. T5: extreme conditional loss ratios
# ---------------------------------------------------------------------------
t5 <- blk[lr_p95 > 0.6][order(-lr_p95)]
fwrite(t5, file.path(table_dir, "flood_diag_T5_extreme_LR_blocks.csv"))

# ---------------------------------------------------------------------------
# 8. T6: old vs new per-county flood probability
# ---------------------------------------------------------------------------
if (file.exists(old_damage_file)) {
  blk_old <- block_flood_stats(old_damage_file)
  cmp <- merge(blk[, .(GEOID20, county, pr_new = pr_flood)],
               blk_old[, .(GEOID20, pr_old = pr_flood)], by = "GEOID20", all = TRUE)
  cmp[is.na(pr_old), pr_old := 0]; cmp[is.na(pr_new), pr_new := 0]
  t6 <- cmp[substr(GEOID20, 1, 5) %in% names(county_names), .(
    mean_pr_old = round(mean(pr_old), 4),
    mean_pr_new = round(mean(pr_new), 4),
    ratio = round(mean(pr_new) / pmax(mean(pr_old), 1e-6), 1)
  ), by = county][order(-ratio)]
  fwrite(t6, file.path(table_dir, "flood_diag_T6_old_vs_new_county.csv"))
}
