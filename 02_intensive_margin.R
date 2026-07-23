# 02_intensive_margin.R
# Intensive-margin loss statistics for RES1 single-family homes: per-building
# loss, per-hazard conditional damage, intensity-damage bins, home-level
# prevalence and storm-level summaries.
#
# In : output_data/damage_hurricanes.qs
# Out: output_files/tables/RES1_intensive_table1_building_loss_summary.csv
#      output_files/tables/RES1_intensive_table2_hazard_specific_stats.csv
#      output_files/tables/RES1_intensive_table2_home_prevalence.csv
#      output_files/tables/RES1_intensive_table3_loss_ratio_percentiles.csv
#      output_files/tables/RES1_intensive_table4_aggregation_comparison.csv
#      output_files/tables/RES1_intensive_table5_compound_hazard_frequency.csv
#      output_files/tables/RES1_intensive_table6_building_quantile_summary.csv
#      output_files/tables/RES1_intensive_table8_storm_level_detail.csv
#      output_files/tables/RES1_intensive_table8_storm_level_summary.csv
#      output_files/tables/RES1_intensive_table9_intensity_damage_bins.csv
#      output_files/tables/RES1_intensive_table10_hazard_contribution.csv
#      output_files/tables/RES1_intensive_table11_compound_hazard_stats.csv

library(data.table)
library(qs2)

d0 <- qs_read(file.path(output_dir, "damage_hurricanes.qs"))
d0 <- d0[grepl("^RES1", occtype)]

d0[, `:=`(
  wind_loss  = wind_damage_structure + wind_damage_contents,
  flood_loss = inundation_terrain_damage_structure + inundation_terrain_damage_contents,
  surge_loss = surge_inundation_terrain_damage_structure + surge_inundation_terrain_damage_contents,
  total_val  = val_struct + val_cont,
  max_wind   = max_wind_terrain,
  max_flood  = max_inundation_terrain,
  max_surge  = max_surge_inundation_terrain
)]
d0[is.na(wind_loss),  wind_loss  := 0]
d0[is.na(flood_loss), flood_loss := 0]
d0[is.na(surge_loss), surge_loss := 0]
d0[is.na(max_wind),   max_wind   := 0]
d0[is.na(max_flood),  max_flood  := 0]
d0[is.na(max_surge),  max_surge  := 0]

d0[, compound_loss_max     := pmax(wind_loss, flood_loss, surge_loss, na.rm = TRUE)]
d0[, compound_loss_bounded := pmin(wind_loss + flood_loss + surge_loss, total_val)]

d0[, `:=`(
  wind_loss_ratio             = wind_loss             / total_val,
  flood_loss_ratio            = flood_loss            / total_val,
  surge_loss_ratio            = surge_loss            / total_val,
  compound_loss_ratio_max     = compound_loss_max     / total_val,
  compound_loss_ratio_bounded = compound_loss_bounded / total_val
)]
d0[is.infinite(wind_loss_ratio)  | is.nan(wind_loss_ratio),  wind_loss_ratio  := 0]
d0[is.infinite(flood_loss_ratio) | is.nan(flood_loss_ratio), flood_loss_ratio := 0]
d0[is.infinite(surge_loss_ratio) | is.nan(surge_loss_ratio), surge_loss_ratio := 0]
d0[is.infinite(compound_loss_ratio_max),     compound_loss_ratio_max := 0]
d0[is.infinite(compound_loss_ratio_bounded), compound_loss_ratio_bounded := 0]

d0[, `:=`(
  wind_damage_occurs  = wind_loss  > 0,
  flood_damage_occurs = flood_loss > 0,
  surge_damage_occurs = surge_loss > 0
)]

# ---------------------------------------------------------------------------
# Table 1: building-level loss summary
# ---------------------------------------------------------------------------
building_summary <- d0[, .(
  n_observations    = .N,
  mean_wind_loss    = mean(wind_loss,  na.rm = TRUE),
  median_wind_loss  = median(wind_loss, na.rm = TRUE),
  p95_wind_loss     = quantile(wind_loss, 0.95, na.rm = TRUE),
  p99_wind_loss     = quantile(wind_loss, 0.99, na.rm = TRUE),
  mean_flood_loss   = mean(flood_loss, na.rm = TRUE),
  median_flood_loss = median(flood_loss, na.rm = TRUE),
  p95_flood_loss    = quantile(flood_loss, 0.95, na.rm = TRUE),
  p99_flood_loss    = quantile(flood_loss, 0.99, na.rm = TRUE),
  mean_surge_loss   = mean(surge_loss, na.rm = TRUE),
  median_surge_loss = median(surge_loss, na.rm = TRUE),
  p95_surge_loss    = quantile(surge_loss, 0.95, na.rm = TRUE),
  p99_surge_loss    = quantile(surge_loss, 0.99, na.rm = TRUE),
  mean_max_loss     = mean(compound_loss_max, na.rm = TRUE),
  median_max_loss   = median(compound_loss_max, na.rm = TRUE),
  p95_max_loss      = quantile(compound_loss_max, 0.95, na.rm = TRUE),
  p99_max_loss      = quantile(compound_loss_max, 0.99, na.rm = TRUE),
  mean_building_value = mean(total_val, na.rm = TRUE)
)]
fwrite(building_summary, file.path(table_dir, "RES1_intensive_table1_building_loss_summary.csv"))

# ---------------------------------------------------------------------------
# Table 2: hazard-specific stats (conditional on damage)
# ---------------------------------------------------------------------------
hazard_stats <- rbind(
  d0[wind_damage_occurs == TRUE, .(
    hazard = "Wind", n_damaged = .N, homes_per_storm = .N / 604, pct_damaged = .N / nrow(d0) * 100,
    mean_loss = mean(wind_loss, na.rm = TRUE),
    median_loss = median(wind_loss, na.rm = TRUE),
    p25_loss = quantile(wind_loss, 0.25, na.rm = TRUE),
    p75_loss = quantile(wind_loss, 0.75, na.rm = TRUE),
    p95_loss = quantile(wind_loss, 0.95, na.rm = TRUE),
    p99_loss = quantile(wind_loss, 0.99, na.rm = TRUE),
    mean_loss_ratio   = mean(wind_loss_ratio,   na.rm = TRUE),
    median_loss_ratio = median(wind_loss_ratio, na.rm = TRUE),
    p95_loss_ratio    = quantile(wind_loss_ratio, 0.95, na.rm = TRUE),
    p99_loss_ratio    = quantile(wind_loss_ratio, 0.99, na.rm = TRUE),
    mean_intensity   = mean(max_wind,   na.rm = TRUE),
    median_intensity = median(max_wind, na.rm = TRUE)
  )],
  d0[flood_damage_occurs == TRUE, .(
    hazard = "Flood", n_damaged = .N, homes_per_storm = .N / 604, pct_damaged = .N / nrow(d0) * 100,
    mean_loss = mean(flood_loss, na.rm = TRUE),
    median_loss = median(flood_loss, na.rm = TRUE),
    p25_loss = quantile(flood_loss, 0.25, na.rm = TRUE),
    p75_loss = quantile(flood_loss, 0.75, na.rm = TRUE),
    p95_loss = quantile(flood_loss, 0.95, na.rm = TRUE),
    p99_loss = quantile(flood_loss, 0.99, na.rm = TRUE),
    mean_loss_ratio   = mean(flood_loss_ratio,   na.rm = TRUE),
    median_loss_ratio = median(flood_loss_ratio, na.rm = TRUE),
    p95_loss_ratio    = quantile(flood_loss_ratio, 0.95, na.rm = TRUE),
    p99_loss_ratio    = quantile(flood_loss_ratio, 0.99, na.rm = TRUE),
    mean_intensity   = mean(max_flood,   na.rm = TRUE),
    median_intensity = median(max_flood, na.rm = TRUE)
  )],
  d0[surge_damage_occurs == TRUE, .(
    hazard = "Surge", n_damaged = .N, homes_per_storm = .N / 604, pct_damaged = .N / nrow(d0) * 100,
    mean_loss = mean(surge_loss, na.rm = TRUE),
    median_loss = median(surge_loss, na.rm = TRUE),
    p25_loss = quantile(surge_loss, 0.25, na.rm = TRUE),
    p75_loss = quantile(surge_loss, 0.75, na.rm = TRUE),
    p95_loss = quantile(surge_loss, 0.95, na.rm = TRUE),
    p99_loss = quantile(surge_loss, 0.99, na.rm = TRUE),
    mean_loss_ratio   = mean(surge_loss_ratio,   na.rm = TRUE),
    median_loss_ratio = median(surge_loss_ratio, na.rm = TRUE),
    p95_loss_ratio    = quantile(surge_loss_ratio, 0.95, na.rm = TRUE),
    p99_loss_ratio    = quantile(surge_loss_ratio, 0.99, na.rm = TRUE),
    mean_intensity   = mean(max_surge,   na.rm = TRUE),
    median_intensity = median(max_surge, na.rm = TRUE)
  )]
)
fwrite(hazard_stats, file.path(table_dir, "RES1_intensive_table2_hazard_specific_stats.csv"))

# ---------------------------------------------------------------------------
# Table 3: loss-ratio percentiles
# ---------------------------------------------------------------------------
percentiles <- seq(0.1, 0.99, 0.05)
loss_ratio_percentiles <- rbind(
  data.table(hazard = "Wind",  percentile = percentiles,
             loss_ratio = quantile(d0[wind_damage_occurs  == TRUE]$wind_loss_ratio,  percentiles, na.rm = TRUE)),
  data.table(hazard = "Flood", percentile = percentiles,
             loss_ratio = quantile(d0[flood_damage_occurs == TRUE]$flood_loss_ratio, percentiles, na.rm = TRUE)),
  data.table(hazard = "Surge", percentile = percentiles,
             loss_ratio = quantile(d0[surge_damage_occurs == TRUE]$surge_loss_ratio, percentiles, na.rm = TRUE))
)
fwrite(loss_ratio_percentiles, file.path(table_dir, "RES1_intensive_table3_loss_ratio_percentiles.csv"))

# ---------------------------------------------------------------------------
# Table 4: max vs bounded-additive comparison
# ---------------------------------------------------------------------------
comparison_stats <- d0[, .(
  n_observations         = .N,
  sum_individual_losses  = sum(wind_loss + flood_loss + surge_loss, na.rm = TRUE),
  sum_max_losses         = sum(compound_loss_max, na.rm = TRUE),
  sum_bounded_losses     = sum(compound_loss_bounded, na.rm = TRUE),
  mean_individual_sum    = mean(wind_loss + flood_loss + surge_loss, na.rm = TRUE),
  mean_max               = mean(compound_loss_max, na.rm = TRUE),
  mean_bounded           = mean(compound_loss_bounded, na.rm = TRUE),
  pct_max_equals_sum     = sum(abs(compound_loss_max - (wind_loss + flood_loss + surge_loss)) < 0.01) / .N * 100,
  pct_bounded_equals_max = sum(abs(compound_loss_bounded - compound_loss_max) < 0.01) / .N * 100,
  mean_diff_bounded_max  = mean(compound_loss_bounded - compound_loss_max, na.rm = TRUE)
)]
fwrite(comparison_stats, file.path(table_dir, "RES1_intensive_table4_aggregation_comparison.csv"))

# ---------------------------------------------------------------------------
# Table 5: compound-hazard frequency
# ---------------------------------------------------------------------------
d0[, hazard_type := fcase(
  wind_damage_occurs  & !flood_damage_occurs & !surge_damage_occurs, "Wind only",
  !wind_damage_occurs & flood_damage_occurs  & !surge_damage_occurs, "Flood only",
  !wind_damage_occurs & !flood_damage_occurs & surge_damage_occurs,  "Surge only",
  wind_damage_occurs  & surge_damage_occurs  & !flood_damage_occurs, "Wind + Surge",
  wind_damage_occurs  & flood_damage_occurs  & !surge_damage_occurs, "Wind + Flood",
  !wind_damage_occurs & flood_damage_occurs  & surge_damage_occurs,  "Flood + Surge",
  wind_damage_occurs  & flood_damage_occurs  & surge_damage_occurs,  "Wind + Flood + Surge",
  default = "None"
)]
hazard_freq <- d0[, .(
  n_occurrences   = .N,
  pct_total       = .N / nrow(d0) * 100,
  mean_max_loss   = mean(compound_loss_max, na.rm = TRUE),
  median_max_loss = median(compound_loss_max, na.rm = TRUE),
  p95_max_loss    = quantile(compound_loss_max, 0.95, na.rm = TRUE),
  total_loss      = sum(compound_loss_max, na.rm = TRUE),
  pct_total_loss  = sum(compound_loss_max, na.rm = TRUE) / sum(d0$compound_loss_max, na.rm = TRUE) * 100
), by = hazard_type]
hazard_freq <- hazard_freq[order(-n_occurrences)]
fwrite(hazard_freq, file.path(table_dir, "RES1_intensive_table5_compound_hazard_frequency.csv"))

# ---------------------------------------------------------------------------
# Table 6: building-level quantile summary
# ---------------------------------------------------------------------------
building_quantiles <- d0[, .(
  max_loss       = max(compound_loss_max, na.rm = TRUE),
  mean_loss      = mean(compound_loss_max, na.rm = TRUE),
  p50_loss       = quantile(compound_loss_max, 0.50, na.rm = TRUE),
  p95_loss       = quantile(compound_loss_max, 0.95, na.rm = TRUE),
  p99_loss       = quantile(compound_loss_max, 0.99, na.rm = TRUE),
  building_value = first(total_val),
  n_storms       = .N
), by = .(x250m, y250m, occtype)]
building_quantiles[, `:=`(
  mean_loss_ratio = mean_loss / building_value,
  p95_loss_ratio  = p95_loss  / building_value,
  p99_loss_ratio  = p99_loss  / building_value
)]
quantile_summary <- building_quantiles[, .(
  n_buildings        = .N,
  mean_mean_loss     = mean(mean_loss,   na.rm = TRUE),
  median_mean_loss   = median(mean_loss, na.rm = TRUE),
  mean_p95_loss      = mean(p95_loss,    na.rm = TRUE),
  median_p95_loss    = median(p95_loss,  na.rm = TRUE),
  mean_p99_loss      = mean(p99_loss,    na.rm = TRUE),
  median_p99_loss    = median(p99_loss,  na.rm = TRUE),
  total_building_value = sum(building_value, na.rm = TRUE)
)]
fwrite(quantile_summary, file.path(table_dir, "RES1_intensive_table6_building_quantile_summary.csv"))

# ---------------------------------------------------------------------------
# Table 2 companion: share of homes, defined as (x250m, y250m, occtype), hit by
# each loss category in at least one of the 604 storms
# ---------------------------------------------------------------------------
bldg_prev <- d0[, .(
  ever_any      = any(compound_loss_max > 0),
  ever_wind     = any(wind_damage_occurs),
  ever_flood    = any(flood_damage_occurs),
  ever_surge    = any(surge_damage_occurs),
  ever_compound = any((wind_damage_occurs + flood_damage_occurs + surge_damage_occurs) >= 2),
  ever_wind_surge  = any(hazard_type == "Wind + Surge"),
  ever_wind_flood  = any(hazard_type == "Wind + Flood"),
  ever_wfs         = any(hazard_type == "Wind + Flood + Surge"),
  ever_flood_surge = any(hazard_type == "Flood + Surge")
), by = .(x250m, y250m, occtype)]

n_homes <- nrow(bldg_prev)
home_prevalence <- data.table(
  category = c("Any loss", "Wind", "Flood", "Surge", "Compound (2+)",
               "Wind + Surge", "Wind + Flood", "Wind + Flood + Surge", "Flood + Surge"),
  pct_homes_affected = 100 * c(
    mean(bldg_prev$ever_any), mean(bldg_prev$ever_wind), mean(bldg_prev$ever_flood),
    mean(bldg_prev$ever_surge), mean(bldg_prev$ever_compound),
    mean(bldg_prev$ever_wind_surge), mean(bldg_prev$ever_wind_flood),
    mean(bldg_prev$ever_wfs), mean(bldg_prev$ever_flood_surge)),
  n_homes = n_homes
)
fwrite(home_prevalence, file.path(table_dir, "RES1_intensive_table2_home_prevalence.csv"))

# ---------------------------------------------------------------------------
# Table 8: storm-level summaries
# ---------------------------------------------------------------------------
storm_summary <- d0[, .(
  total_wind_loss       = sum(wind_loss,  na.rm = TRUE),
  total_flood_loss      = sum(flood_loss, na.rm = TRUE),
  total_surge_loss      = sum(surge_loss, na.rm = TRUE),
  total_max_loss        = sum(compound_loss_max,     na.rm = TRUE),
  total_bounded_loss    = sum(compound_loss_bounded, na.rm = TRUE),
  n_buildings_damaged   = sum(compound_loss_max > 0),
  mean_loss_per_building = mean(compound_loss_max, na.rm = TRUE),
  max_building_loss      = max(compound_loss_max,  na.rm = TRUE)
), by = storm_id]
storm_summary[, `:=`(
  pct_loss_wind  = total_wind_loss  / total_max_loss * 100,
  pct_loss_flood = total_flood_loss / total_max_loss * 100,
  pct_loss_surge = total_surge_loss / total_max_loss * 100
)]
storm_stats <- storm_summary[, .(
  n_storms             = .N,
  mean_total_loss      = mean(total_max_loss,   na.rm = TRUE),
  median_total_loss    = median(total_max_loss, na.rm = TRUE),
  p95_total_loss       = quantile(total_max_loss, 0.95, na.rm = TRUE),
  max_total_loss       = max(total_max_loss,    na.rm = TRUE),
  mean_buildings_damaged = mean(n_buildings_damaged, na.rm = TRUE),
  storms_with_damage   = sum(total_max_loss > 0)
)]
fwrite(storm_summary, file.path(table_dir, "RES1_intensive_table8_storm_level_detail.csv"))
fwrite(storm_stats,   file.path(table_dir, "RES1_intensive_table8_storm_level_summary.csv"))

# ---------------------------------------------------------------------------
# Table 9: intensity-damage bins
# ---------------------------------------------------------------------------
d0[, wind_bin  := cut(max_wind,  breaks = seq(0, 200, 10), include.lowest = TRUE)]
d0[, flood_bin := cut(max_flood, breaks = seq(0,  30,  2), include.lowest = TRUE)]
d0[, surge_bin := cut(max_surge, breaks = seq(0,  30,  2), include.lowest = TRUE)]

wind_intensity_damage <- d0[wind_damage_occurs == TRUE & !is.na(wind_bin), .(
  hazard = "Wind", n_observations = .N,
  mean_loss = mean(wind_loss, na.rm = TRUE),
  median_loss = median(wind_loss, na.rm = TRUE),
  p95_loss = quantile(wind_loss, 0.95, na.rm = TRUE),
  mean_loss_ratio   = mean(wind_loss_ratio,   na.rm = TRUE),
  median_loss_ratio = median(wind_loss_ratio, na.rm = TRUE),
  p95_loss_ratio    = quantile(wind_loss_ratio, 0.95, na.rm = TRUE)
), by = .(intensity_bin = wind_bin)]

flood_intensity_damage <- d0[flood_damage_occurs == TRUE & !is.na(flood_bin), .(
  hazard = "Flood", n_observations = .N,
  mean_loss = mean(flood_loss, na.rm = TRUE),
  median_loss = median(flood_loss, na.rm = TRUE),
  p95_loss = quantile(flood_loss, 0.95, na.rm = TRUE),
  mean_loss_ratio   = mean(flood_loss_ratio,   na.rm = TRUE),
  median_loss_ratio = median(flood_loss_ratio, na.rm = TRUE),
  p95_loss_ratio    = quantile(flood_loss_ratio, 0.95, na.rm = TRUE)
), by = .(intensity_bin = flood_bin)]

surge_intensity_damage <- d0[surge_damage_occurs == TRUE & !is.na(surge_bin), .(
  hazard = "Surge", n_observations = .N,
  mean_loss = mean(surge_loss, na.rm = TRUE),
  median_loss = median(surge_loss, na.rm = TRUE),
  p95_loss = quantile(surge_loss, 0.95, na.rm = TRUE),
  mean_loss_ratio   = mean(surge_loss_ratio,   na.rm = TRUE),
  median_loss_ratio = median(surge_loss_ratio, na.rm = TRUE),
  p95_loss_ratio    = quantile(surge_loss_ratio, 0.95, na.rm = TRUE)
), by = .(intensity_bin = surge_bin)]

intensity_damage <- rbind(wind_intensity_damage, flood_intensity_damage, surge_intensity_damage)
intensity_damage <- intensity_damage[order(hazard, intensity_bin)]
fwrite(intensity_damage, file.path(table_dir, "RES1_intensive_table9_intensity_damage_bins.csv"))

# ---------------------------------------------------------------------------
# Table 10: hazard contribution to total loss
# ---------------------------------------------------------------------------
d0[, dominant_hazard := fcase(
  wind_loss  > flood_loss & wind_loss  > surge_loss, "Wind",
  flood_loss > wind_loss  & flood_loss > surge_loss, "Flood",
  surge_loss > wind_loss  & surge_loss > flood_loss, "Surge",
  default = "Tie"
)]
hazard_contribution <- d0[compound_loss_max > 0, .(
  total_loss        = sum(compound_loss_max, na.rm = TRUE),
  wind_contribution  = sum(wind_loss,  na.rm = TRUE),
  flood_contribution = sum(flood_loss, na.rm = TRUE),
  surge_contribution = sum(surge_loss, na.rm = TRUE),
  n_wind_dominant    = sum(dominant_hazard == "Wind"),
  n_flood_dominant   = sum(dominant_hazard == "Flood"),
  n_surge_dominant   = sum(dominant_hazard == "Surge")
)]
hazard_contribution[, `:=`(
  pct_wind  = wind_contribution  / total_loss * 100,
  pct_flood = flood_contribution / total_loss * 100,
  pct_surge = surge_contribution / total_loss * 100,
  pct_wind_dominant  = n_wind_dominant  / (n_wind_dominant + n_flood_dominant + n_surge_dominant) * 100,
  pct_flood_dominant = n_flood_dominant / (n_wind_dominant + n_flood_dominant + n_surge_dominant) * 100,
  pct_surge_dominant = n_surge_dominant / (n_wind_dominant + n_flood_dominant + n_surge_dominant) * 100
)]
fwrite(hazard_contribution, file.path(table_dir, "RES1_intensive_table10_hazard_contribution.csv"))

# ---------------------------------------------------------------------------
# Table 11: compound-hazard loss stats
# ---------------------------------------------------------------------------
compound_only <- d0[hazard_type %in% c("Wind + Surge", "Wind + Flood", "Flood + Surge", "Wind + Flood + Surge")]
compound_stats <- compound_only[, .(
  n_occurrences  = .N,
  homes_per_storm = .N / 604,
  pct_of_compound = .N / nrow(compound_only) * 100,
  mean_max_loss   = mean(compound_loss_max,   na.rm = TRUE),
  median_max_loss = median(compound_loss_max, na.rm = TRUE),
  p95_max_loss    = quantile(compound_loss_max, 0.95, na.rm = TRUE),
  p99_max_loss    = quantile(compound_loss_max, 0.99, na.rm = TRUE),
  mean_loss_ratio   = mean(compound_loss_ratio_max,   na.rm = TRUE),
  median_loss_ratio = median(compound_loss_ratio_max, na.rm = TRUE),
  p95_loss_ratio    = quantile(compound_loss_ratio_max, 0.95, na.rm = TRUE),
  total_loss = sum(compound_loss_max, na.rm = TRUE)
), by = hazard_type]
compound_stats <- compound_stats[order(-n_occurrences)]
fwrite(compound_stats, file.path(table_dir, "RES1_intensive_table11_compound_hazard_stats.csv"))

rm(d0, compound_only); gc()
