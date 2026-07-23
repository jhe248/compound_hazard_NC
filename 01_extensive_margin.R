# 01_extensive_margin.R
# Extensive-margin exposure for RES1 single-family homes: probability of being
# hit, asset and population at risk, and joint-hazard splits across the 604
# synthetic hurricanes. Produces Table 1 Panels A, B and C.
#
# In : output_data/nc_building_inventory_hazus7.qs, output_data/damage_hurricanes.qs
# Out: output_files/tables/RES1_extensive_table1_value_by_prob.csv
#      output_files/tables/RES1_extensive_table2_summary_by_hazard.csv
#      output_files/tables/RES1_extensive_table3_by_category.csv
#      output_files/tables/RES1_storm_table1_loss_summary.csv
#      output_files/tables/RES1_building_table1_compound_vs_single.csv
#      output_files/tables/RES1_building_table2_compound_by_prob.csv
#      output_files/tables/RES1_extensive_table_panelA_ci.csv
#      output_files/tables/RES1_extensive_table_panelB_prob_quantiles.csv
#      output_files/tables/RES1_extensive_table_panelC_exposure_at_quantiles.csv
#      output_files/tables/RES1_extensive_panelBC_latex.txt

library(data.table)
library(qs2)

WIND_THRESHOLD  <- 55
SURGE_THRESHOLD <- 0
FLOOD_THRESHOLD <- 0

# ---------------------------------------------------------------------------
# 1. RES1 inventory on the 250 m grid
# ---------------------------------------------------------------------------
building_inv <- qs_read(file.path(output_dir, "nc_building_inventory_hazus7.qs"))
building_inv <- building_inv[grepl("^RES1", occtype)]
building_inv[, `:=`(x250m = round(x / 0.0025) * 0.0025,
                    y250m = round(y / 0.0025) * 0.0025)]

building_cell <- building_inv[, .(
  n_buildings = .N,
  pop_night   = sum(poppm, na.rm = TRUE),
  val_total   = sum(val_struct + val_cont, na.rm = TRUE)
), by = .(x250m, y250m)]

rm(building_inv); gc()

# ---------------------------------------------------------------------------
# 2. RES1 damage table with derived loss columns
# ---------------------------------------------------------------------------
d0 <- qs_read(file.path(output_dir, "damage_hurricanes.qs"))
d0 <- d0[grepl("^RES1", occtype)]

d0[, `:=`(
  max_wind  = max_wind_terrain,
  max_surge = max_surge_inundation_terrain,
  max_flood = max_inundation_terrain,
  wind_loss   = wind_damage_structure + wind_damage_contents,
  surge_loss  = surge_inundation_terrain_damage_structure + surge_inundation_terrain_damage_contents,
  flood_loss  = inundation_terrain_damage_structure + inundation_terrain_damage_contents,
  total_value = val_struct + val_cont
)]
d0[is.na(wind_loss),  wind_loss  := 0]
d0[is.na(surge_loss), surge_loss := 0]
d0[is.na(flood_loss), flood_loss := 0]
d0[is.na(max_wind),   max_wind   := 0]
d0[is.na(max_surge),  max_surge  := 0]
d0[is.na(max_flood),  max_flood  := 0]

d0[, compound_loss := pmax(wind_loss, surge_loss, flood_loss, na.rm = TRUE)]
d0[, `:=`(
  wind_exposed  = ifelse(max_wind  > WIND_THRESHOLD,  1, 0),
  surge_exposed = ifelse(max_surge > SURGE_THRESHOLD, 1, 0),
  flood_exposed = ifelse(max_flood > FLOOD_THRESHOLD, 1, 0)
)]
d0[, hazard_type := fcase(
  wind_exposed == 1 & surge_exposed == 0 & flood_exposed == 0, "wind",
  wind_exposed == 0 & surge_exposed == 1 & flood_exposed == 0, "surge",
  wind_exposed == 0 & surge_exposed == 0 & flood_exposed == 1, "flood",
  wind_exposed == 1 & surge_exposed == 1 & flood_exposed == 0, "wind_surge",
  wind_exposed == 1 & surge_exposed == 0 & flood_exposed == 1, "wind_flood",
  wind_exposed == 0 & surge_exposed == 1 & flood_exposed == 1, "flood_surge",
  wind_exposed == 1 & surge_exposed == 1 & flood_exposed == 1, "wind_flood_surge",
  default = "none"
)]
d0[, loss_ratio := compound_loss / total_value]
d0[is.infinite(loss_ratio) | is.nan(loss_ratio), loss_ratio := 0]

n_storms <- length(unique(d0$storm_id))

# ---------------------------------------------------------------------------
# 3. Cell-level hazard exposure probabilities
# ---------------------------------------------------------------------------
hazard_data <- d0[, .(
  max_wind  = max(max_wind,  na.rm = TRUE),
  max_surge = max(max_surge, na.rm = TRUE),
  max_flood = max(max_flood, na.rm = TRUE)
), by = .(x250m, y250m, storm_id)]
hazard_data[max_wind  == -Inf, max_wind  := 0]
hazard_data[max_surge == -Inf, max_surge := 0]
hazard_data[max_flood == -Inf, max_flood := 0]
hazard_data[, `:=`(
  wind_exposed  = ifelse(max_wind  > WIND_THRESHOLD,  1, 0),
  surge_exposed = ifelse(max_surge > SURGE_THRESHOLD, 1, 0),
  flood_exposed = ifelse(max_flood > FLOOD_THRESHOLD, 1, 0)
)]
hazard_data[, hazard_type := fcase(
  wind_exposed == 1 & surge_exposed == 0 & flood_exposed == 0, "wind",
  wind_exposed == 0 & surge_exposed == 1 & flood_exposed == 0, "surge",
  wind_exposed == 0 & surge_exposed == 0 & flood_exposed == 1, "flood",
  wind_exposed == 1 & surge_exposed == 1 & flood_exposed == 0, "wind_surge",
  wind_exposed == 1 & surge_exposed == 0 & flood_exposed == 1, "wind_flood",
  wind_exposed == 0 & surge_exposed == 1 & flood_exposed == 1, "flood_surge",
  wind_exposed == 1 & surge_exposed == 1 & flood_exposed == 1, "wind_flood_surge",
  default = "none"
)]

hazard_prob <- hazard_data[hazard_type != "none",
                           .(n_occur = .N), by = .(x250m, y250m, hazard_type)]
hazard_prob[, prob := n_occur / n_storms]

hazard_prob_wide <- dcast(hazard_prob, x250m + y250m ~ hazard_type,
                          value.var = "prob", fill = 0)
hazard_prob_merge <- merge(building_cell, hazard_prob_wide,
                           by = c("x250m", "y250m"), all.x = TRUE)

prob_cols <- c("wind", "surge", "flood", "wind_surge", "wind_flood",
               "flood_surge", "wind_flood_surge")
for (col in prob_cols) hazard_prob_merge[is.na(get(col)), (col) := 0]
hazard_prob_merge[, any_hazard := pmax(wind, surge, flood, wind_surge,
                                       wind_flood, flood_surge, wind_flood_surge,
                                       na.rm = TRUE)]

hazard_prob_long <- melt(
  hazard_prob_merge[any_hazard > 0],
  id.vars = c("x250m", "y250m", "n_buildings", "pop_night", "val_total"),
  measure.vars = prob_cols,
  variable.name = "hazard_type", value.name = "probability"
)
hazard_prob_long <- hazard_prob_long[probability > 0]
hazard_prob_long[, prob_bin := cut(
  probability,
  breaks = c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.5, 1),
  labels = c("0-0.1%", "0.1-0.5%", "0.5-1%", "1-5%", "5-10%", "10-20%", "20-50%", "50-100%"),
  include.lowest = TRUE
)]
hazard_prob_long[, hazard_label := factor(
  hazard_type,
  levels = c("wind", "surge", "flood", "wind_surge", "wind_flood",
             "flood_surge", "wind_flood_surge"),
  labels = c("Wind", "Surge", "Flood", "Wind+Surge", "Wind+Flood",
             "Flood+Surge", "Wind+Flood+Surge")
)]
hazard_prob_long[, category := fcase(
  hazard_label %in% c("Wind", "Surge", "Flood"),                "Single",
  hazard_label %in% c("Wind+Surge", "Wind+Flood", "Flood+Surge"), "Compound (2)",
  hazard_label == "Wind+Flood+Surge",                           "Compound (3)"
)]

exposure_summary <- hazard_prob_long[, .(
  n_cells          = .N,
  total_buildings  = sum(n_buildings, na.rm = TRUE),
  total_pop        = sum(pop_night,   na.rm = TRUE),
  total_val        = sum(val_total,   na.rm = TRUE),
  mean_prob        = mean(probability, na.rm = TRUE),
  median_prob      = median(probability, na.rm = TRUE),
  p95_prob         = quantile(probability, 0.95, na.rm = TRUE)
), by = .(hazard_label, category, prob_bin)]

# ---------------------------------------------------------------------------
# 4. Storm-level aggregation
# ---------------------------------------------------------------------------
storm_summary <- d0[, .(
  total_wind_loss     = sum(wind_loss,  na.rm = TRUE),
  total_surge_loss    = sum(surge_loss, na.rm = TRUE),
  total_flood_loss    = sum(flood_loss, na.rm = TRUE),
  total_compound_loss = sum(compound_loss, na.rm = TRUE),
  total_value         = sum(total_value, na.rm = TRUE),
  n_buildings_affected = sum(compound_loss > 0),
  n_buildings_total    = .N
), by = storm_id]
storm_summary[, `:=`(
  wind_loss_ratio     = total_wind_loss     / total_value,
  surge_loss_ratio    = total_surge_loss    / total_value,
  flood_loss_ratio    = total_flood_loss    / total_value,
  compound_loss_ratio = total_compound_loss / total_value
)]
storm_summary_long <- melt(
  storm_summary,
  id.vars = c("storm_id", "total_value", "n_buildings_affected", "n_buildings_total"),
  measure.vars = c("total_wind_loss", "total_surge_loss", "total_flood_loss", "total_compound_loss"),
  variable.name = "loss_type", value.name = "total_loss"
)
storm_summary_long[, loss_label := factor(
  loss_type,
  levels = c("total_wind_loss", "total_surge_loss", "total_flood_loss", "total_compound_loss"),
  labels = c("Wind", "Surge", "Flood", "Compound (MAX)")
)]

# ---------------------------------------------------------------------------
# 5. Building-level hazard probabilities
# ---------------------------------------------------------------------------
building_hazard_prob <- d0[, .(
  n_wind             = sum(hazard_type == "wind"),
  n_surge            = sum(hazard_type == "surge"),
  n_flood            = sum(hazard_type == "flood"),
  n_wind_surge       = sum(hazard_type == "wind_surge"),
  n_wind_flood       = sum(hazard_type == "wind_flood"),
  n_flood_surge      = sum(hazard_type == "flood_surge"),
  n_wind_flood_surge = sum(hazard_type == "wind_flood_surge"),
  n_any_hazard       = sum(hazard_type != "none"),
  total_value        = first(total_value),
  pop_night          = first(poppm)
), by = .(x250m, y250m, occtype)]
building_hazard_prob[, `:=`(
  prob_wind             = n_wind             / n_storms,
  prob_surge            = n_surge            / n_storms,
  prob_flood            = n_flood            / n_storms,
  prob_wind_surge       = n_wind_surge       / n_storms,
  prob_wind_flood       = n_wind_flood       / n_storms,
  prob_flood_surge      = n_flood_surge      / n_storms,
  prob_wind_flood_surge = n_wind_flood_surge / n_storms,
  prob_any              = n_any_hazard       / n_storms
)]
building_category <- building_hazard_prob[prob_any > 0]
building_category[, hazard_category := fcase(
  prob_wind > 0 & prob_surge == 0 & prob_flood == 0 & prob_wind_surge == 0 &
    prob_wind_flood == 0 & prob_flood_surge == 0 & prob_wind_flood_surge == 0, "Single",
  prob_surge > 0 & prob_wind == 0 & prob_flood == 0 & prob_wind_surge == 0 &
    prob_wind_flood == 0 & prob_flood_surge == 0 & prob_wind_flood_surge == 0, "Single",
  prob_flood > 0 & prob_wind == 0 & prob_surge == 0 & prob_wind_surge == 0 &
    prob_wind_flood == 0 & prob_flood_surge == 0 & prob_wind_flood_surge == 0, "Single",
  (prob_wind_surge + prob_wind_flood + prob_flood_surge) > 0 & prob_wind_flood_surge == 0, "Compound (2)",
  prob_wind_flood_surge > 0, "Compound (3)",
  default = "Mixed"
)]

compound_prob_long <- melt(
  building_category[hazard_category %in% c("Compound (2)", "Compound (3)")],
  id.vars = c("x250m", "y250m", "occtype", "total_value", "pop_night", "hazard_category"),
  measure.vars = c("prob_wind_surge", "prob_wind_flood", "prob_flood_surge", "prob_wind_flood_surge"),
  variable.name = "hazard_type", value.name = "probability"
)
compound_prob_long <- compound_prob_long[probability > 0]
compound_prob_long[, hazard_label := factor(
  hazard_type,
  levels = c("prob_wind_surge", "prob_wind_flood", "prob_flood_surge", "prob_wind_flood_surge"),
  labels = c("Wind+Surge", "Wind+Flood", "Flood+Surge", "Wind+Flood+Surge")
)]
compound_prob_long[, prob_bin := cut(
  probability,
  breaks = c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.5, 1),
  labels = c("0-0.1%", "0.1-0.5%", "0.5-1%", "1-5%", "5-10%", "10-20%", "20-50%", "50-100%"),
  include.lowest = TRUE
)]

rm(d0, hazard_data); gc()

# ---------------------------------------------------------------------------
# 6. Exposure, storm and building CSV tables
# ---------------------------------------------------------------------------
table1 <- dcast(exposure_summary, hazard_label + category ~ prob_bin,
                value.var = "total_val", fun.aggregate = sum)
table1[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = 3:ncol(table1)]
for (col in 3:ncol(table1)) set(table1, j = col, value = round(table1[[col]] / 1e9, 2))
table1[, Pct := round(100 * Total / sum(Total), 1)]
table1 <- table1[order(-Total)]
fwrite(table1, file.path(table_dir, "RES1_extensive_table1_value_by_prob.csv"))

table2 <- exposure_summary[, .(
  Cells       = format(sum(n_cells),         big.mark = ","),
  Buildings   = format(sum(total_buildings), big.mark = ","),
  Population  = format(round(sum(total_pop)), big.mark = ","),
  `Value ($B)` = sprintf("%.2f", sum(total_val) / 1e9),
  `Mean Prob`   = sprintf("%.3f", weighted.mean(mean_prob,   n_cells)),
  `Median Prob` = sprintf("%.3f", weighted.mean(median_prob, n_cells))
), by = .(hazard_label, category)]
table2 <- table2[order(-as.numeric(gsub(",", "", Buildings)))]
fwrite(table2, file.path(table_dir, "RES1_extensive_table2_summary_by_hazard.csv"))

table3 <- exposure_summary[, .(
  `Value ($B)` = sprintf("%.2f", sum(total_val) / 1e9),
  Population   = format(round(sum(total_pop)), big.mark = ","),
  Buildings    = format(sum(total_buildings),  big.mark = ","),
  `Pct Value`  = sprintf("%.1f%%", 100 * sum(total_val) / sum(exposure_summary$total_val))
), by = category]
table3 <- table3[order(category)]
fwrite(table3, file.path(table_dir, "RES1_extensive_table3_by_category.csv"))

storm_table <- storm_summary_long[, .(
  `Mean Loss ($B)`   = sprintf("%.2f", mean(total_loss, na.rm = TRUE)              / 1e9),
  `Median Loss ($B)` = sprintf("%.2f", median(total_loss, na.rm = TRUE)            / 1e9),
  `P95 Loss ($B)`    = sprintf("%.2f", quantile(total_loss, 0.95, na.rm = TRUE)    / 1e9),
  `Max Loss ($B)`    = sprintf("%.2f", max(total_loss, na.rm = TRUE)               / 1e9),
  `Storms with Loss` = sum(total_loss > 0),
  `Pct Storms`       = sprintf("%.1f%%", 100 * sum(total_loss > 0) / .N)
), by = loss_label]
fwrite(storm_table, file.path(table_dir, "RES1_storm_table1_loss_summary.csv"))

building_summary <- building_category[, .(
  n_buildings = .N,
  total_value = sum(total_value, na.rm = TRUE),
  total_pop   = sum(pop_night,   na.rm = TRUE),
  mean_prob   = mean(prob_any,   na.rm = TRUE)
), by = hazard_category]
building_summary[, `:=`(
  pct_buildings = 100 * n_buildings / sum(n_buildings),
  pct_value     = 100 * total_value / sum(total_value),
  pct_pop       = 100 * total_pop   / sum(total_pop)
)]
fwrite(building_summary, file.path(table_dir, "RES1_building_table1_compound_vs_single.csv"))

compound_summary <- compound_prob_long[, .(
  n_buildings = .N,
  total_value = sum(total_value, na.rm = TRUE),
  total_pop   = sum(pop_night,   na.rm = TRUE)
), by = .(hazard_label, prob_bin)]
compound_summary[, pct_value := 100 * total_value / sum(total_value), by = hazard_label]
fwrite(compound_summary, file.path(table_dir, "RES1_building_table2_compound_by_prob.csv"))

# ---------------------------------------------------------------------------
# 7. Table 1 Panel A: exposed shares with Wilson 95% intervals
# ---------------------------------------------------------------------------
N_cells <- nrow(hazard_prob_merge)
N_homes <- sum(hazard_prob_merge$n_buildings, na.rm = TRUE)
N_pop   <- sum(hazard_prob_merge$pop_night, na.rm = TRUE)
N_val_M <- sum(hazard_prob_merge$val_total, na.rm = TRUE) / 1e6

wilson <- function(k, n, z = 1.96) {
  if (n <= 0 || k < 0) return(list(lo = NA_real_, hi = NA_real_))
  p      <- k / n
  denom  <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  list(lo = max(0, centre - half), hi = min(1, centre + half))
}

panel_a_cols <- c("wind", "surge", "flood", "wind_surge", "wind_flood",
                  "flood_surge", "wind_flood_surge", "any_hazard")

panel_a_ci <- rbindlist(lapply(panel_a_cols, function(col) {
  exposed <- hazard_prob_merge[get(col) > 0]
  k_cells <- nrow(exposed)
  k_homes <- sum(exposed$n_buildings, na.rm = TRUE)
  k_pop   <- sum(exposed$pop_night, na.rm = TRUE)
  k_val_M <- sum(exposed$val_total, na.rm = TRUE) / 1e6

  ci_cells <- wilson(k_cells,         N_cells)
  ci_homes <- wilson(round(k_homes),  round(N_homes))
  ci_pop   <- wilson(round(k_pop),    round(N_pop))
  ci_val   <- wilson(round(k_val_M),  round(N_val_M))

  data.table(
    hazard_type = col,
    homes_pct = sprintf("%.2f", 100 * k_homes / N_homes),
    homes_lo  = sprintf("%.2f", 100 * ci_homes$lo),
    homes_hi  = sprintf("%.2f", 100 * ci_homes$hi),
    cells_pct = sprintf("%.2f", 100 * k_cells / N_cells),
    cells_lo  = sprintf("%.2f", 100 * ci_cells$lo),
    cells_hi  = sprintf("%.2f", 100 * ci_cells$hi),
    pop_pct   = sprintf("%.2f", 100 * k_pop   / N_pop),
    pop_lo    = sprintf("%.2f", 100 * ci_pop$lo),
    pop_hi    = sprintf("%.2f", 100 * ci_pop$hi),
    val_pct   = sprintf("%.2f", 100 * k_val_M / N_val_M),
    val_lo    = sprintf("%.2f", 100 * ci_val$lo),
    val_hi    = sprintf("%.2f", 100 * ci_val$hi)
  )
}))

fwrite(panel_a_ci, file.path(table_dir, "RES1_extensive_table_panelA_ci.csv"))

# ---------------------------------------------------------------------------
# 8. Table 1 Panels B and C: per-home probability quantiles and exposure
# ---------------------------------------------------------------------------
wquantile <- function(x, w, probs) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

hazard_prob_merge[, any_loss := Reduce(`+`, .SD), .SDcols = prob_cols]
stopifnot(max(hazard_prob_merge$any_loss, na.rm = TRUE) <= 1 + 1e-9)

panelBC_categories <- c(any_loss = "Any loss", wind = "Wind", surge = "Surge",
                        flood = "Flood", wind_surge = "Wind + Surge",
                        wind_flood = "Wind + Flood", flood_surge = "Flood + Surge",
                        wind_flood_surge = "Wind + Flood + Surge")

panelB <- vector("list", length(panelBC_categories)); names(panelB) <- names(panelBC_categories)
panelC <- vector("list", length(panelBC_categories)); names(panelC) <- names(panelBC_categories)

for (col in names(panelBC_categories)) {
  exposed <- hazard_prob_merge[get(col) > 0]
  q <- wquantile(exposed[[col]], exposed$n_buildings, c(0.50, 0.95, 0.99))

  panelB[[col]] <- data.table(
    hazard_category      = panelBC_categories[[col]],
    n_properties_exposed = sum(exposed$n_buildings, na.rm = TRUE),
    n_cells_exposed      = nrow(exposed),
    p50_pct = 100 * q[1], p95_pct = 100 * q[2], p99_pct = 100 * q[3])

  at_q <- lapply(q, function(qq) exposed[get(col) >= qq, .(
    nprop = sum(n_buildings, na.rm = TRUE),
    pop   = sum(pop_night,   na.rm = TRUE),
    val   = sum(val_total,   na.rm = TRUE))])

  panelC[[col]] <- data.table(
    hazard_category      = panelBC_categories[[col]],
    total_nprop_exposed  = sum(exposed$n_buildings, na.rm = TRUE),
    total_pop_exposed    = sum(exposed$pop_night,   na.rm = TRUE),
    total_val_exposed_B  = sum(exposed$val_total,   na.rm = TRUE) / 1e9,
    nprop_p50 = at_q[[1]]$nprop, pop_p50_M = at_q[[1]]$pop / 1e6, val_p50_B = at_q[[1]]$val / 1e9,
    nprop_p95 = at_q[[2]]$nprop, pop_p95_M = at_q[[2]]$pop / 1e6, val_p95_B = at_q[[2]]$val / 1e9,
    nprop_p99 = at_q[[3]]$nprop, pop_p99_M = at_q[[3]]$pop / 1e6, val_p99_B = at_q[[3]]$val / 1e9)
}
panelB <- rbindlist(panelB)
panelC <- rbindlist(panelC)

fwrite(panelB, file.path(table_dir, "RES1_extensive_table_panelB_prob_quantiles.csv"))
fwrite(panelC, file.path(table_dir, "RES1_extensive_table_panelC_exposure_at_quantiles.csv"))

stopifnot(panelC[, all(pop_p50_M >= pop_p95_M & pop_p95_M >= pop_p99_M)])
stopifnot(panelC[, all(val_p50_B >= val_p95_B & val_p95_B >= val_p99_B)])
stopifnot(panelC[, all(pop_p50_M * 1e6 <= total_pop_exposed * (1 + 1e-9))])

# ---------------------------------------------------------------------------
# 9. Panel B and C rows in main.tex format
# ---------------------------------------------------------------------------
fmt_prob <- function(x) ifelse(x < 1, sprintf("%.2f", x), sprintf("%.1f", x))
fmt_pop  <- function(m) ifelse(m < 0.001, "$<$0.001", sprintf("%.3f", m))
fmt_val  <- function(b) ifelse(b < 0.095, sprintf("%.2f", b), sprintf("%.1f", b))

row_order <- c("Any loss", "Wind", "Surge", "Flood", "Wind + Surge",
               "Wind + Flood", "Flood + Surge", "Wind + Flood + Surge")
padB <- function(s) formatC(s, width = 21, flag = "-")

linesB <- sapply(row_order, function(h) {
  r <- panelB[hazard_category == h]
  sprintf("%s & %5s & %5s & %5s \\\\", padB(h),
          fmt_prob(r$p50_pct), fmt_prob(r$p95_pct), fmt_prob(r$p99_pct))
})
linesC <- sapply(row_order, function(h) {
  r <- panelC[hazard_category == h]
  sprintf("%s & %s & %5s & %s & %5s & %s & %5s \\\\", padB(h),
          fmt_pop(r$pop_p50_M), fmt_val(r$val_p50_B),
          fmt_pop(r$pop_p95_M), fmt_val(r$val_p95_B),
          fmt_pop(r$pop_p99_M), fmt_val(r$val_p99_B))
})
linesB <- append(linesB, "\\addlinespace", after = 4)
linesC <- append(linesC, "\\addlinespace", after = 4)

latex_out <- c("% Panel B rows (per-home probability quantiles; paste between \\midrule and \\bottomrule):",
               linesB, "",
               "% Panel C rows (paste between \\midrule and \\bottomrule):",
               linesC)
writeLines(latex_out, file.path(table_dir, "RES1_extensive_panelBC_latex.txt"))
