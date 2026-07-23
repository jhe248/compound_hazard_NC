# 08_SI_figures_insurance.R
# Supplementary insurance figures and tables on the priced sample:
#   worst-case uninsured loss under a wind-only vs a wind+flood policy market;
#   affordability of the offered full-coverage premium;
#   the price distribution behind the two coverage choices;
#   the stratification of homes, damage and uninsured damage by hazard type;
#   a 6-panel P95 / P99 event map over the coastal zones.
#
# In : output_data/insurance_building_merged.csv
#      output_data/damage_hurricanes_bldg_inssample.qs
#      input_data/insurance/zones_1mile.shp, zones_1to2miles.shp (+ sidecars)
# Out: output_files/tables/worstcase_uninsured_crosstab.csv
#      output_files/tables/insurance_affordability_xtab.csv
#      output_files/tables/stratification_by_haztype.csv
#      output_files/tables/stratification_by_haztype_latex.txt
#      output_files/tables/stratification_afford_tails.csv
#      output_files/tables/stratification_zone_p95_p99.csv
#      output_files/figures/SI_figures/worstcase_uninsured_crosstab_combined.png
#      output_files/figures/SI_figures/insurance_affordability_map.png
#      output_files/figures/SI_figures/insurance_price_density.png
#      output_files/figures/SI_figures/stratification_6panel_p95_p99.png

library(qs2)
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(sf)
library(tigris)
library(bit64)

FONT             <- "Arial"
N_STORMS         <- 604
AFFORD_THRESHOLD <- 0.05

HAZ_CATS <- c("wind_only", "surge_only", "flood_only",
              "wind_surge", "wind_flood", "surge_flood",
              "wind_surge_flood")
HAZ_TYPES  <- c("Wind only", "Wind + Inland flood", "Wind + Surge",
                "Wind + Surge + Inland flood", "Water only (no wind)")
CMPD_TYPES <- c("Wind + Inland flood", "Wind + Surge",
                "Wind + Surge + Inland flood")

# ---------------------------------------------------------------------------
# 1. Priced sample: value, offered premiums, affordability, exposure class
#
# The zone price applies per $ of insured expected loss and the flood policy
# covers flood and surge, so full coverage of a home's expected losses costs
# price_per_loss * (E[wind] + E[flood] + E[surge]); both identities are asserted
# below so a re-priced spreadsheet fails loudly rather than silently.
# ---------------------------------------------------------------------------
ins <- fread(file.path(output_dir, "insurance_building_merged.csv"))
setnames(ins, "Insurance_Price_per_$_loss", "price_per_loss")

stopifnot(ins[Wind_insurance_decision == 1,
              max(abs(Wind_premium - price_per_loss * Winde_insured_demand))] < 1e-6)
stopifnot(ins[Flood_insurance_decision == 1,
              max(abs(Flood_insured_loss -
                        (Expected_Flood_loss + Expected_Surge_loss)))] < 1e-6)

ins[, `:=`(
  val_total      = as.numeric(val_struct) + as.numeric(val_cont),
  e_water        = Expected_Flood_loss + Expected_Surge_loss
)]
ins[, `:=`(
  prem_wind_off  = price_per_loss * Expected_Wind_loss,
  prem_water_off = price_per_loss * e_water
)]
ins[, prem_full_off := prem_wind_off + prem_water_off]
ins[, `:=`(
  cannot_afford_full = fifelse(prem_full_off > AFFORD_THRESHOLD * val_total, 1, 0),
  cannot_afford_wind = fifelse(prem_wind_off > AFFORD_THRESHOLD * val_total, 1, 0),
  compound_exposed   = fifelse(Expected_Wind_loss > 0 & e_water > 0, 1, 0)
)]
ins[, exposure_class := fcase(
  Expected_Wind_loss > 0 & e_water > 0,  "Compound (wind + water)",
  Expected_Wind_loss > 0 & e_water == 0, "Wind only",
  Expected_Wind_loss == 0 & e_water > 0, "Water only",
  default = "No expected loss"
)]

# ---------------------------------------------------------------------------
# 2. Per-storm sample damages: proportional cap and the two uninsured measures
#
# Coverage is nested, not exclusive: a wind+flood household is still
# wind-insured, so wind loss is covered whenever Wind_dec == 1 in BOTH markets.
# Moving from a wind-only market to a wind+flood market only ADDS flood+surge
# coverage, so uninsured loss can never rise; the assertion below locks that in.
#   Wind-only market : uninsured = (1-Wind_dec)*wind + flood + surge
#   Wind+flood market: uninsured = (1-Wind_dec)*wind + (1-Flood_dec)*(flood+surge)
# ---------------------------------------------------------------------------
bldg_storm <- qs_read(file.path(output_dir, "damage_hurricanes_bldg_inssample.qs"))

bldg_storm[, haz_cat := fcase(
  wind_loss >  0 & surge_loss == 0 & flood_loss == 0, "wind_only",
  wind_loss == 0 & surge_loss >  0 & flood_loss == 0, "surge_only",
  wind_loss == 0 & surge_loss == 0 & flood_loss >  0, "flood_only",
  wind_loss >  0 & surge_loss >  0 & flood_loss == 0, "wind_surge",
  wind_loss >  0 & surge_loss == 0 & flood_loss >  0, "wind_flood",
  wind_loss == 0 & surge_loss >  0 & flood_loss >  0, "surge_flood",
  wind_loss >  0 & surge_loss >  0 & flood_loss >  0, "wind_surge_flood"
)]

bldg_storm[, raw_peril_sum := wind_loss + flood_loss + surge_loss]
bldg_storm[, scale_cap := fifelse(raw_peril_sum > 0, total_loss / raw_peril_sum, 0)]
bldg_storm[, `:=`(
  wind_loss_c  = wind_loss  * scale_cap,
  flood_loss_c = flood_loss * scale_cap,
  surge_loss_c = surge_loss * scale_cap
)]

bldg_storm <- merge(
  bldg_storm,
  ins[, .(bid, Wind_dec  = as.integer(Wind_insurance_decision),
          Flood_dec = as.integer(Flood_insurance_decision))],
  by = "bid", all.x = TRUE
)
bldg_storm[is.na(Wind_dec),  Wind_dec  := 0L]
bldg_storm[is.na(Flood_dec), Flood_dec := 0L]

bldg_storm[, uninsured_windonly :=
             (1 - Wind_dec) * as.numeric(wind_loss_c) +
             as.numeric(flood_loss_c) + as.numeric(surge_loss_c)]
bldg_storm[, uninsured_windflood :=
             (1 - Wind_dec)  * as.numeric(wind_loss_c) +
             (1 - Flood_dec) * (as.numeric(flood_loss_c) + as.numeric(surge_loss_c))]

stopifnot(bldg_storm[, all(uninsured_windflood <= uninsured_windonly + 1e-6)])

# ---------------------------------------------------------------------------
# 3. Worst-case uninsured cross-tab
#    Per building, the storm leaving the largest uninsured loss under each
#    market, attributed to the hazard category of that storm.
# ---------------------------------------------------------------------------
wo_idx   <- bldg_storm[, .I[which.max(uninsured_windonly)],  by = bid]$V1
worst_wo <- bldg_storm[wo_idx, .(bid,
                                 wc_uninsured_wo = as.numeric(uninsured_windonly),
                                 haz_cat_wo = haz_cat)]

wf_idx   <- bldg_storm[, .I[which.max(uninsured_windflood)], by = bid]$V1
worst_wf <- bldg_storm[wf_idx, .(bid,
                                 wc_uninsured_wf = as.numeric(uninsured_windflood),
                                 haz_cat_wf = haz_cat)]

bldg_wc <- merge(ins[, .(bid, val_total)], worst_wo, by = "bid", all.x = TRUE)
bldg_wc <- merge(bldg_wc, worst_wf, by = "bid", all.x = TRUE)
bldg_wc[is.na(wc_uninsured_wo), `:=`(wc_uninsured_wo = 0, haz_cat_wo = NA_character_)]
bldg_wc[is.na(wc_uninsured_wf), `:=`(wc_uninsured_wf = 0, haz_cat_wf = NA_character_)]

total_val  <- sum(as.numeric(ins$val_total))
indiv_cats <- c("wind_only", "surge_only", "flood_only")
cmpd_cats  <- c("wind_surge", "wind_flood", "surge_flood", "wind_surge_flood")

sum_wo <- bldg_wc[!is.na(haz_cat_wo),
                  .(windonly_dollars  = sum(wc_uninsured_wo)), by = .(category = haz_cat_wo)]
sum_wf <- bldg_wc[!is.na(haz_cat_wf),
                  .(windflood_dollars = sum(wc_uninsured_wf)), by = .(category = haz_cat_wf)]

ct_cat <- data.table(category = HAZ_CATS)
ct_cat <- merge(ct_cat, sum_wo, by = "category", all.x = TRUE)
ct_cat <- merge(ct_cat, sum_wf, by = "category", all.x = TRUE)
ct_cat[is.na(windonly_dollars),  windonly_dollars  := 0]
ct_cat[is.na(windflood_dollars), windflood_dollars := 0]
ct_cat[, category := as.character(category)]
ct_cat <- ct_cat[match(HAZ_CATS, category)]
setcolorder(ct_cat, c("category", "windonly_dollars", "windflood_dollars"))

make_agg <- function(label, sub_dt) {
  data.table(category = label,
             windonly_dollars  = sum(as.numeric(sub_dt$windonly_dollars)),
             windflood_dollars = sum(as.numeric(sub_dt$windflood_dollars)))
}

crosstab_full <- rbind(ct_cat, rbind(
  make_agg("SUBTOTAL_individual", ct_cat[category %in% indiv_cats]),
  make_agg("SUBTOTAL_compound",   ct_cat[category %in% cmpd_cats]),
  make_agg("TOTAL",               ct_cat),
  use.names = TRUE
), use.names = TRUE)

crosstab_full[, windonly_pct_of_value  := windonly_dollars  / total_val]
crosstab_full[, windflood_pct_of_value := windflood_dollars / total_val]
tot_wo <- crosstab_full[category == "TOTAL", windonly_dollars]
tot_wf <- crosstab_full[category == "TOTAL", windflood_dollars]
crosstab_full[, windonly_share_of_total  := windonly_dollars  / tot_wo]
crosstab_full[, windflood_share_of_total := windflood_dollars / tot_wf]
setcolorder(crosstab_full, c(
  "category",
  "windonly_dollars",  "windonly_pct_of_value",  "windonly_share_of_total",
  "windflood_dollars", "windflood_pct_of_value", "windflood_share_of_total"
))

fwrite(crosstab_full, file.path(table_dir, "worstcase_uninsured_crosstab.csv"))

# ---------------------------------------------------------------------------
# 4. Worst-case bar charts
# ---------------------------------------------------------------------------
cat_labels <- c(
  wind_only        = "Wind only",
  surge_only       = "Surge only",
  flood_only       = "Flood only",
  wind_surge       = "Wind + Surge",
  wind_flood       = "Wind + Flood",
  surge_flood      = "Surge + Flood",
  wind_surge_flood = "All three"
)
cat_palette <- c(
  "Wind only"     = "#3AB0C8",
  "Surge only"    = "#D9C64A",
  "Flood only"    = "#C84E9A",
  "Wind + Surge"  = "#7CCD7C",
  "Wind + Flood"  = "#AB82FF",
  "Surge + Flood" = "#FFA07A",
  "All three"     = "#555555"
)

bar_theme <- theme_bw(base_size = 11, base_family = FONT) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.border       = element_rect(colour = "grey20", linewidth = 0.3),
        plot.title   = element_text(size = 11, face = "plain", hjust = 0.5, family = FONT),
        axis.title   = element_text(size = 10, face = "plain", family = FONT),
        axis.text    = element_text(size = 10, face = "plain", family = FONT),
        legend.title = element_blank(),
        legend.text  = element_text(size = 9, face = "plain", family = FONT),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        text = element_text(face = "plain", family = FONT))

scen_lab <- function(v) fifelse(v == "windonly_pct_of_value",
                                "Wind-only\npolicy", "Wind + Flood\npolicy")

chart_dt <- melt(
  crosstab_full[!category %in% c("SUBTOTAL_individual", "SUBTOTAL_compound", "TOTAL")],
  id.vars       = "category",
  measure.vars  = c("windonly_pct_of_value", "windflood_pct_of_value"),
  variable.name = "scenario", value.name = "pct_of_value"
)
chart_dt[, scenario := scen_lab(scenario)]
chart_dt[, category := factor(cat_labels[as.character(category)],
                              levels = unname(cat_labels))]

p_xtab_cat <- ggplot(chart_dt, aes(x = scenario, y = pct_of_value, fill = category)) +
  geom_col(width = 0.55) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = cat_palette, drop = FALSE) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL, y = "Uninsured loss (% of total value)", title = "By hazard category") +
  bar_theme

chart_ic_dt <- melt(
  crosstab_full[category %in% c("SUBTOTAL_individual", "SUBTOTAL_compound")],
  id.vars       = "category",
  measure.vars  = c("windonly_pct_of_value", "windflood_pct_of_value"),
  variable.name = "scenario", value.name = "pct_of_value"
)
chart_ic_dt[, scenario := scen_lab(scenario)]
chart_ic_dt[, hazard_type := fifelse(category == "SUBTOTAL_individual",
                                     "Individual hazard", "Compound hazard")]
chart_ic_dt[, hazard_type := factor(hazard_type,
                                    levels = c("Individual hazard", "Compound hazard"))]

p_xtab_ic <- ggplot(chart_ic_dt, aes(x = scenario, y = pct_of_value, fill = hazard_type)) +
  geom_col(position = "stack", width = 0.55) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Individual hazard" = "#64b0c8",
                               "Compound hazard"   = "#f768a1")) +
  labs(x = NULL, y = NULL, title = "Individual vs compound") +
  bar_theme

p_xtab_combined <- (p_xtab_cat | p_xtab_ic) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 11, face = "bold", family = FONT))

ggsave(file.path(si_dir, "worstcase_uninsured_crosstab_combined.png"),
       p_xtab_combined, width = 9, height = 5.5, dpi = 300, bg = "white")

# ---------------------------------------------------------------------------
# 5. Affordability cross-tab and map
# ---------------------------------------------------------------------------
afford_xtab <- ins[, .(
  n_homes                = .N,
  n_cannot_afford_full   = sum(cannot_afford_full),
  n_cannot_afford_wind   = sum(cannot_afford_wind),
  mean_prem_full_off     = mean(prem_full_off),
  mean_prem_pct_of_value = 100 * mean(prem_full_off / val_total),
  total_expected_loss    = sum(Expected_Wind_loss + e_water)
), by = exposure_class]
afford_xtab[, `:=`(
  pct_of_class_unaffordable = 100 * n_cannot_afford_full / n_homes,
  pct_of_all_unaffordable   = 100 * n_cannot_afford_full / sum(n_cannot_afford_full)
)]
setorder(afford_xtab, -n_cannot_afford_full)
fwrite(afford_xtab, file.path(table_dir, "insurance_affordability_xtab.csv"))

nc_counties <- st_transform(counties(state = "NC", cb = TRUE, year = 2020), crs = 4326)

ins_afford_ok  <- ins[cannot_afford_full == 0]
ins_afford_bad <- ins[cannot_afford_full == 1]

lab_ok  <- "Affordable"
lab_bad <- sprintf("Premium > %.0f%% of home value (n = %s)",
                   AFFORD_THRESHOLD * 100,
                   format(nrow(ins_afford_bad), big.mark = ","))

p_afford <- ggplot() +
  geom_sf(data = nc_counties, fill = "white", color = "grey55", linewidth = 0.2) +
  geom_point(data = ins_afford_ok,  aes(x = x, y = y, color = lab_ok),
             size = 0.05, alpha = 0.35) +
  geom_point(data = ins_afford_bad, aes(x = x, y = y, color = lab_bad),
             size = 0.3, alpha = 0.8) +
  scale_color_manual(values = setNames(c("grey82", "#e64992"), c(lab_ok, lab_bad)),
                     breaks = c(lab_bad, lab_ok), name = NULL,
                     guide = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  coord_sf(xlim = c(-78.65, -75.40), ylim = c(33.80, 36.60), expand = FALSE, datum = NA) +
  theme_bw(base_size = 13) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(color = NA),
        legend.position = "bottom",
        legend.text = element_text(size = 12, family = FONT),
        text = element_text(family = FONT))

ggsave(file.path(si_dir, "insurance_affordability_map.png"),
       p_afford, width = 7, height = 6, dpi = 300, bg = "white")

# ---------------------------------------------------------------------------
# 6. Offered-premium density for the two coverage products
# ---------------------------------------------------------------------------
price_dt <- rbind(
  data.table(coverage  = "Wind only",
             price_pct = ins[prem_wind_off > 0, 100 * prem_wind_off / val_total]),
  data.table(coverage  = "Wind + Flood",
             price_pct = ins[prem_full_off > 0, 100 * prem_full_off / val_total])
)
price_dt[, coverage := factor(coverage, levels = c("Wind only", "Wind + Flood"))]

wind_up  <- 100 * mean(ins$Wind_insurance_decision)
flood_up <- 100 * mean(ins$Flood_insurance_decision)

p_price_density <- ggplot(price_dt, aes(x = price_pct, fill = coverage, colour = coverage)) +
  geom_density(alpha = 0.35, linewidth = 0.6, adjust = 1.2) +
  scale_fill_manual(values   = c("Wind only" = "#3d8fa8", "Wind + Flood" = "#f768a1")) +
  scale_colour_manual(values = c("Wind only" = "#3d8fa8", "Wind + Flood" = "#f768a1")) +
  annotate("text", x = Inf, y = Inf, label = "Wind only",
           colour = "#3d8fa8", hjust = 1.1, vjust = 1.8, size = 4, fontface = "plain", family = FONT) +
  annotate("text", x = Inf, y = Inf, label = "Wind + Flood",
           colour = "#f768a1", hjust = 1.1, vjust = 3.3, size = 4, fontface = "plain", family = FONT) +
  labs(x = "Offered premium (% of home value)", y = "Density",
       subtitle = sprintf("Uptake: wind %.1f%%, combined wind + flood %.1f%%", wind_up, flood_up)) +
  coord_cartesian(xlim = c(0, quantile(price_dt$price_pct, 0.99))) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        plot.subtitle   = element_text(size = 9),
        text            = element_text(family = FONT))

ggsave(file.path(si_dir, "insurance_price_density.png"),
       p_price_density, width = 6, height = 4, dpi = 300, bg = "white")

# ---------------------------------------------------------------------------
# 7. Stratification by the perils that ever damage a home
#    P95 / P99 are conditional on damaging events, the same convention as the
#    tricolore percentiles in 04.
# ---------------------------------------------------------------------------
bldg_ever <- bldg_storm[, .(
  ever_wind  = any(wind_loss  > 0),
  ever_flood = any(flood_loss > 0),
  ever_surge = any(surge_loss > 0)
), by = bid]
bldg_ever[, haz_type := fcase(
  ever_wind  & !ever_flood & !ever_surge, "Wind only",
  ever_wind  &  ever_flood & !ever_surge, "Wind + Inland flood",
  ever_wind  & !ever_flood &  ever_surge, "Wind + Surge",
  ever_wind  &  ever_flood &  ever_surge, "Wind + Surge + Inland flood",
  default = "Water only (no wind)"
)]

bldg_cat <- merge(ins, bldg_ever[, .(bid, haz_type)], by = "bid", all.x = TRUE)
bldg_cat[is.na(haz_type), haz_type := "No modeled damage"]

bldg_stats <- bldg_storm[, .(
  exp_loss       = sum(as.numeric(total_loss))          / N_STORMS,
  exp_uninsured  = sum(as.numeric(uninsured_windflood)) / N_STORMS,
  p95_loss_cond  = quantile(as.numeric(total_loss), 0.95),
  p99_loss_cond  = quantile(as.numeric(total_loss), 0.99),
  n_damaging     = .N
), by = bid]

bldg_cat <- merge(bldg_cat, bldg_stats, by = "bid", all.x = TRUE)
for (col in c("exp_loss", "exp_uninsured", "p95_loss_cond", "p99_loss_cond"))
  bldg_cat[is.na(get(col)), (col) := 0]

cat_storm <- merge(bldg_storm[, .(bid, storm_id, total_loss)],
                   bldg_cat[, .(bid, haz_type)], by = "bid")
cat_event <- cat_storm[, .(event_loss = sum(as.numeric(total_loss))),
                       by = .(haz_type, storm_id)]

cat_tails <- cat_event[event_loss > 0, .(
  p95_event_loss = quantile(event_loss, 0.95),
  p99_event_loss = quantile(event_loss, 0.99),
  n_damaging_events = .N
), by = haz_type]

tail_row <- function(label, sub) {
  ev <- sub[, .(event_loss = sum(as.numeric(total_loss))), by = storm_id]
  data.table(haz_type = label,
             p95_event_loss = quantile(ev$event_loss, 0.95),
             p99_event_loss = quantile(ev$event_loss, 0.99),
             n_damaging_events = nrow(ev))
}
cat_tails <- rbind(
  cat_tails,
  tail_row("All compound (wind + water)", cat_storm[haz_type %in% CMPD_TYPES]),
  tail_row("Total", cat_storm)
)

strat_core <- function(sub, label) {
  data.table(
    haz_type          = label,
    n_homes           = nrow(sub),
    population        = sum(as.numeric(sub$poppm), na.rm = TRUE),
    asset_value       = sum(sub$val_total),
    exp_damage        = sum(sub$exp_loss),
    exp_uninsured     = sum(sub$exp_uninsured),
    exp_uninsured_kevin = sum((1 - sub$Wind_insurance_decision)  * sub$Expected_Wind_loss +
                                (1 - sub$Flood_insurance_decision) * sub$e_water),
    n_uninsured_homes = sub[, sum(Wind_insurance_decision == 0 & Flood_insurance_decision == 0)],
    n_cannot_afford   = sum(sub$cannot_afford_full)
  )
}

row_types <- intersect(c(HAZ_TYPES, "No modeled damage"), unique(bldg_cat$haz_type))
strat <- rbindlist(c(
  lapply(row_types, function(h) strat_core(bldg_cat[haz_type == h], h)),
  list(strat_core(bldg_cat[haz_type %in% CMPD_TYPES], "All compound (wind + water)"),
       strat_core(bldg_cat, "Total"))
))
strat <- merge(strat, cat_tails, by = "haz_type", all.x = TRUE, sort = FALSE)
strat[, `:=`(
  pct_homes           = 100 * n_homes     / sum(bldg_cat[, .N]),
  pct_value           = 100 * asset_value / sum(bldg_cat$val_total),
  uninsured_share_pct = 100 * exp_uninsured / exp_damage
)]
setcolorder(strat, c("haz_type", "n_homes", "pct_homes", "population",
                     "asset_value", "pct_value", "exp_damage",
                     "p95_event_loss", "p99_event_loss",
                     "exp_uninsured", "uninsured_share_pct",
                     "exp_uninsured_kevin", "n_uninsured_homes",
                     "n_cannot_afford", "n_damaging_events"))
fwrite(strat, file.path(table_dir, "stratification_by_haztype.csv"))

fmt_latex <- function(r) {
  sprintf("%s & %s & %s & %.2f & %.1f & %.1f & %.1f & %.1f & %.1f \\\\",
          r$haz_type,
          format(r$n_homes,           big.mark = ","),
          format(round(r$population), big.mark = ","),
          r$asset_value    / 1e9,
          r$exp_damage     / 1e6,
          r$p95_event_loss / 1e6,
          r$p99_event_loss / 1e6,
          r$exp_uninsured  / 1e6,
          r$uninsured_share_pct)
}
latex_lines <- c(
  "% stratification_by_haztype -> tab:stratification_appendix",
  "% cols: category & homes & population & value ($B) & E[damage] ($M/event) &",
  "%       P95 ($M) & P99 ($M) & E[uninsured] ($M/event) & uninsured share (%)",
  vapply(seq_len(nrow(strat)), function(i) fmt_latex(strat[i]), character(1))
)
writeLines(latex_lines, file.path(table_dir, "stratification_by_haztype_latex.txt"))

afford_tails <- bldg_cat[haz_type != "No modeled damage", .(
  n_homes          = .N,
  mean_exp_LR_pct  = 100 * mean(exp_loss      / val_total),
  mean_p95_LR_pct  = 100 * mean(p95_loss_cond / val_total),
  mean_p99_LR_pct  = 100 * mean(p99_loss_cond / val_total),
  med_p95_loss     = median(p95_loss_cond),
  med_p99_loss     = median(p99_loss_cond)
), by = .(haz_type, cannot_afford_full)]
setorder(afford_tails, haz_type, cannot_afford_full)
fwrite(afford_tails, file.path(table_dir, "stratification_afford_tails.csv"))

# ---------------------------------------------------------------------------
# 8. Six-panel coastal-zone map: P95 / P99 event x exposure and damage
#    Per zone, the storm nearest that percentile of the zone's aggregate
#    per-event loss, as in 04.
# ---------------------------------------------------------------------------
ins_dir <- file.path(input_dir, "insurance")
z1mile <- st_read(file.path(ins_dir, "zones_1mile.shp"),     quiet = TRUE)
z1to2  <- st_read(file.path(ins_dir, "zones_1to2miles.shp"), quiet = TRUE)
z1mile$zone_type <- "1mile"
z1to2$zone_type  <- "1to2miles"
z1mile <- st_transform(z1mile, crs = 4326)
z1to2  <- st_transform(z1to2,  crs = 4326)
zones <- rbind(z1mile[, c("ID_Order", "zone_type", "geometry")],
               z1to2[,  c("ID_Order", "zone_type", "geometry")])

ins_sf   <- st_as_sf(ins[, .(bid, x, y)], coords = c("x", "y"), crs = 4326)
bid_zone <- as.data.table(st_join(ins_sf, zones))[!is.na(ID_Order),
                                                 .(bid, ID_Order, zone_type)]

zs <- merge(bldg_storm[, .(bid, storm_id, total_loss)], bid_zone, by = "bid",
            allow.cartesian = TRUE)
zone_event <- zs[, .(event_loss = sum(as.numeric(total_loss))),
                 by = .(ID_Order, zone_type, storm_id)]

zq <- zone_event[, .(q95 = quantile(event_loss, 0.95),
                     q99 = quantile(event_loss, 0.99)),
                 by = .(ID_Order, zone_type)]
zone_event <- merge(zone_event, zq, by = c("ID_Order", "zone_type"))
pick95 <- zone_event[, .SD[which.min((event_loss - q95)^2)], by = .(ID_Order, zone_type)]
pick99 <- zone_event[, .SD[which.min((event_loss - q99)^2)], by = .(ID_Order, zone_type)]

zone_exposure <- function(pick, tag) {
  sel <- merge(zs, pick[, .(ID_Order, zone_type, storm_id)],
               by = c("ID_Order", "zone_type", "storm_id"))
  sel <- merge(sel, ins[, .(bid, poppm, val_total)], by = "bid")
  out <- sel[total_loss > 0, .(
    pop_exposed  = sum(as.numeric(poppm), na.rm = TRUE),
    val_exposed  = sum(val_total),
    event_damage = sum(as.numeric(total_loss))
  ), by = .(ID_Order, zone_type)]
  out[, pctile := tag]
  out
}
zone_stats <- rbind(zone_exposure(pick95, "p95"), zone_exposure(pick99, "p99"))

zone_area <- data.table(ID_Order = zones$ID_Order, zone_type = zones$zone_type,
                        area_km2 = as.numeric(st_area(zones)) / 1e6)
zone_stats <- merge(zone_stats, zone_area, by = c("ID_Order", "zone_type"), all.x = TRUE)

fwrite(zone_stats, file.path(table_dir, "stratification_zone_p95_p99.csv"))

mycolors <- c("#b8dceb", "#7ab8d3", "#3d8fa8",
              "#ffe14a", "#fdc086", "#fa9d5f", "#f58a55",
              "#f768a1", "#e64992",
              "#c994c7", "#b3a4d6", "#8a6fb0", "#5d3f7e")
mycolorRamp <- colorRampPalette(mycolors)

RAMP_PROBS <- c(0, 0.32, 0.60, 0.72, 0.80, 0.87, 0.92, 0.955, 0.975,
                0.985, 0.992, 0.997, 1)
wtd_quantile <- function(v, w, probs) {
  o <- order(v)
  cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) v[o][which(cw >= p)[1]], numeric(1))
}
ramp_anchors <- function(v, w, vmax) {
  q <- wtd_quantile(v, w, RAMP_PROBS) / vmax
  q[1] <- 0; q[length(q)] <- 1
  for (i in 2:length(q)) if (q[i] <= q[i - 1]) q[i] <- q[i - 1] + 1e-6
  q / max(q)
}

make_zone_map <- function(stats_dt, mycol, scale_max, ramp_vals, lab_fun,
                          title_txt = NULL, row_tag = NULL) {
  zsf <- merge(zones, stats_dt[, c("ID_Order", "zone_type", mycol), with = FALSE],
               by = c("ID_Order", "zone_type"), all.x = TRUE)
  zsf$myval <- zsf[[mycol]]
  zsf <- zsf[!is.na(zsf$myval), ]
  p <- ggplot() +
    geom_sf(data = nc_counties, fill = "white", color = "grey55", linewidth = 0.2) +
    geom_sf(data = zsf, aes(fill = myval), color = NA) +
    coord_sf(datum = NA) +
    scale_fill_gradientn(colors = mycolorRamp(length(ramp_vals)),
                         values = ramp_vals,
                         limits = c(0, scale_max), labels = lab_fun) +
    theme_bw(base_size = 13) +
    xlim(-78.5, -75.5) + ylim(33.9, 36.55) +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(color = NA),
          plot.margin  = unit(c(0, 0, 0, 0), "pt"),
          text         = element_text(family = FONT))
  if (!is.null(title_txt)) p <- p + ggtitle(title_txt)
  p +
    labs(y = if (is.null(row_tag)) " " else row_tag) +
    theme(axis.title.y = element_text(size = 14, family = FONT,
                                      angle = 90, vjust = 0.5))
}

panel_theme <- theme(legend.direction = "horizontal",
                     legend.title     = element_blank(),
                     legend.position  = "bottom",
                     legend.key.width = unit(0.55, "in"),
                     legend.key.height= unit(0.18, "in"),
                     legend.text      = element_text(size = 11, family = FONT),
                     plot.title       = element_text(size = 14, hjust = 0.5,
                                                     family = FONT, face = "plain"))

s95 <- zone_stats[pctile == "p95"]
s99 <- zone_stats[pctile == "p99"]

lab_count  <- label_number(scale_cut = cut_short_scale())
lab_dollar <- label_dollar(scale_cut = cut_short_scale())

max_pop <- max(zone_stats$pop_exposed)
max_val <- max(zone_stats$val_exposed)
max_dmg <- max(zone_stats$event_damage)

vals_pop <- ramp_anchors(zone_stats$pop_exposed,  zone_stats$area_km2, max_pop)
vals_val <- ramp_anchors(zone_stats$val_exposed,  zone_stats$area_km2, max_val)
vals_dmg <- ramp_anchors(zone_stats$event_damage, zone_stats$area_km2, max_dmg)

col_pop <- (make_zone_map(s95, "pop_exposed", max_pop, vals_pop, lab_count,
                          title_txt = "Population exposed", row_tag = "P95 event") /
              make_zone_map(s99, "pop_exposed", max_pop, vals_pop, lab_count,
                            row_tag = "P99 event")) +
  plot_layout(guides = "collect") & panel_theme
col_val <- (make_zone_map(s95, "val_exposed", max_val, vals_val, lab_dollar,
                          title_txt = "Asset value exposed") /
              make_zone_map(s99, "val_exposed", max_val, vals_val, lab_dollar)) +
  plot_layout(guides = "collect") & panel_theme
col_dmg <- (make_zone_map(s95, "event_damage", max_dmg, vals_dmg, lab_dollar,
                          title_txt = "Event damage") /
              make_zone_map(s99, "event_damage", max_dmg, vals_dmg, lab_dollar)) +
  plot_layout(guides = "collect") & panel_theme

fig_zone <- wrap_elements(col_pop) | wrap_elements(col_val) | wrap_elements(col_dmg)

ggsave(file.path(si_dir, "stratification_6panel_p95_p99.png"),
       fig_zone, width = 12, height = 9.5, dpi = 300, bg = "white")

rm(bldg_storm, cat_storm, cat_event, zs, zone_event, ins_sf); gc()
