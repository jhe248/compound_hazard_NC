# 07_SI_figures_hazard.R
# Supplementary hazard figures (RES1 only):
#   A1  damage probability at 250 m and block resolution
#   A2  hazard intensity at 250 m, by percentile
#   A3  block-level loss ratios, by percentile
#   A4  block-level loss totals, by percentile
#   A5  any-loss percentile maps decomposed into wind / flood / surge
#   A8  residential growth in eastern NC vs the compound-risk coast
#   plus the 6-panel block map of loss probability by hazard combination
#
# In : input_data/shapefiles/nc_blk_shp/, input_data/shapefiles/nc_county_shp/
#      input_data/shapefiles/nc_tract_shp.gpkg (cached; rebuilt from blocks if absent)
#      input_data/new_construction/nc_tract_pop_change_2010_2020.csv
#      input_data/new_construction/nc_tract_new_construction_acs.csv
#      output_data/damage_hurricanes.qs
#      output_data/block_loss_probabilities.qs
# Out: output_files/figures/SI_figures/SI_FigA1a, A1b, A2a-c, A3a-d, A4a-d, A5a-g
#      output_files/figures/SI_figures/SI_FigA8_new_construction_overlay.png
#      output_files/figures/SI_figures/probability_exposure_map.png

library(qs2)
library(data.table)
library(sf)
library(ggplot2)
library(patchwork)
library(scales)
library(stringr)
library(dplyr)
library(bit64)

FONT <- "Arial"

hazardcolors <- c(
  rep("#d0ebf2", 2), "#b8dceb", "#9ccde5", "#a6cfe1",
  rep("#ffe14a", 3), rep("#fdc086", 3), rep("#fa9d5f", 3), rep("#f768a1", 2),
  rep("#f28fbf", 2), rep("#f1a2d2", 2), rep("#c994c7", 2),
  rep("#b3a4d6", 2), rep("#b3a4d6", 6)
)
losscolors <- c("#9ccde5", "#64b0c8", "#ffe14a", "#fdc086", "#fa9d5f", "#f768a1", "#b3a4d6")
probcolors <- c("#b8dceb",
                rep("#ffe14a", 2),
                rep("#fdc086", 3),
                rep("#f58a55", 4),
                rep("#f768a1", 5),
                rep("#c994c7", 6),
                rep("#5d3f7e", 7))

# ---------------------------------------------------------------------------
# 1. Shapefiles
# ---------------------------------------------------------------------------
blk_raw <- st_read(file.path(input_dir, "shapefiles", "nc_blk_shp"),    quiet = TRUE)
cty_raw <- st_read(file.path(input_dir, "shapefiles", "nc_county_shp"), quiet = TRUE)

c0 <- cty_raw[cty_raw$STUSPS == "NC", ]
s0 <- blk_raw
if (st_crs(s0)$epsg != 4326) s0 <- st_transform(s0, 4326)
if (st_crs(c0)$epsg != 4326) c0 <- st_transform(c0, 4326)

bbox_poly <- st_as_sfc(st_bbox(c(xmin = -81, xmax = -75.4, ymin = 33.8, ymax = 36.6),
                               crs = st_crs(4326)))
s0 <- s0 |> st_crop(bbox_poly)

map_theme <- function(strip_size = 10, key_h = 0.8, key_w = 0.3) {
  theme_minimal(base_size = 10) +
    theme(strip.text = element_text(size = strip_size, face = "bold"),
          panel.grid = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_blank(),
          plot.background  = element_rect(fill = "white", color = NA),
          legend.position = "right", legend.direction = "vertical",
          legend.key.height = unit(key_h, "cm"), legend.key.width = unit(key_w, "cm"),
          legend.title = element_text(size = 9), legend.text = element_text(size = 8))
}

si_coord <- coord_sf(xlim = c(-80.75, -75.4), ylim = c(33.8, 36.6), expand = FALSE, datum = NA)

# ---------------------------------------------------------------------------
# 2. RES1 peril losses
# ---------------------------------------------------------------------------
d0 <- qs_read(file.path(output_dir, "damage_hurricanes.qs"))
d0 <- d0[grepl("^RES1", occtype)]

d0[, `:=`(
  wind_loss  = wind_damage_structure + wind_damage_contents,
  flood_loss = inundation_terrain_damage_structure + inundation_terrain_damage_contents,
  surge_loss = surge_inundation_terrain_damage_structure + surge_inundation_terrain_damage_contents
)]
d0[is.na(wind_loss),  wind_loss  := 0]
d0[is.na(flood_loss), flood_loss := 0]
d0[is.na(surge_loss), surge_loss := 0]

fix_geoid <- function(dt) {
  dt[, GEOID20 := str_pad(as.character(format(GEOID20, scientific = FALSE)), 15, pad = "0")]
}

# ---------------------------------------------------------------------------
# 3. A5: any-loss percentile maps with peril decomposition
# ---------------------------------------------------------------------------
d0_block_anyloss <- d0[, .(
  wind_loss  = sum(wind_loss,  na.rm = TRUE),
  flood_loss = sum(flood_loss, na.rm = TRUE),
  surge_loss = sum(surge_loss, na.rm = TRUE),
  val        = sum(val_struct + val_cont, na.rm = TRUE)
), by = .(storm_id, GEOID20)]

d0_block_anyloss[, any_loss := pmax(wind_loss, flood_loss, surge_loss, na.rm = TRUE)]

d0_block_pos <- d0_block_anyloss[any_loss > 0]
d0_block_pos[, rank_pct := rank(any_loss, ties.method = "first") / .N, by = GEOID20]

get_at_pct <- function(data, q) {
  data[, .SD[which.min(abs(rank_pct - q))], by = GEOID20][
    , .(GEOID20,
        any_loss, wind_loss, flood_loss, surge_loss, val,
        wind_share  = wind_loss  / any_loss,
        flood_share = flood_loss / any_loss,
        surge_share = surge_loss / any_loss)
  ]
}

d_p50 <- get_at_pct(d0_block_pos, 0.50)
d_p95 <- get_at_pct(d0_block_pos, 0.95)
d_p99 <- get_at_pct(d0_block_pos, 0.99)
fix_geoid(d_p50); fix_geoid(d_p95); fix_geoid(d_p99)

s0_p50 <- left_join(s0, as.data.frame(d_p50), by = "GEOID20")
s0_p95 <- left_join(s0, as.data.frame(d_p95), by = "GEOID20")
s0_p99 <- left_join(s0, as.data.frame(d_p99), by = "GEOID20")

make_any_loss_facet <- function(s50, s95, s99, var, label) {
  long <- rbind(
    as.data.frame(st_drop_geometry(s50))[, c("GEOID20", var)] |> transform(pct = "p50 (Median)"),
    as.data.frame(st_drop_geometry(s95))[, c("GEOID20", var)] |> transform(pct = "p95"),
    as.data.frame(st_drop_geometry(s99))[, c("GEOID20", var)] |> transform(pct = "p99")
  )
  names(long)[2] <- "value"
  long$pct <- factor(long$pct, levels = c("p50 (Median)", "p95", "p99"))

  s0_joined     <- left_join(s0, long, by = "GEOID20")
  s0_joined_pos <- s0_joined[!is.na(s0_joined$value) & s0_joined$value > 0, ]

  ggplot() +
    geom_sf(data = s0_joined_pos, aes(fill = value), linewidth = 0, alpha = 0.85) +
    geom_sf(data = c0, fill = NA, color = "grey20", linewidth = 0.2) +
    scale_fill_gradientn(colours = losscolors, na.value = "transparent",
                         labels = scales::label_dollar(scale = 1e-3, suffix = "K"),
                         name = label) +
    facet_wrap(~pct, nrow = 1) +
    si_coord +
    map_theme(key_h = 1, key_w = 0.35)
}

ggsave(file.path(si_dir, "SI_FigA5a_any_loss_pct_block_RES1.png"),        make_any_loss_facet(s0_p50, s0_p95, s0_p99, "any_loss",    "Any Loss ($)"),   width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA5b_wind_at_anyloss_pct_RES1.png"),       make_any_loss_facet(s0_p50, s0_p95, s0_p99, "wind_loss",   "Wind Loss ($)"),  width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA5c_flood_at_anyloss_pct_RES1.png"),      make_any_loss_facet(s0_p50, s0_p95, s0_p99, "flood_loss",  "Flood Loss ($)"), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA5d_surge_at_anyloss_pct_RES1.png"),      make_any_loss_facet(s0_p50, s0_p95, s0_p99, "surge_loss",  "Surge Loss ($)"), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA5e_wind_share_at_anyloss_pct_RES1.png"), make_any_loss_facet(s0_p50, s0_p95, s0_p99, "wind_share",  "Wind Share"),     width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA5f_flood_share_at_anyloss_pct_RES1.png"),make_any_loss_facet(s0_p50, s0_p95, s0_p99, "flood_share", "Flood Share"),    width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA5g_surge_share_at_anyloss_pct_RES1.png"),make_any_loss_facet(s0_p50, s0_p95, s0_p99, "surge_share", "Surge Share"),    width = 18, height = 5, dpi = 600, bg = "white")

# ---------------------------------------------------------------------------
# 4. A1a: damage probability at 250 m
#    Collapse to one row per (storm, cell) first, so each storm counts once.
# ---------------------------------------------------------------------------
d_cell_storm <- d0[, .(
  wind_loss  = sum(wind_loss,  na.rm = TRUE),
  flood_loss = sum(flood_loss, na.rm = TRUE),
  surge_loss = sum(surge_loss, na.rm = TRUE)
), by = .(x250m, y250m, storm_id)]
n_storms_total <- uniqueN(d_cell_storm$storm_id)
d_cell <- d_cell_storm[, .(
  prob_wind  = sum(wind_loss  > 0, na.rm = TRUE) / n_storms_total,
  prob_flood = sum(flood_loss > 0, na.rm = TRUE) / n_storms_total,
  prob_surge = sum(surge_loss > 0, na.rm = TRUE) / n_storms_total
), by = .(x250m, y250m)]

d_cell_long <- melt(
  d_cell, id.vars = c("x250m", "y250m"),
  measure.vars  = c("prob_wind", "prob_flood", "prob_surge"),
  variable.name = "hazard", value.name = "probability"
)[probability > 0]
d_cell_long[, hazard := factor(hazard,
                               levels = c("prob_wind", "prob_flood", "prob_surge"),
                               labels = c("Wind", "Flood", "Surge"))]

d_cell_long_sf <- st_as_sf(d_cell_long, coords = c("x250m", "y250m"), crs = 4326)

fig_a1a <- ggplot() +
  geom_sf(data = d_cell_long_sf,
          aes(fill = probability, color = probability),
          shape = 21, size = 0.25, stroke = 0, alpha = 1) +
  geom_sf(data = c0, fill = NA, color = "grey20", linewidth = 0.2) +
  scale_fill_gradientn(colours = losscolors, na.value = "transparent",
                       labels = scales::percent_format(accuracy = 1), name = "P(damage)") +
  scale_color_gradientn(colours = losscolors, na.value = "transparent",
                        labels = scales::percent_format(accuracy = 1), name = "P(damage)") +
  facet_wrap(~hazard, nrow = 1) +
  si_coord +
  map_theme(strip_size = 11, key_h = 1, key_w = 0.35)

ggsave(file.path(si_dir, "SI_FigA1a_damage_probability_250m_RES1.png"),
       fig_a1a, width = 18, height = 5, dpi = 600, bg = "white")

# ---------------------------------------------------------------------------
# 5. A1b: damage probability at block level
# ---------------------------------------------------------------------------
d_block_a1 <- d0[, .(
  wind_loss_sum  = sum(wind_loss,  na.rm = TRUE),
  flood_loss_sum = sum(flood_loss, na.rm = TRUE),
  surge_loss_sum = sum(surge_loss, na.rm = TRUE)
), by = .(storm_id, GEOID20)]
n_storms_total <- uniqueN(d_block_a1$storm_id)
d_block_prob <- d_block_a1[, .(
  prob_wind  = sum(wind_loss_sum  > 0, na.rm = TRUE) / n_storms_total,
  prob_flood = sum(flood_loss_sum > 0, na.rm = TRUE) / n_storms_total,
  prob_surge = sum(surge_loss_sum > 0, na.rm = TRUE) / n_storms_total
), by = GEOID20]
fix_geoid(d_block_prob)

d_block_prob_long <- melt(
  d_block_prob, id.vars = "GEOID20",
  measure.vars  = c("prob_wind", "prob_flood", "prob_surge"),
  variable.name = "hazard", value.name = "probability"
)[probability > 0]
d_block_prob_long[, hazard := factor(hazard,
                                     levels = c("prob_wind", "prob_flood", "prob_surge"),
                                     labels = c("Wind", "Flood", "Surge"))]

s0_prob_long <- left_join(s0, as.data.frame(d_block_prob_long), by = "GEOID20")
s0_prob_long <- s0_prob_long[!is.na(s0_prob_long$probability) & s0_prob_long$probability > 0, ]

fig_a1b <- ggplot() +
  geom_sf(data = s0_prob_long, aes(fill = probability), linewidth = 0, alpha = 0.85) +
  geom_sf(data = c0, fill = NA, color = "grey20", linewidth = 0.2) +
  scale_fill_gradientn(colours = losscolors, na.value = "transparent",
                       labels = scales::percent_format(accuracy = 1), name = "P(damage)") +
  facet_wrap(~hazard, nrow = 1) +
  si_coord +
  map_theme(strip_size = 11)

ggsave(file.path(si_dir, "SI_FigA1b_damage_probability_block_RES1.png"),
       fig_a1b, width = 18, height = 5, dpi = 600, bg = "white")

# ---------------------------------------------------------------------------
# 6. A2: hazard intensity at 250 m, by percentile
# ---------------------------------------------------------------------------
d_intensity <- d0[, .(
  wind_median  = median(max_wind_terrain,                na.rm = TRUE),
  wind_p95     = quantile(max_wind_terrain,        0.95, na.rm = TRUE),
  wind_p99     = quantile(max_wind_terrain,        0.99, na.rm = TRUE),
  flood_median = median(max_inundation_terrain,          na.rm = TRUE),
  flood_p95    = quantile(max_inundation_terrain,  0.95, na.rm = TRUE),
  flood_p99    = quantile(max_inundation_terrain,  0.99, na.rm = TRUE),
  surge_median = median(max_surge_inundation_terrain,    na.rm = TRUE),
  surge_p95    = quantile(max_surge_inundation_terrain, 0.95, na.rm = TRUE),
  surge_p99    = quantile(max_surge_inundation_terrain, 0.99, na.rm = TRUE)
), by = .(x250m, y250m)]

make_intensity_facet <- function(wide_dt, vars, legend_name) {
  long <- melt(wide_dt, id.vars = c("x250m", "y250m"),
               measure.vars = vars, variable.name = "pct", value.name = "value")
  long[, pct := factor(pct, levels = vars, labels = c("p50 (Median)", "p95", "p99"))]
  long_sf <- st_as_sf(long, coords = c("x250m", "y250m"), crs = 4326)
  ggplot() +
    geom_sf(data = long_sf, aes(fill = value, color = value), shape = 21, size = 0.012) +
    geom_sf(data = c0, fill = NA, color = "grey20", linewidth = 0.2) +
    scale_fill_gradientn(colors = hazardcolors, na.value = "transparent", name = legend_name) +
    scale_color_gradientn(colors = hazardcolors, na.value = "transparent", name = legend_name) +
    facet_wrap(~pct, nrow = 1) +
    si_coord +
    map_theme(key_w = 0.4) +
    theme(legend.margin = margin(t = -15, b = 0),
          legend.title = element_text(size = 8), legend.text = element_text(size = 10))
}

ggsave(file.path(si_dir, "SI_FigA2a_wind_intensity_250m.png"),  make_intensity_facet(d_intensity, c("wind_median",  "wind_p95",  "wind_p99"),  "Wind Speed"),  width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA2b_flood_intensity_250m.png"), make_intensity_facet(d_intensity, c("flood_median", "flood_p95", "flood_p99"), "Flood Depth"), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA2c_surge_intensity_250m.png"), make_intensity_facet(d_intensity, c("surge_median", "surge_p95", "surge_p99"), "Surge Depth"), width = 18, height = 5, dpi = 600, bg = "white")

# ---------------------------------------------------------------------------
# 7. A3 / A4: block-level loss ratios and loss totals, by percentile
# ---------------------------------------------------------------------------
d0_block <- d0[, .(
  wind_loss  = sum(wind_loss,  na.rm = TRUE),
  flood_loss = sum(flood_loss, na.rm = TRUE),
  surge_loss = sum(surge_loss, na.rm = TRUE),
  val        = sum(val_struct + val_cont, na.rm = TRUE)
), by = .(storm_id, GEOID20)]
d0_block[, `:=`(
  wind_loss_ratio  = wind_loss  / val,
  flood_loss_ratio = flood_loss / val,
  surge_loss_ratio = surge_loss / val,
  total_loss = pmax(wind_loss, flood_loss, surge_loss, na.rm = TRUE)
)]
d0_block[, total_loss_ratio := total_loss / val]

d_block_stats <- d0_block[, .(
  wind_median  = median(wind_loss_ratio[wind_loss   > 0], na.rm = TRUE),
  wind_p95     = quantile(wind_loss_ratio[wind_loss  > 0], 0.95, na.rm = TRUE),
  wind_p99     = quantile(wind_loss_ratio[wind_loss  > 0], 0.99, na.rm = TRUE),
  flood_median = median(flood_loss_ratio[flood_loss  > 0], na.rm = TRUE),
  flood_p95    = quantile(flood_loss_ratio[flood_loss > 0], 0.95, na.rm = TRUE),
  flood_p99    = quantile(flood_loss_ratio[flood_loss > 0], 0.99, na.rm = TRUE),
  surge_median = median(surge_loss_ratio[surge_loss  > 0], na.rm = TRUE),
  surge_p95    = quantile(surge_loss_ratio[surge_loss > 0], 0.95, na.rm = TRUE),
  surge_p99    = quantile(surge_loss_ratio[surge_loss > 0], 0.99, na.rm = TRUE),
  total_median = median(total_loss_ratio,   na.rm = TRUE),
  total_p95    = quantile(total_loss_ratio, 0.95, na.rm = TRUE),
  total_p99    = quantile(total_loss_ratio, 0.99, na.rm = TRUE)
), by = GEOID20]
fix_geoid(d_block_stats)

d_block_loss <- d0_block[, .(
  wind_median  = median(wind_loss[wind_loss   > 0], na.rm = TRUE),
  wind_p95     = quantile(wind_loss[wind_loss  > 0], 0.95, na.rm = TRUE),
  wind_p99     = quantile(wind_loss[wind_loss  > 0], 0.99, na.rm = TRUE),
  flood_median = median(flood_loss[flood_loss  > 0], na.rm = TRUE),
  flood_p95    = quantile(flood_loss[flood_loss > 0], 0.95, na.rm = TRUE),
  flood_p99    = quantile(flood_loss[flood_loss > 0], 0.99, na.rm = TRUE),
  surge_median = median(surge_loss[surge_loss  > 0], na.rm = TRUE),
  surge_p95    = quantile(surge_loss[surge_loss > 0], 0.95, na.rm = TRUE),
  surge_p99    = quantile(surge_loss[surge_loss > 0], 0.99, na.rm = TRUE),
  total_median = median(total_loss,   na.rm = TRUE),
  total_p95    = quantile(total_loss, 0.95, na.rm = TRUE),
  total_p99    = quantile(total_loss, 0.99, na.rm = TRUE)
), by = GEOID20]
fix_geoid(d_block_loss)

make_block_facet <- function(wide_dt, vars, legend_name, colors = losscolors) {
  long <- melt(wide_dt, id.vars = "GEOID20",
               measure.vars = vars, variable.name = "pct", value.name = "value")
  long[, pct := factor(pct, levels = vars, labels = c("p50 (Median)", "p95", "p99"))]
  s0_joined     <- left_join(s0, as.data.frame(long), by = "GEOID20")
  s0_joined_pos <- s0_joined[!is.na(s0_joined$value) & s0_joined$value > 0, ]
  ggplot() +
    geom_sf(data = s0_joined_pos, aes(fill = value), linewidth = 0, alpha = 0.85) +
    geom_sf(data = c0, fill = NA, color = "grey20", linewidth = 0.2) +
    scale_fill_gradientn(colors = colors, na.value = "transparent", name = legend_name) +
    facet_wrap(~pct, nrow = 1) +
    si_coord +
    map_theme()
}

ggsave(file.path(si_dir, "SI_FigA3a_wind_loss_ratio_block_RES1.png"),  make_block_facet(d_block_stats, c("wind_median",  "wind_p95",  "wind_p99"),  "Loss Ratio"), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA3b_flood_loss_ratio_block_RES1.png"), make_block_facet(d_block_stats, c("flood_median", "flood_p95", "flood_p99"), "Loss Ratio"), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA3c_surge_loss_ratio_block_RES1.png"), make_block_facet(d_block_stats, c("surge_median", "surge_p95", "surge_p99"), "Loss Ratio"), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA3d_total_loss_ratio_block_RES1.png"), make_block_facet(d_block_stats, c("total_median", "total_p95", "total_p99"), "Loss Ratio"), width = 18, height = 5, dpi = 600, bg = "white")

ggsave(file.path(si_dir, "SI_FigA4a_wind_loss_total_block_RES1.png"),  make_block_facet(d_block_loss, c("wind_median",  "wind_p95",  "wind_p99"),  "Loss ($)", colors = hazardcolors), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA4b_flood_loss_total_block_RES1.png"), make_block_facet(d_block_loss, c("flood_median", "flood_p95", "flood_p99"), "Loss ($)", colors = hazardcolors), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA4c_surge_loss_total_block_RES1.png"), make_block_facet(d_block_loss, c("surge_median", "surge_p95", "surge_p99"), "Loss ($)", colors = hazardcolors), width = 18, height = 5, dpi = 600, bg = "white")
ggsave(file.path(si_dir, "SI_FigA4d_total_loss_total_block_RES1.png"), make_block_facet(d_block_loss, c("total_median", "total_p95", "total_p99"), "Loss ($)", colors = hazardcolors), width = 18, height = 5, dpi = 600, bg = "white")

rm(d0, d0_block, d0_block_anyloss, d0_block_pos, d_cell_storm); gc()

# ---------------------------------------------------------------------------
# 8. Block probability of loss, by hazard combination (companion to Figure 2)
# ---------------------------------------------------------------------------
p0 <- qs_read(file.path(output_dir, "block_loss_probabilities.qs"))

p1     <- merge(blk_raw[, "GEOID20"], p0, by = "GEOID20")
nc_cty <- sort(unique(substr(p0$GEOID20, 1, 5)))
c_prob <- cty_raw[cty_raw$GEOID %in% nc_cty, ]

prob_map <- function(var, lbl) {
  ggplot() +
    geom_sf(data = p1, aes(fill = .data[[var]]), color = NA) +
    geom_sf(data = c_prob, fill = NA) +
    coord_sf(datum = NA) +
    scale_fill_gradientn(colors = probcolors, limits = c(0, 1)) +
    theme_bw() + xlim(-80.5, -75.45) +
    annotate(geom = "text", x = -76.5, y = 34, label = lbl, size = 3) +
    theme(panel.border = element_blank(), legend.position = "none")
}

map_pr <- (prob_map("pr_any_loss",        "Any loss") +
           prob_map("pr_wind_loss_only",  "Wind")) /
          (prob_map("pr_surge_loss_only", "Surge") +
           prob_map("pr_flood_loss_only", "Flood")) /
          (prob_map("pr_wind_surge_loss", "Wind + Surge") +
           prob_map("pr_wind_flood_loss", "Wind + Flood"))
map_pr <- map_pr + plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(), legend.key.height = unit(6, "pt"))

ggsave(map_pr, file = file.path(si_dir, "probability_exposure_map.png"),
       width = 10, height = 12, dpi = 300, bg = "white")

rm(p0, p1, map_pr); gc()

# ---------------------------------------------------------------------------
# 9. A8: residential growth in eastern NC, by census tract
#    Panel A is population change 2010-2020; panel B is the share of homes built
#    2010 or later (ACS B25034), the tract-resolved stand-in for building
#    permits, which the Census publishes only down to place level.
# ---------------------------------------------------------------------------
nc_dir <- file.path(input_dir, "new_construction")

pop <- fread(file.path(nc_dir, "nc_tract_pop_change_2010_2020.csv"), encoding = "UTF-8")
setnames(pop, names(pop), sub("^﻿", "", names(pop)))
pop[, GEOID := paste0("37", sprintf("%09d", tract))]
stopifnot(all(nchar(pop$GEOID) == 11))

acs <- fread(file.path(nc_dir, "nc_tract_new_construction_acs.csv"),
             colClasses = c(GEOID = "character"))
stopifnot(all(nchar(acs$GEOID) == 11))

tract_gpkg <- file.path(input_dir, "shapefiles", "nc_tract_shp.gpkg")
if (file.exists(tract_gpkg)) {
  tr <- st_read(tract_gpkg, quiet = TRUE)
} else {
  tr <- blk_raw
  tr$GEOID_tract <- substr(as.character(tr$GEOID20), 1, 11)
  tr <- tr |> group_by(GEOID_tract) |> summarise(.groups = "drop")
  st_write(tr, tract_gpkg, delete_dsn = TRUE, quiet = TRUE)
}
if (st_crs(tr)$epsg != 4326) tr <- st_transform(tr, 4326)

tr <- merge(tr, pop[, .(GEOID, per_change)],         by.x = "GEOID_tract", by.y = "GEOID", all.x = TRUE)
tr <- merge(tr, acs[, .(GEOID, pct_built_2010plus)], by.x = "GEOID_tract", by.y = "GEOID", all.x = TRUE)

a8_theme <- theme_minimal(base_size = 10, base_family = FONT) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        plot.title   = element_text(size = 10, face = "bold", hjust = 0.5, family = FONT),
        legend.position = "right", legend.direction = "vertical",
        legend.key.height = unit(0.8, "cm"), legend.key.width = unit(0.3, "cm"),
        legend.title = element_text(size = 9, face = "plain", family = FONT),
        legend.text  = element_text(size = 8, face = "plain", family = FONT),
        text         = element_text(face = "plain", family = FONT))

make_a8_panel <- function(fill_var, title, scale_layer) {
  ggplot() +
    geom_sf(data = tr, aes(fill = .data[[fill_var]]), color = NA) +
    scale_layer +
    geom_sf(data = c0, fill = NA, color = "grey20", linewidth = 0.2) +
    si_coord +
    ggtitle(title) + a8_theme
}

scale_pop <- scale_fill_gradient2(
  low = "#f58a55", mid = "white", high = "#5d3f7e", midpoint = 0,
  limits = c(-50, 50), oob = scales::squish, na.value = "grey90",
  labels = label_number(suffix = "%"), name = "% change")
scale_built <- scale_fill_gradient(
  low = "white", high = "#5d3f7e", na.value = "grey90",
  limits = c(0, 40), oob = scales::squish,
  labels = label_number(suffix = "%"), name = "% of homes")

fig_a8 <- (make_a8_panel("per_change",         "Population change, 2010-2020", scale_pop) +
           make_a8_panel("pct_built_2010plus", "Homes built 2010 or later",    scale_built)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 11, face = "bold", family = FONT))

ggsave(file.path(si_dir, "SI_FigA8_new_construction_overlay.png"),
       fig_a8, width = 13.5, height = 4.6, dpi = 600, bg = "white")
