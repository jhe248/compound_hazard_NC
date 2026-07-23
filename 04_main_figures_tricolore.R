# 04_main_figures_tricolore.R
# Main-text Figure 5: tricolore (RGB ternary) maps of the peril mix behind the
# storm at the 50th / 95th / 99th percentile of any-loss in each NC census
# block. Map, ternary key and dollar-loss legend are rendered as transparent
# PNGs and composited into one panel per percentile.
#
# In : input_data/shapefiles/nc_county_shp/, input_data/shapefiles/nc_blk_shp/
#      output_data/damage_hurricanes.qs
# Out: output_files/figures/panel_RES1_50th_percentile.png   (Figure 5a)
#      output_files/figures/panel_RES1_95th_percentile.png   (Figure 5b)
#      output_files/figures/panel_RES1_99th_percentile.png   (Figure 5c)
#      output_files/figures/tricolore_parts/                 (map / key / legend PNGs)

library(qs2)
library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(cowplot)
library(bit64)
library(stringr)
library(tricolore)
library(ggtern)
library(extrafont)
library(grid)
library(magick)

FONT      <- "Arial"
parts_dir <- file.path(figure_dir, "tricolore_parts")
dir.create(parts_dir, showWarnings = FALSE, recursive = TRUE)

legend_cols <- c(
  wind_only   = "#00B0B5", flood_only = "#FF97F3", surge_only = "#D9C525",
  wind_surge  = "#7CCD7C", wind_flood = "#AB82FF",
  surge_flood = "#FFA07A", all_three  = "#555555"
)
cat_order  <- c("wind_only", "flood_only", "surge_only",
                "wind_surge", "wind_flood", "surge_flood", "all_three")
cat_labels <- c(wind_only = "Wind only", flood_only = "Flood only", surge_only = "Surge only",
                wind_surge = "Wind + Surge", wind_flood = "Wind + Flood",
                surge_flood = "Surge + Flood", all_three = "All three")

# ---------------------------------------------------------------------------
# 1. RES1 damages, bounded by structure / contents value
# ---------------------------------------------------------------------------
d0 <- qs_read(file.path(output_dir, "damage_hurricanes.qs"))
setDT(d0)
d0 <- d0[grepl("^RES1", occtype)]

c0 <- st_read(file.path(input_dir, "shapefiles", "nc_county_shp"), quiet = TRUE)
b0 <- st_read(file.path(input_dir, "shapefiles", "nc_blk_shp"),    quiet = TRUE)

d0[is.na(max_wind_terrain), max_wind_terrain := 0]
d0[is.na(max_inundation_terrain), max_inundation_terrain := 0]
d0[is.na(max_surge_inundation_terrain), max_surge_inundation_terrain := 0]

d0[, `:=`(
  adj_wind_damage_structure = pmin(wind_damage_structure, val_struct),
  adj_wind_damage_contents  = pmin(wind_damage_contents,  val_cont),
  adj_inundation_terrain_damage_structure = pmin(inundation_terrain_damage_structure, val_struct),
  adj_inundation_terrain_damage_contents  = pmin(inundation_terrain_damage_contents,  val_cont),
  adj_surge_inundation_terrain_damage_structure = pmin(surge_inundation_terrain_damage_structure, val_struct),
  adj_surge_inundation_terrain_damage_contents  = pmin(surge_inundation_terrain_damage_contents,  val_cont)
)]
d0[, any_loss := pmin(adj_wind_damage_structure + adj_inundation_terrain_damage_structure + surge_inundation_terrain_damage_structure, val_struct)
   + pmin(adj_wind_damage_contents + adj_inundation_terrain_damage_contents + surge_inundation_terrain_damage_contents, val_cont)]

d_raw_block <- d0[, .(
  wind_loss  = sum(adj_wind_damage_structure + adj_wind_damage_contents, na.rm = TRUE),
  flood_loss = sum(adj_inundation_terrain_damage_structure + adj_inundation_terrain_damage_contents, na.rm = TRUE),
  surge_loss = sum(adj_surge_inundation_terrain_damage_structure + adj_surge_inundation_terrain_damage_contents, na.rm = TRUE)
), by = .(storm_id, GEOID20)]

d1 <- d0[, .(any_loss = sum(any_loss)), by = .(GEOID20, storm_id)]
rm(d0); gc()

# ---------------------------------------------------------------------------
# 2. Storm nearest each block's p50 / p95 / p99 of any-loss
# ---------------------------------------------------------------------------
t0 <- d1[any_loss > 0, .(
  any_loss50 = quantile(any_loss, 0.50),
  any_loss95 = quantile(any_loss, 0.95),
  any_loss99 = quantile(any_loss, 0.99)
), by = "GEOID20"]

t1 <- merge(t0, d1[any_loss > 0, ], by = "GEOID20")
t1[, `:=`(
  any_loss50_dev = (any_loss - any_loss50)^2,
  any_loss95_dev = (any_loss - any_loss95)^2,
  any_loss99_dev = (any_loss - any_loss99)^2
)]
t2_50 <- t1[, .SD[which.min(any_loss50_dev)], by = "GEOID20"]
t2_95 <- t1[, .SD[which.min(any_loss95_dev)], by = "GEOID20"]
t2_99 <- t1[, .SD[which.min(any_loss99_dev)], by = "GEOID20"]
rm(t1); gc()

# ---------------------------------------------------------------------------
# 3. Spatial setup and per-percentile join
# ---------------------------------------------------------------------------
s0 <- b0
norm_geoid20 <- function(x) stringr::str_pad(as.character(x), 15, pad = "0")
s0$GEOID20          <- norm_geoid20(s0$GEOID20)
d_raw_block$GEOID20 <- norm_geoid20(d_raw_block$GEOID20)
t2_50$GEOID20       <- norm_geoid20(t2_50$GEOID20)
t2_95$GEOID20       <- norm_geoid20(t2_95$GEOID20)
t2_99$GEOID20       <- norm_geoid20(t2_99$GEOID20)

nc_cty <- sort(unique(substr(t2_50$GEOID20, 1, 5)))
c0 <- c0[c0$GEOID %in% nc_cty, ]
if (sf::st_crs(s0)$epsg != 4326) s0 <- sf::st_transform(s0, 4326)
if (sf::st_crs(c0)$epsg != 4326) c0 <- sf::st_transform(c0, 4326)

bbox <- sf::st_bbox(c(xmin = -81, xmax = -75.4, ymin = 33.8, ymax = 36.6),
                    crs = sf::st_crs(4326))
s0_cropped <- s0 |> sf::st_crop(sf::st_as_sfc(bbox))

prepare_data <- function(t2) {
  keys     <- t2[, .(GEOID20, storm_id)]
  d_merged <- merge(keys, d_raw_block, by = c("GEOID20", "storm_id"))
  d_merged[, total_loss := pmax(wind_loss, flood_loss, surge_loss)]
  d_merged <- d_merged[total_loss > 0]
  d_merged[, `:=`(prop_wind  = wind_loss  / total_loss,
                  prop_flood = flood_loss / total_loss,
                  prop_surge = surge_loss / total_loss)]
  s0_cropped |>
    dplyr::left_join(as.data.frame(d_merged), by = "GEOID20") |>
    dplyr::filter(!is.na(prop_wind))
}
data_p50_raw <- prepare_data(t2_50)
data_p95_raw <- prepare_data(t2_95)
data_p99_raw <- prepare_data(t2_99)

# ---------------------------------------------------------------------------
# 4. Tricolore RGB fill and hazard category
# ---------------------------------------------------------------------------
add_tricolore_info <- function(data, threshold = 0.01) {
  data$wind_loss_orig  <- data$prop_wind  * data$total_loss
  data$flood_loss_orig <- data$prop_flood * data$total_loss
  data$surge_loss_orig <- data$prop_surge * data$total_loss
  eps <- 1
  wl <- log(data$wind_loss_orig  + eps); fl <- log(data$flood_loss_orig + eps)
  sl <- log(data$surge_loss_orig + eps); tl <- wl + fl + sl
  tric_input <- data.frame(wind = wl / tl, flood = fl / tl, surge = sl / tl)
  tric <- Tricolore(tric_input, p1 = "wind", p2 = "flood", p3 = "surge",
                    breaks = 3, center = NA, hue = 0.55, chroma = 0.5,
                    lightness = 0.85, contrast = 0.75, spread = 1.5,
                    show_data = TRUE, show_center = TRUE, label_as = "pct")
  data$rgb <- tric$rgb
  data <- data %>% mutate(
    has_wind  = prop_wind  > threshold,
    has_flood = prop_flood > threshold,
    has_surge = prop_surge > threshold,
    hazard_type = case_when(
      has_wind & has_surge & has_flood   ~ "all_three",
      has_wind & has_surge & !has_flood  ~ "wind_surge",
      has_wind & has_flood & !has_surge  ~ "wind_flood",
      !has_wind & has_surge & has_flood  ~ "surge_flood",
      has_wind & !has_flood & !has_surge ~ "wind_only",
      !has_wind & has_flood & !has_surge ~ "flood_only",
      !has_wind & !has_flood & has_surge ~ "surge_only",
      TRUE ~ "none"
    )
  )
  list(data = data, tric = tric)
}
res_p50 <- add_tricolore_info(data_p50_raw)
res_p95 <- add_tricolore_info(data_p95_raw)
res_p99 <- add_tricolore_info(data_p99_raw)
data_p50 <- res_p50$data; tric_p50 <- res_p50$tric
data_p95 <- res_p95$data; tric_p95 <- res_p95$tric
data_p99 <- res_p99$data; tric_p99 <- res_p99$tric
rm(data_p50_raw, data_p95_raw, data_p99_raw, res_p50, res_p95, res_p99)

loss_p50 <- as.data.table(as.data.frame(data_p50))[, .(total_loss_B = sum(total_loss, na.rm = TRUE) * 1e-9), by = hazard_type]
loss_p95 <- as.data.table(as.data.frame(data_p95))[, .(total_loss_B = sum(total_loss, na.rm = TRUE) * 1e-9), by = hazard_type]
loss_p99 <- as.data.table(as.data.frame(data_p99))[, .(total_loss_B = sum(total_loss, na.rm = TRUE) * 1e-9), by = hazard_type]

# ---------------------------------------------------------------------------
# 5. Panel elements
# ---------------------------------------------------------------------------
create_map <- function(data) {
  data_ws <- data[data$hazard_type == "wind_surge", ]
  data_wf <- data[data$hazard_type == "wind_flood", ]
  data_sf <- data[data$hazard_type == "surge_flood", ]
  data_at <- data[data$hazard_type == "all_three", ]
  ggplot() +
    geom_sf(data = data, aes(fill = rgb, geometry = geometry), linewidth = 0, alpha = 0.9) +
    scale_fill_identity() +
    geom_sf(data = c0, fill = NA, color = "grey20", linewidth = 0.3) +
    geom_sf(data = data_ws, fill = NA, color = "#7CCD7C", linewidth = 0.4, alpha = 0.9) +
    geom_sf(data = data_wf, fill = NA, color = "#AB82FF", linewidth = 0.4, alpha = 0.9) +
    geom_sf(data = data_sf, fill = NA, color = "#FFA07A", linewidth = 0.4, alpha = 0.9) +
    geom_sf(data = data_at, fill = NA, color = "#555555", linewidth = 0.6, alpha = 0.95) +
    coord_sf(xlim = c(-80.75, -75.4), ylim = c(33.8, 36.6), expand = FALSE, datum = NA) +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = margin(0, 0, 0, 0))
}

create_loss_legend <- function(loss_dt) {
  n  <- length(cat_order); ys <- seq(n, 1)
  labs <- vapply(cat_order, function(cc) {
    v <- loss_dt[hazard_type == cc, total_loss_B]
    sprintf("%s: $%.2fB", cat_labels[cc], if (length(v)) v[1] else 0)
  }, character(1))
  legend_df <- data.frame(y = ys, label = labs, col = unname(legend_cols[cat_order]),
                          stringsAsFactors = FALSE)
  ggplot(legend_df) +
    annotation_custom(grid::roundrectGrob(r = unit(0.08, "snpc"),
                                          gp = grid::gpar(fill = "grey85", col = NA))) +
    geom_text(aes(x = 0, y = y, label = label, colour = col), hjust = 0,
              size = 6.5, family = FONT) +
    scale_colour_identity() +
    coord_cartesian(xlim = c(-0.3, 6.2), ylim = c(0.2, n + 0.8), clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = margin(6, 6, 6, 6))
}

create_ternary_key <- function(tric_obj) {
  key <- tric_obj$key
  key@layers$Tricolore$aes_params$size   <- 3.5
  key@layers$Tricolore$aes_params$colour <- "lightsteelblue"
  key <- key +
    theme_showarrows() + theme_hidetitles() +
    theme(tern.axis.arrow.sep   = 0.16,
          tern.axis.arrow       = element_line(linewidth = 1.1, colour = "grey15"),
          tern.plot.background  = element_rect(fill = "transparent", colour = NA),
          tern.panel.background = element_rect(fill = "transparent", colour = NA),
          tern.axis.text       = element_text(size = 26, face = "plain", family = FONT),
          tern.axis.arrow.text = element_text(size = 26, face = "plain", family = FONT),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = margin(8, 12, 8, 12)) +
    scale_L_continuous(label = c(0, 33, 67, 100), breaks = c(0, 0.33, 0.67, 1)) +
    scale_T_continuous(label = c(0, 33, 67, 100), breaks = c(0, 0.33, 0.67, 1)) +
    scale_R_continuous(label = c(0, 33, 67, 100), breaks = c(0, 0.33, 0.67, 1))
  key@coordinates$limits$x <- c(-0.26, 1.26)
  key@coordinates$limits$y <- c(-0.28, 1.15)
  key
}

# ---------------------------------------------------------------------------
# 6. Export the parts, then composite one panel per percentile
# ---------------------------------------------------------------------------
export_parts <- function(data, loss_dt, tric_obj, stem) {
  ggsave(file.path(parts_dir, paste0(stem, "_map.png")),
         create_map(data),            width = 11,  height = 7,   dpi = 600, bg = "transparent")
  ggsave(file.path(parts_dir, paste0(stem, "_legend.png")),
         create_loss_legend(loss_dt), width = 3.4, height = 3,   dpi = 600, bg = "transparent")
  ggtern::ggsave(file.path(parts_dir, paste0(stem, "_key.png")),
                 create_ternary_key(tric_obj), width = 9, height = 9.5, dpi = 600, bg = "transparent")
}

assemble_panel <- function(stem, out) {
  f <- function(s) file.path(parts_dir, paste0(stem, s))
  fig <- cowplot::ggdraw() +
    cowplot::draw_image(f("_map.png"),    x = 0.00, y = 0.500, width = 0.70, height = 1.00, hjust = 0, vjust = 0.5) +
    cowplot::draw_image(f("_legend.png"), x = 0.94, y = 0.970, width = 0.27, height = 0.42, hjust = 1, vjust = 1) +
    cowplot::draw_image(f("_key.png"),    x = 1.04, y = 0.335, width = 0.57, height = 1.00, hjust = 1, vjust = 0.5)
  ggplot2::ggsave(file.path(figure_dir, out), fig, width = 16, height = 8, dpi = 600, bg = "transparent")
}

export_parts(data_p50, loss_p50, tric_p50, "panel_RES1_50th")
export_parts(data_p95, loss_p95, tric_p95, "panel_RES1_95th")
export_parts(data_p99, loss_p99, tric_p99, "panel_RES1_99th")

assemble_panel("panel_RES1_50th", "panel_RES1_50th_percentile.png")
assemble_panel("panel_RES1_95th", "panel_RES1_95th_percentile.png")
assemble_panel("panel_RES1_99th", "panel_RES1_99th_percentile.png")
