# 06_main_figures_insurance.R
# Main-text Figure 6: insurance over the coastal 1-mile and 1-to-2-mile zones.
# Top row plots % uptake (wind, wind+flood, uninsured); bottom row plots
# % premium of value (wind, wind+flood) and the uninsured loss ratio at
# expected and P95 loss.
#
# In : output_data/insurance_building_merged.qs
#      output_data/damage_hurricanes_bldg_inssample.qs
#      input_data/insurance/zones_1mile.shp, zones_1to2miles.shp (+ sidecars)
# Out: output_files/figures/insurance_panels.png            (Figure 6)
#      output_files/tables/insurance_zone_stats.csv

library(qs2)
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(sf)
library(tigris)
library(bit64)

FONT    <- "Arial"
ins_dir <- file.path(input_dir, "insurance")

mycolors <- c("#b8dceb", "#7ab8d3", "#3d8fa8",
              "#ffe14a", "#fdc086", "#fa9d5f", "#f58a55",
              "#f768a1", "#e64992",
              "#c994c7", "#b3a4d6", "#8a6fb0", "#5d3f7e")
mycolorRamp <- colorRampPalette(mycolors)

# ---------------------------------------------------------------------------
# 1. Priced sample with the two policy decisions
# ---------------------------------------------------------------------------
ins <- as.data.table(qs_read(file.path(output_dir, "insurance_building_merged.qs")))

ins[, `:=`(
  wind_only  = fifelse(Wind_insurance_decision == 1 & Flood_insurance_decision == 0, 1, 0),
  wind_flood = fifelse(Wind_insurance_decision == 1 & Flood_insurance_decision == 1, 1, 0),
  uninsured  = fifelse(Wind_insurance_decision == 0 & Flood_insurance_decision == 0, 1, 0)
)]

# ---------------------------------------------------------------------------
# 2. Coastal zones and the buildings-to-zones spatial join
# ---------------------------------------------------------------------------
z1mile <- st_read(file.path(ins_dir, "zones_1mile.shp"),     quiet = TRUE)
z1to2  <- st_read(file.path(ins_dir, "zones_1to2miles.shp"), quiet = TRUE)
z1mile$zone_type <- "1mile"
z1to2$zone_type  <- "1to2miles"
z1mile <- st_transform(z1mile, crs = 4326)
z1to2  <- st_transform(z1to2,  crs = 4326)
zones <- rbind(z1mile[, c("ID_Order", "zone_type", "geometry")],
               z1to2[,  c("ID_Order", "zone_type", "geometry")])

ins_sf     <- st_as_sf(ins, coords = c("x", "y"), crs = 4326)
ins_joined <- st_join(ins_sf, zones)
ins_dt     <- as.data.table(ins_joined)
ins_dt[, geometry := NULL]

# ---------------------------------------------------------------------------
# 3. Per-building conditional P95 loss from the 604-storm ensemble
# ---------------------------------------------------------------------------
bs      <- as.data.table(qs_read(file.path(output_dir, "damage_hurricanes_bldg_inssample.qs")))
p95_bld <- bs[total_loss > 0, .(p95_loss = quantile(as.numeric(total_loss), 0.95)), by = bid]
ins_dt  <- merge(ins_dt, p95_bld, by = "bid", all.x = TRUE)
ins_dt[is.na(p95_loss), p95_loss := 0]
rm(bs, p95_bld); gc()

# ---------------------------------------------------------------------------
# 4. Zone aggregates: uptake, premium share, uninsured loss ratios
# ---------------------------------------------------------------------------
tract_stats <- ins_dt[!is.na(ID_Order), .(
  wind_uptake             = mean(Wind_insurance_decision, na.rm = TRUE) * 100,
  wind_flood_uptake       = mean(wind_flood, na.rm = TRUE) * 100,
  uninsured               = mean(uninsured,  na.rm = TRUE) * 100,
  wind_premium_frac       = 100 * sum(wind_only  * Wind_premium,                   na.rm = TRUE) /
    sum(wind_only  * (val_struct + val_cont),    na.rm = TRUE),
  wind_flood_premium_frac = 100 * sum(wind_flood * (Wind_premium + Flood_premium), na.rm = TRUE) /
    sum(wind_flood * (val_struct + val_cont),    na.rm = TRUE),
  loss_uninsured_frac     = 100 * sum(uninsured * (Expected_Wind_loss + Expected_Flood_loss + Expected_Surge_loss), na.rm = TRUE) /
    sum(uninsured * (val_struct + val_cont),    na.rm = TRUE),
  p95_uninsured_frac      = 100 * sum(uninsured * p95_loss, na.rm = TRUE) /
    sum(uninsured * (val_struct + val_cont),    na.rm = TRUE)
), by = .(ID_Order, zone_type)]

zones_wind       <- merge(zones, tract_stats[, .(ID_Order, zone_type, wind_uptake, wind_premium_frac)],
                          by = c("ID_Order", "zone_type"), all.x = TRUE)
zones_wind_flood <- merge(zones, tract_stats[, .(ID_Order, zone_type, wind_flood_uptake, wind_flood_premium_frac)],
                          by = c("ID_Order", "zone_type"), all.x = TRUE)
zones_uninsured  <- merge(zones, tract_stats[, .(ID_Order, zone_type, uninsured, loss_uninsured_frac, p95_uninsured_frac)],
                          by = c("ID_Order", "zone_type"), all.x = TRUE)

zones_wind       <- zones_wind[!is.na(zones_wind$wind_uptake), ]
zones_wind_flood <- zones_wind_flood[!is.na(zones_wind_flood$wind_flood_uptake), ]
zones_uninsured  <- zones_uninsured[!is.na(zones_uninsured$uninsured), ]

fwrite(tract_stats, file.path(table_dir, "insurance_zone_stats.csv"))

# ---------------------------------------------------------------------------
# 5. Panels
# ---------------------------------------------------------------------------
nc_counties <- st_transform(counties(state = "NC", cb = TRUE, year = 2020), crs = 4326)

make_map <- function(zone_sf, mycol, scale_max, scale_breaks) {
  zone_sf$myval <- zone_sf[[mycol]]
  ggplot() +
    geom_sf(data = nc_counties, fill = "white", color = "grey55", linewidth = 0.2) +
    geom_sf(data = zone_sf, aes(fill = myval), color = NA) +
    coord_sf(datum = NA) +
    scale_fill_gradientn(colors = mycolorRamp(50),
                         limits = c(0, scale_max), breaks = scale_breaks) +
    theme_bw(base_size = 13) +
    xlim(-78.5, -75.5) + ylim(33.9, 36.55) +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(color = NA),
          plot.margin  = unit(c(0, 0, 0, 0), "pt"),
          text         = element_text(family = FONT))
}

panel_theme <- theme(legend.direction = "horizontal",
                     legend.title     = element_blank(),
                     legend.position  = "bottom",
                     legend.key.width = unit(0.65, "in"),
                     legend.key.height= unit(0.22, "in"),
                     legend.text      = element_text(size = 12, family = FONT),
                     plot.title       = element_text(size = 12, hjust = 0.4,
                                                     family = FONT, face = "plain"))

uptake_breaks <- c(0, 10, 20, 30, 40, 75)

p_wind_uptake       <- make_map(zones_wind,       "wind_uptake",       scale_max = 90, scale_breaks = uptake_breaks) + ggtitle("% Uptake: Wind")
p_wind_flood_uptake <- make_map(zones_wind_flood, "wind_flood_uptake", scale_max = 90, scale_breaks = uptake_breaks) + ggtitle("% Uptake: Flood")
p_uninsured         <- make_map(zones_uninsured,  "uninsured",         scale_max = 90, scale_breaks = uptake_breaks) + ggtitle("% Uninsured")

row_uptake <- (plot_spacer() + p_wind_uptake + p_wind_flood_uptake + p_uninsured + plot_spacer()) +
  plot_layout(ncol = 5, widths = c(0.5, 1, 1, 1, 0.5), guides = "collect") & panel_theme

premium_breaks <- seq(0, 5, 1)

f_scale_max <- ceiling(max(tract_stats$loss_uninsured_frac, na.rm = TRUE))
f_breaks    <- pretty(c(0, f_scale_max), n = 5)
g_scale_max <- ceiling(max(tract_stats$p95_uninsured_frac, na.rm = TRUE) / 10) * 10
g_breaks    <- pretty(c(0, g_scale_max), n = 5)

lossbar <- guides(fill = guide_colourbar(barwidth = unit(1.5, "in"), barheight = unit(0.22, "in")))

p_wind_premium_pc       <- make_map(zones_wind[zones_wind$wind_premium_frac > 0, ],                   "wind_premium_frac",       scale_max = 7,           scale_breaks = premium_breaks) + ggtitle("% Premiums: Wind")
p_wind_flood_premium_pc <- make_map(zones_wind_flood[zones_wind_flood$wind_flood_premium_frac > 0, ], "wind_flood_premium_frac", scale_max = 7,           scale_breaks = premium_breaks) + ggtitle("% Premiums: Wind + Flood")
p_uninsured_exp         <- make_map(zones_uninsured[zones_uninsured$loss_uninsured_frac > 0, ],       "loss_uninsured_frac",     scale_max = f_scale_max, scale_breaks = f_breaks)       + ggtitle("Uninsured expected loss (% of value)") + lossbar
p_uninsured_p95         <- make_map(zones_uninsured[zones_uninsured$p95_uninsured_frac > 0, ],        "p95_uninsured_frac",      scale_max = g_scale_max, scale_breaks = g_breaks)       + ggtitle("Uninsured P95 loss (% of value)")      + lossbar

row_prem  <- (p_wind_premium_pc + p_wind_flood_premium_pc) +
  plot_layout(ncol = 2, guides = "collect") & panel_theme

row_lossr <- (p_uninsured_exp + p_uninsured_p95) +
  plot_layout(ncol = 2) &
  panel_theme &
  theme(plot.title = element_text(size = 11, hjust = 0.3, family = FONT, face = "plain"))

row_premium <- wrap_elements(row_prem) | wrap_elements(row_lossr)

fig6 <- wrap_elements(row_uptake) / wrap_elements(row_premium)

ggsave(file.path(figure_dir, "insurance_panels.png"),
       fig6, width = 12, height = 9, dpi = 300, bg = "white")

rm(ins, ins_sf, ins_joined, ins_dt); gc()
