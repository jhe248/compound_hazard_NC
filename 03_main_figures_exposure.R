# 03_main_figures_exposure.R
# Main-text Figures 2-4 for RES1 single-family homes: exposure by loss
# probability, exposure and loss by hazard intensity, and the bivariate
# wind+flood / wind+surge heatmaps. Also writes the block-level loss
# probabilities used by the supplementary probability map in 07.
#
# In : output_data/damage_hurricanes.qs
# Out: output_data/block_loss_probabilities.qs
#      output_files/tables/RES1_hazard_correlations.csv
#      output_files/figures/probability_exposure_histograms.png       (Figure 2)
#      output_files/figures/hist_exposure_loss_individual_hazards.png (Figure 3)
#      output_files/figures/bivariate_exposure_loss.png               (Figure 4)

library(qs2)
library(data.table)
library(ggplot2)
library(patchwork)
library(cowplot)
library(bit64)
library(pammtools)

FONT <- "Arial"

mycolors <- c("#b8dceb",
              rep("#ffe14a", 2),
              rep("#fdc086", 3),
              rep("#f58a55", 4),
              rep("#f768a1", 5),
              rep("#c994c7", 6),
              rep("#5d3f7e", 7))

# ---------------------------------------------------------------------------
# 1. RES1 damages, bounded by structure / contents value
# ---------------------------------------------------------------------------
d0 <- qs_read(file.path(output_dir, "damage_hurricanes.qs"))
setDT(d0)
d0 <- d0[grepl("^RES1", occtype)]

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

d0[, `:=`(
  wind_loss_only = fifelse(adj_wind_damage_structure > 0 & adj_inundation_terrain_damage_structure == 0 & adj_surge_inundation_terrain_damage_structure == 0,
                           adj_wind_damage_structure, 0)
                   + fifelse(wind_damage_contents > 0 & inundation_terrain_damage_contents == 0 & surge_inundation_terrain_damage_contents == 0,
                             adj_wind_damage_contents, 0),
  surge_loss_only = fifelse(adj_surge_inundation_terrain_damage_structure > 0 & adj_wind_damage_structure == 0 & adj_inundation_terrain_damage_structure == 0,
                            adj_surge_inundation_terrain_damage_structure, 0)
                    + fifelse(adj_surge_inundation_terrain_damage_contents > 0 & adj_wind_damage_contents == 0 & adj_inundation_terrain_damage_contents == 0,
                              adj_surge_inundation_terrain_damage_contents, 0),
  flood_loss_only = fifelse(adj_inundation_terrain_damage_structure > 0 & adj_surge_inundation_terrain_damage_structure == 0 & adj_wind_damage_structure == 0,
                            adj_inundation_terrain_damage_structure, 0)
                    + fifelse(adj_inundation_terrain_damage_contents > 0 & adj_surge_inundation_terrain_damage_contents == 0 & adj_wind_damage_contents == 0,
                              adj_inundation_terrain_damage_contents, 0),
  wind_flood_loss = fifelse(adj_wind_damage_structure > 0 & adj_inundation_terrain_damage_structure > 0 & adj_surge_inundation_terrain_damage_structure == 0,
                            pmax(adj_wind_damage_structure, adj_inundation_terrain_damage_structure), 0)
                    + fifelse(adj_wind_damage_contents > 0 & adj_inundation_terrain_damage_contents > 0 & adj_surge_inundation_terrain_damage_contents == 0,
                              pmax(adj_wind_damage_contents, adj_inundation_terrain_damage_contents), 0),
  wind_surge_loss = fifelse(adj_wind_damage_structure > 0 & adj_surge_inundation_terrain_damage_structure > 0 & adj_inundation_terrain_damage_structure == 0,
                            pmax(adj_wind_damage_structure, adj_surge_inundation_terrain_damage_structure), 0)
                    + fifelse(adj_wind_damage_contents > 0 & adj_surge_inundation_terrain_damage_contents > 0 & adj_inundation_terrain_damage_contents == 0,
                              pmax(adj_wind_damage_contents, adj_surge_inundation_terrain_damage_contents), 0),
  surge_flood_loss = fifelse(adj_surge_inundation_terrain_damage_structure > 0 & adj_inundation_terrain_damage_structure > 0 & adj_wind_damage_structure == 0,
                             pmax(adj_surge_inundation_terrain_damage_structure, adj_inundation_terrain_damage_structure), 0)
                     + fifelse(adj_surge_inundation_terrain_damage_contents > 0 & adj_inundation_terrain_damage_contents > 0 & adj_wind_damage_contents == 0,
                               pmax(adj_surge_inundation_terrain_damage_contents, adj_inundation_terrain_damage_contents), 0),
  wind_surge_flood_loss = fifelse(adj_wind_damage_structure > 0 & adj_inundation_terrain_damage_structure > 0 & adj_surge_inundation_terrain_damage_structure > 0,
                                  pmax(adj_wind_damage_structure, inundation_terrain_damage_structure, adj_surge_inundation_terrain_damage_structure), 0)
                          + fifelse(adj_wind_damage_contents > 0 & adj_inundation_terrain_damage_contents > 0 & adj_surge_inundation_terrain_damage_contents > 0,
                                    pmax(adj_wind_damage_contents, adj_inundation_terrain_damage_contents, adj_surge_inundation_terrain_damage_contents), 0),
  any_loss = pmin(adj_wind_damage_structure + adj_inundation_terrain_damage_structure + surge_inundation_terrain_damage_structure, val_struct)
             + pmin(adj_wind_damage_contents + adj_inundation_terrain_damage_contents + surge_inundation_terrain_damage_contents, val_cont)
)]

n_storms <- length(unique(d0$storm_id)); gc()

# ---------------------------------------------------------------------------
# 2. Hazard co-occurrence correlations over building-storm records where both
#    perils cause positive damage
# ---------------------------------------------------------------------------
d0[, `:=`(
  wind_l  = adj_wind_damage_structure + adj_wind_damage_contents,
  flood_l = adj_inundation_terrain_damage_structure + adj_inundation_terrain_damage_contents,
  surge_l = adj_surge_inundation_terrain_damage_structure + adj_surge_inundation_terrain_damage_contents
)]
cor_pair <- function(dt, li, lj, ii, ij, name) {
  s <- dt[get(li) > 0 & get(lj) > 0]
  data.table(
    pair          = name,
    n_records     = nrow(s),
    rho_intensity = if (nrow(s) > 2) cor(s[[ii]], s[[ij]], method = "spearman") else NA_real_,
    r_loss        = if (nrow(s) > 2) cor(s[[li]], s[[lj]], method = "pearson")  else NA_real_
  )
}
hazard_cor <- rbind(
  cor_pair(d0, "wind_l",  "flood_l", "max_wind_terrain", "max_inundation_terrain",       "Wind-Flood"),
  cor_pair(d0, "wind_l",  "surge_l", "max_wind_terrain", "max_surge_inundation_terrain", "Wind-Surge"),
  cor_pair(d0, "flood_l", "surge_l", "max_inundation_terrain", "max_surge_inundation_terrain", "Flood-Surge")
)
fwrite(hazard_cor, file.path(table_dir, "RES1_hazard_correlations.csv"))

# ---------------------------------------------------------------------------
# 3. Block x storm losses and block-level loss probabilities
# ---------------------------------------------------------------------------
d1 <- d0[, .(
  wind_loss_only        = sum(wind_loss_only),
  surge_loss_only       = sum(surge_loss_only),
  flood_loss_only       = sum(flood_loss_only),
  wind_flood_loss       = sum(wind_flood_loss),
  wind_surge_loss       = sum(wind_surge_loss),
  surge_flood_loss      = sum(surge_flood_loss),
  wind_surge_flood_loss = sum(wind_surge_flood_loss),
  any_loss              = sum(any_loss),
  poppm                 = sum(poppm),
  val                   = sum(val_struct + val_cont),
  max_wind_terrain             = round(mean(max_wind_terrain_raw,             na.rm = TRUE), 0),
  max_surge_inundation_terrain = round(mean(max_surge_inundation_terrain_raw, na.rm = TRUE), 0),
  max_inundation_terrain       = round(mean(max_inundation_terrain_raw,       na.rm = TRUE), 0)
), by = c("GEOID20", "storm_id")]

p0 <- d1[, .(
  pr_wind_loss_only        = sum(wind_loss_only        > 0) / n_storms,
  pr_surge_loss_only       = sum(surge_loss_only       > 0) / n_storms,
  pr_flood_loss_only       = sum(flood_loss_only       > 0) / n_storms,
  pr_wind_flood_loss       = sum(wind_flood_loss       > 0) / n_storms,
  pr_wind_surge_loss       = sum(wind_surge_loss       > 0) / n_storms,
  pr_surge_flood_loss      = sum(surge_flood_loss      > 0) / n_storms,
  pr_wind_surge_flood_loss = sum(wind_surge_flood_loss > 0) / n_storms,
  pr_any_loss              = sum(any_loss              > 0) / n_storms,
  poppm = mean(poppm), val = mean(val)
), by = "GEOID20"]

qs_save(p0, file = file.path(output_dir, "block_loss_probabilities.qs"))

rm(d0); gc()

# ---------------------------------------------------------------------------
# 4. Figure 2: population and asset value by loss probability
# ---------------------------------------------------------------------------
base_theme <- theme_bw(base_size = 11, base_family = FONT) +
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10))

corner_labels <- function(labels, colors, x = 0.198) {
  lapply(seq_along(labels), function(i) {
    annotate("text", x = x, y = Inf, label = labels[i], colour = colors[i],
             hjust = 1, vjust = 1.6 + (i - 1) * 1.5,
             size = 4, family = FONT)
  })
}

ind_labels <- c("Wind", "Flood", "Surge", "Any loss")
ind_colors <- c("seagreen", "orange", "red", "black")
cmp_labels <- c("Wind + Surge", "Surge + Flood", "Wind + Flood", "Wind + Surge + Flood")
cmp_colors <- c("seagreen", "orange", "red", "black")

p_pop_individual <- ggplot() +
  geom_histogram(data = p0[pr_wind_loss_only  > 0, ], aes(x = pr_wind_loss_only,  weight = poppm * 1e-6), bins = 250, alpha = 0.3, fill = "seagreen") +
  geom_histogram(data = p0[pr_surge_loss_only > 0, ], aes(x = pr_surge_loss_only, weight = poppm * 1e-6), bins = 250, alpha = 0.3, fill = "red") +
  geom_histogram(data = p0[pr_flood_loss_only > 0, ], aes(x = pr_flood_loss_only, weight = poppm * 1e-6), bins = 250, alpha = 0.3, fill = "orange") +
  geom_step(stat = "bin", data = p0[pr_any_loss > 0, ], aes(x = pr_any_loss, weight = poppm * 1e-6), bins = 250, color = "black", direction = "mid") +
  corner_labels(ind_labels, ind_colors) +
  base_theme + labs(y = "Population (M)") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  coord_cartesian(xlim = c(0, 0.2))

p_val_individual <- ggplot() +
  geom_histogram(data = p0[pr_wind_loss_only  > 0, ], aes(x = pr_wind_loss_only,  weight = val * 1e-9), bins = 250, alpha = 0.3, fill = "seagreen") +
  geom_histogram(data = p0[pr_surge_loss_only > 0, ], aes(x = pr_surge_loss_only, weight = val * 1e-9), bins = 250, alpha = 0.3, fill = "red") +
  geom_histogram(data = p0[pr_flood_loss_only > 0, ], aes(x = pr_flood_loss_only, weight = val * 1e-9), bins = 250, alpha = 0.3, fill = "orange") +
  geom_step(stat = "bin", data = p0, aes(x = pr_any_loss, weight = val * 1e-9), bins = 250, color = "black", direction = "mid") +
  base_theme + xlab("Probability of non-zero individual loss") + ylab("Asset value ($ Bn)") +
  coord_cartesian(xlim = c(0, 0.2))

p_pop_compound <- ggplot() +
  geom_histogram(data = p0[pr_wind_surge_loss  > 0, ], aes(x = pr_wind_surge_loss,  weight = poppm * 1e-6), bins = 60, alpha = 0.3, fill = "seagreen") +
  geom_histogram(data = p0[pr_wind_flood_loss  > 0, ], aes(x = pr_wind_flood_loss,  weight = poppm * 1e-6), bins = 60, alpha = 0.3, fill = "red") +
  geom_histogram(data = p0[pr_surge_flood_loss > 0, ], aes(x = pr_surge_flood_loss, weight = poppm * 1e-6), bins = 60, alpha = 0.3, fill = "orange") +
  geom_histogram(data = p0[pr_wind_surge_flood_loss > 0, ], aes(x = pr_wind_surge_flood_loss, weight = poppm * 1e-6), bins = 60, alpha = 0.3, fill = "black") +
  corner_labels(cmp_labels, cmp_colors) +
  base_theme +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_cartesian(xlim = c(0, 0.2))

p_val_compound <- ggplot() +
  geom_histogram(data = p0[pr_wind_surge_loss  > 0, ], aes(x = pr_wind_surge_loss,  weight = val * 1e-9), bins = 60, alpha = 0.3, fill = "seagreen") +
  geom_histogram(data = p0[pr_wind_flood_loss  > 0, ], aes(x = pr_wind_flood_loss,  weight = val * 1e-9), bins = 60, alpha = 0.3, fill = "red") +
  geom_histogram(data = p0[pr_surge_flood_loss > 0, ], aes(x = pr_surge_flood_loss, weight = val * 1e-9), bins = 60, alpha = 0.3, fill = "orange") +
  geom_histogram(data = p0[pr_wind_surge_flood_loss > 0, ], aes(x = pr_wind_surge_flood_loss, weight = val * 1e-9), bins = 60, alpha = 0.3, fill = "black") +
  base_theme + labs(x = "Probability of non-zero compound loss", y = NULL) +
  theme(axis.title.y = element_blank()) +
  coord_cartesian(xlim = c(0, 0.2))

fig2 <- (p_pop_individual + p_pop_compound) / (p_val_individual + p_val_compound)

ggsave(fig2, file = file.path(figure_dir, "probability_exposure_histograms.png"),
       width = 10, height = 7, dpi = 600, bg = "white")

# ---------------------------------------------------------------------------
# 5. Figure 3: population, assets and loss by single-hazard intensity
# ---------------------------------------------------------------------------
summarize_by_intensity <- function(dat, intensity_col, loss_col) {
  dat[get(loss_col) > 0, .(
    exposure_pop = sum(poppm),
    exposure_val = sum(val),
    loss         = sum(get(loss_col))
  ), by = c(intensity_col, "storm_id")][, .(
    exposure_pop    = median(exposure_pop),
    exposure_pop025 = quantile(exposure_pop, 0.025),
    exposure_pop250 = quantile(exposure_pop, 0.25),
    exposure_pop750 = quantile(exposure_pop, 0.75),
    exposure_pop975 = quantile(exposure_pop, 0.975),
    exposure_val    = median(exposure_val),
    exposure_val025 = quantile(exposure_val, 0.025),
    exposure_val250 = quantile(exposure_val, 0.25),
    exposure_val750 = quantile(exposure_val, 0.75),
    exposure_val975 = quantile(exposure_val, 0.975),
    loss    = median(loss),
    loss025 = quantile(loss, 0.025),
    loss250 = quantile(loss, 0.25),
    loss750 = quantile(loss, 0.75),
    loss975 = quantile(loss, 0.975)
  ), by = intensity_col]
}

d4_wind_only  <- summarize_by_intensity(d1, "max_wind_terrain",             "wind_loss_only")
d4_surge_only <- summarize_by_intensity(d1, "max_surge_inundation_terrain", "surge_loss_only")
d4_flood_only <- summarize_by_intensity(d1, "max_inundation_terrain",       "flood_loss_only")

ceil_to     <- function(x, step) ceiling(x / step) * step
sf_pop_lim  <- c(0, ceil_to(1e-3 * max(d4_surge_only$exposure_pop975, d4_flood_only$exposure_pop975), 2))
sf_val_lim  <- c(0, ceil_to(1e-9 * max(d4_surge_only$exposure_val975, d4_flood_only$exposure_val975), 1))
sf_loss_lim <- c(0, ceil_to(1e-9 * max(d4_surge_only$loss975,         d4_flood_only$loss975),         0.05))

pop_hist_wind_only <- ggplot() +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-3 * exposure_pop975), color = NA, fill = "seagreen", alpha = 0.2) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-3 * exposure_pop750), color = NA, fill = "seagreen", alpha = 0.4) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-3 * exposure_pop250), color = NA, fill = "white") +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-3 * exposure_pop250), color = NA, fill = "seagreen", alpha = 0.2) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-3 * exposure_pop025), color = NA, fill = "white") +
  geom_step(stat = "bin", data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-3 * exposure_pop), color = "seagreen") +
  theme_bw(base_size = 10) + ylab("Population (000)") + ggtitle("Wind") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5))

val_hist_wind_only <- ggplot() +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * exposure_val975), color = NA, fill = "orange", alpha = 0.2) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * exposure_val750), color = NA, fill = "orange", alpha = 0.4) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * exposure_val250), color = NA, fill = "white") +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * exposure_val250), color = NA, fill = "orange", alpha = 0.2) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * exposure_val025), color = NA, fill = "white") +
  geom_step(stat = "bin", data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * exposure_val), color = "orange") +
  theme_bw(base_size = 10) + ylab("Assets (Bn $)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

loss_hist_wind_only <- ggplot() +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * loss975), color = NA, fill = "red", alpha = 0.2) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * loss750), color = NA, fill = "red", alpha = 0.4) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * loss250), color = NA, fill = "white") +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * loss250), color = NA, fill = "red", alpha = 0.2) +
  geom_histogram(data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * loss025), color = NA, fill = "white") +
  geom_step(stat = "bin", data = d4_wind_only, aes(x = max_wind_terrain, weight = 1e-9 * loss), color = "red") +
  theme_bw(base_size = 10) + ylab("Loss (Bn $)") + xlab("Wind speed (mph)")

pop_hist_surge_only <- ggplot() +
  geom_stepribbon(data = d4_surge_only, aes(x = max_surge_inundation_terrain, ymin = 1e-3 * exposure_pop025, ymax = 1e-3 * exposure_pop975), color = NA, fill = "seagreen", alpha = 0.2) +
  geom_stepribbon(data = d4_surge_only, aes(x = max_surge_inundation_terrain, ymin = 1e-3 * exposure_pop250, ymax = 1e-3 * exposure_pop750), color = NA, fill = "seagreen", alpha = 0.4) +
  geom_step(data = d4_surge_only, aes(x = max_surge_inundation_terrain, y = 1e-3 * exposure_pop), color = "seagreen") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  coord_cartesian(ylim = sf_pop_lim) +
  theme_bw(base_size = 10) + ggtitle("Surge") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5))

val_hist_surge_only <- ggplot() +
  geom_stepribbon(data = d4_surge_only, aes(x = max_surge_inundation_terrain, ymin = 1e-9 * exposure_val025, ymax = 1e-9 * exposure_val975), color = NA, fill = "orange", alpha = 0.2) +
  geom_stepribbon(data = d4_surge_only, aes(x = max_surge_inundation_terrain, ymin = 1e-9 * exposure_val250, ymax = 1e-9 * exposure_val750), color = NA, fill = "orange", alpha = 0.4) +
  geom_step(data = d4_surge_only, aes(x = max_surge_inundation_terrain, y = 1e-9 * exposure_val), color = "orange") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  coord_cartesian(ylim = sf_val_lim) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank())

loss_hist_surge_only <- ggplot() +
  geom_stepribbon(data = d4_surge_only, aes(x = max_surge_inundation_terrain, ymin = 1e-9 * loss025, ymax = 1e-9 * loss975), color = NA, fill = "red", alpha = 0.2) +
  geom_stepribbon(data = d4_surge_only, aes(x = max_surge_inundation_terrain, ymin = 1e-9 * loss250, ymax = 1e-9 * loss750), color = NA, fill = "red", alpha = 0.4) +
  geom_step(data = d4_surge_only, aes(x = max_surge_inundation_terrain, y = 1e-9 * loss), color = "red") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  coord_cartesian(ylim = sf_loss_lim) +
  theme_bw(base_size = 10) + xlab("Depth (ft)") +
  theme(axis.title.y = element_blank())

pop_hist_flood_only <- ggplot() +
  geom_stepribbon(data = d4_flood_only, aes(x = max_inundation_terrain, ymin = 1e-3 * exposure_pop025, ymax = 1e-3 * exposure_pop975), color = NA, fill = "seagreen", alpha = 0.2) +
  geom_stepribbon(data = d4_flood_only, aes(x = max_inundation_terrain, ymin = 1e-3 * exposure_pop250, ymax = 1e-3 * exposure_pop750), color = NA, fill = "seagreen", alpha = 0.4) +
  geom_step(data = d4_flood_only, aes(x = max_inundation_terrain, y = 1e-3 * exposure_pop), color = "seagreen") +
  coord_cartesian(ylim = sf_pop_lim) +
  theme_bw(base_size = 10) + ggtitle("Flood") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5))

val_hist_flood_only <- ggplot() +
  geom_stepribbon(data = d4_flood_only, aes(x = max_inundation_terrain, ymin = 1e-9 * exposure_val025, ymax = 1e-9 * exposure_val975), color = NA, fill = "orange", alpha = 0.2) +
  geom_stepribbon(data = d4_flood_only, aes(x = max_inundation_terrain, ymin = 1e-9 * exposure_val250, ymax = 1e-9 * exposure_val750), color = NA, fill = "orange", alpha = 0.4) +
  geom_step(data = d4_flood_only, aes(x = max_inundation_terrain, y = 1e-9 * exposure_val), color = "orange") +
  coord_cartesian(ylim = sf_val_lim) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank())

loss_hist_flood_only <- ggplot() +
  geom_stepribbon(data = d4_flood_only, aes(x = max_inundation_terrain, ymin = 1e-9 * loss025, ymax = 1e-9 * loss975), color = NA, fill = "red", alpha = 0.2) +
  geom_stepribbon(data = d4_flood_only, aes(x = max_inundation_terrain, ymin = 1e-9 * loss250, ymax = 1e-9 * loss750), color = NA, fill = "red", alpha = 0.4) +
  geom_step(data = d4_flood_only, aes(x = max_inundation_terrain, y = 1e-9 * loss), color = "red") +
  coord_cartesian(ylim = sf_loss_lim) +
  theme_bw(base_size = 10) + xlab("Depth (ft)") +
  theme(axis.title.y = element_blank())

fig3 <- pop_hist_wind_only  + pop_hist_surge_only  + pop_hist_flood_only +
        val_hist_wind_only  + val_hist_surge_only  + val_hist_flood_only +
        loss_hist_wind_only + loss_hist_surge_only + loss_hist_flood_only +
        plot_layout(ncol = 3)

ggsave(fig3, file = file.path(figure_dir, "hist_exposure_loss_individual_hazards.png"),
       width = 10, height = 8, dpi = 600, bg = "white")

# ---------------------------------------------------------------------------
# 6. Figure 4: bivariate wind+flood and wind+surge tile heatmaps
# ---------------------------------------------------------------------------
d1[, max_wind_terrain10 := round(max_wind_terrain / 10, 0) * 10]

d4_wind_flood <- d1[wind_flood_loss > 0, .(
  exposure_pop = sum(poppm), exposure_val = sum(val), loss = sum(wind_flood_loss)
), by = c("max_wind_terrain10", "max_inundation_terrain", "storm_id")][, .(
  exposure_pop   = median(exposure_pop),
  exposure_pop95 = quantile(exposure_pop, 0.95),
  exposure_pop99 = quantile(exposure_pop, 0.99),
  exposure_val   = median(exposure_val),
  exposure_val95 = quantile(exposure_val, 0.95),
  exposure_val99 = quantile(exposure_val, 0.99),
  loss   = median(loss),
  loss95 = quantile(loss, 0.95),
  loss99 = quantile(loss, 0.99)
), by = c("max_wind_terrain10", "max_inundation_terrain")]

d4_wind_surge <- d1[wind_surge_loss > 0, .(
  exposure_pop = sum(poppm), exposure_val = sum(val), loss = sum(wind_surge_loss)
), by = c("max_wind_terrain10", "max_surge_inundation_terrain")][, .(
  exposure_pop   = median(exposure_pop),
  exposure_pop95 = quantile(exposure_pop, 0.95),
  exposure_pop99 = quantile(exposure_pop, 0.99),
  exposure_val   = median(exposure_val),
  exposure_val95 = quantile(exposure_val, 0.95),
  exposure_val99 = quantile(exposure_val, 0.99),
  loss   = median(loss),
  loss95 = quantile(loss, 0.95),
  loss99 = quantile(loss, 0.99)
), by = c("max_wind_terrain10", "max_surge_inundation_terrain")]

d4_wind_flood[, `:=`(
  exposure_pop_k   = 1e-3 * exposure_pop,
  exposure_pop95_k = 1e-3 * exposure_pop95,
  exposure_pop99_k = 1e-3 * exposure_pop99,
  exposure_val_b   = 1e-9 * exposure_val,
  exposure_val95_b = 1e-9 * exposure_val95,
  exposure_val99_b = 1e-9 * exposure_val99,
  loss_m   = 1e-6 * loss,
  loss95_m = 1e-6 * loss95,
  loss99_m = 1e-6 * loss99
)]
d4_wind_surge[, `:=`(
  exposure_pop_k   = 1e-3 * exposure_pop,
  exposure_pop95_k = 1e-3 * exposure_pop95,
  exposure_pop99_k = 1e-3 * exposure_pop99,
  exposure_val_b   = 1e-9 * exposure_val,
  exposure_val95_b = 1e-9 * exposure_val95,
  exposure_val99_b = 1e-9 * exposure_val99,
  loss_b   = 1e-9 * loss,
  loss95_b = 1e-9 * loss95,
  loss99_b = 1e-9 * loss99
)]

tile_plot <- function(dat, xvar, yvar, fillvar, limits, title = NULL, ylab = NULL, xlab = NULL,
                      legend = FALSE, annot = NULL,
                      hide_x_axis = FALSE, hide_y_axis = FALSE) {
  p <- ggplot() +
    geom_tile(data = dat, aes(x = .data[[xvar]], y = .data[[yvar]], fill = .data[[fillvar]])) +
    scale_fill_gradientn(colors = mycolors, limits = limits) +
    theme_bw(base_size = 12, base_family = FONT) +
    theme(legend.position = if (legend) "right" else "none",
          legend.justification = c(0, 0.5),
          legend.title = element_blank(),
          legend.text  = element_text(size = 11, face = "plain", family = FONT),
          plot.title   = element_text(size = 13, face = "plain", hjust = 0.5, family = FONT),
          axis.title   = element_text(size = 12, face = "plain", family = FONT),
          axis.text    = element_text(size = 11, face = "plain", family = FONT),
          text         = element_text(face = "plain", family = FONT))
  if (!is.null(title)) p <- p + ggtitle(title)
  if (!is.null(ylab))  p <- p + ylab(ylab) else p <- p + theme(axis.title.y = element_blank())
  if (!is.null(xlab))  p <- p + xlab(xlab) else p <- p + theme(axis.title.x = element_blank())
  if (hide_x_axis) p <- p + theme(axis.text.x = element_blank())
  if (hide_y_axis) p <- p + theme(axis.text.y = element_blank())
  if (!is.null(annot)) p <- p + annotate(geom = "text", label = annot, x = -Inf, y = Inf,
                                         hjust = -0.08, vjust = 1.6,
                                         size = 3.8, family = FONT, fontface = "plain")
  p
}

p_wf_pop50  <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "exposure_pop_k",   limits = c(0, 13), title = "Median",          ylab = "Depth (ft)", hide_x_axis = TRUE, annot = "Population (000)")
p_wf_pop95  <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "exposure_pop95_k", limits = c(0, 13), title = "95th percentile", hide_x_axis = TRUE, hide_y_axis = TRUE)
p_wf_pop99  <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "exposure_pop99_k", limits = c(0, 13), title = "99th percentile", hide_x_axis = TRUE, hide_y_axis = TRUE, legend = TRUE)
p_wf_val50  <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "exposure_val_b",   limits = c(0, 7.5), ylab = "Depth (ft)", hide_x_axis = TRUE, annot = "Assets (Bn $)")
p_wf_val95  <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "exposure_val95_b", limits = c(0, 7.5), hide_x_axis = TRUE, hide_y_axis = TRUE)
p_wf_val99  <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "exposure_val99_b", limits = c(0, 7.5), hide_x_axis = TRUE, hide_y_axis = TRUE, legend = TRUE)
p_wf_loss50 <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "loss_m",   limits = c(0, 300), ylab = "Depth (ft)", annot = "Loss (M $)")
p_wf_loss95 <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "loss95_m", limits = c(0, 300), xlab = "Wind speed (mph)", hide_y_axis = TRUE)
p_wf_loss99 <- tile_plot(d4_wind_flood, "max_wind_terrain10", "max_inundation_terrain", "loss99_m", limits = c(0, 300), hide_y_axis = TRUE, legend = TRUE)

bivariate_wind_flood <- p_wf_pop50  + p_wf_pop95  + p_wf_pop99 +
                        p_wf_val50  + p_wf_val95  + p_wf_val99 +
                        p_wf_loss50 + p_wf_loss95 + p_wf_loss99 +
                        plot_layout(ncol = 3)

p_ws_pop50  <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "exposure_pop_k",   limits = c(0, 700), title = "Median",          ylab = "Depth (ft)", hide_x_axis = TRUE, annot = "Population (000)")
p_ws_pop95  <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "exposure_pop95_k", limits = c(0, 700), title = "95th percentile", hide_x_axis = TRUE, hide_y_axis = TRUE)
p_ws_pop99  <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "exposure_pop99_k", limits = c(0, 700), title = "99th percentile", hide_x_axis = TRUE, hide_y_axis = TRUE, legend = TRUE)
p_ws_val50  <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "exposure_val_b",   limits = c(0, 300), ylab = "Depth (ft)", hide_x_axis = TRUE, annot = "Assets (Bn $)")
p_ws_val95  <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "exposure_val95_b", limits = c(0, 300), hide_x_axis = TRUE, hide_y_axis = TRUE)
p_ws_val99  <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "exposure_val99_b", limits = c(0, 300), hide_x_axis = TRUE, hide_y_axis = TRUE, legend = TRUE)
p_ws_loss50 <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "loss_b",   limits = c(0, 9), ylab = "Depth (ft)", annot = "Loss (Bn $)")
p_ws_loss95 <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "loss95_b", limits = c(0, 9), xlab = "Wind speed (mph)", hide_y_axis = TRUE)
p_ws_loss99 <- tile_plot(d4_wind_surge, "max_wind_terrain10", "max_surge_inundation_terrain", "loss99_b", limits = c(0, 9), hide_y_axis = TRUE, legend = TRUE)

bivariate_wind_surge <- p_ws_pop50  + p_ws_pop95  + p_ws_pop99 +
                        p_ws_val50  + p_ws_val95  + p_ws_val99 +
                        p_ws_loss50 + p_ws_loss95 + p_ws_loss99 +
                        plot_layout(ncol = 3)

title_wf <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Wind + Flood",
                                size = 5.2, fontface = "plain", family = FONT) +
  theme_void(base_family = FONT)
title_ws <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Wind + Surge",
                                size = 5.2, fontface = "plain", family = FONT) +
  theme_void(base_family = FONT)

fig4 <- cowplot::plot_grid(
  title_wf, bivariate_wind_flood,
  title_ws, bivariate_wind_surge,
  ncol = 1,
  rel_heights = c(0.04, 1, 0.04, 1)
)

ggsave(fig4, file = file.path(figure_dir, "bivariate_exposure_loss.png"),
       width = 8, height = 12, dpi = 300, bg = "white")
