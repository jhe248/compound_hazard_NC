# config.R
# Paths, packages and the hazard-file index shared by every script.
# Edit project_dir and hazard_dir_csv, then source this file.

project_dir    <- "/projectnb/cheerbugrp/jhhe/hazard_update_07_2026"
hazard_dir_csv <- "/projectnb/cheerbugrp/isw/hazard_data/07-2026"

code_dir   <- file.path(project_dir, "code")
input_dir  <- file.path(project_dir, "input_data")
output_dir <- file.path(project_dir, "output_data")
figure_dir <- file.path(project_dir, "output_files", "figures")
si_dir     <- file.path(figure_dir, "SI_figures")
table_dir  <- file.path(project_dir, "output_files", "tables")

for (d in c(output_dir, figure_dir, si_dir, table_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---------------------------------------------------------------------------
# 1. Hazard CSV index (604 storms split across 8 latitude bins)
# ---------------------------------------------------------------------------
hazard_files <- list.files(hazard_dir_csv, pattern = ".*csv", full.names = TRUE)
chunk_zone   <- gsub(".*bin_|.csv", "", hazard_files)
n_chunk      <- length(hazard_files)

# ---------------------------------------------------------------------------
# 2. Packages
# ---------------------------------------------------------------------------
required_pkgs <- c(
  "qs2", "data.table", "dplyr", "ggplot2", "patchwork", "scales", "cowplot",
  "bit64", "sf", "stringr", "pammtools", "tricolore", "ggtern", "extrafont",
  "tigris", "magick", "readxl"
)
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

options(tigris_use_cache = TRUE)

if (.Platform$OS.type == "windows") {
  try(grDevices::windowsFonts(Arial = grDevices::windowsFont("Arial")), silent = TRUE)
}
