# run_all.R
# Master script. Source it with the working directory set to the project root.
#
#   00-02  data and tables
#   03-04  main-text Figures 2-5
#   05-06  insurance data and main-text Figure 6
#   07-08  supplementary figures and tables
#
# 09_kevin_zone_losses.R and 10_flood_diagnostics.R are standalone extras: they
# are not part of the pipeline. Run them after 00 with
#   source("code/config.R"); source("code/09_kevin_zone_losses.R")

if (!file.exists(file.path("code", "config.R"))) {
  stop("Set the working directory to the project root before sourcing run_all.R")
}

source(file.path("code", "config.R"))

source(file.path(code_dir, "00_loss_calculation.R"))
source(file.path(code_dir, "01_extensive_margin.R"))
source(file.path(code_dir, "02_intensive_margin.R"))
source(file.path(code_dir, "03_main_figures_exposure.R"))
source(file.path(code_dir, "04_main_figures_tricolore.R"))
source(file.path(code_dir, "05_insurance_data.R"))
source(file.path(code_dir, "06_main_figures_insurance.R"))
source(file.path(code_dir, "07_SI_figures_hazard.R"))
source(file.path(code_dir, "08_SI_figures_insurance.R"))
