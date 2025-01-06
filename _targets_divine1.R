library(targets)
library(tarchetypes)
library(here)
library(qs)

# Always use GMT, never BST
Sys.setenv(TZ = "GMT")
set.seed(448)

# Set target options:
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2",
  "lubridate", "dplyr", "future", "viridis", "lme4", "ggeffects",
  "photobiology", "mgcv", "ggpmisc")
tar_option_set(
  packages = v_pkgs,
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

data.table::setDTthreads(threads = 1)
data.table::getDTthreads()

list(
  # read the list of metadata data tables
  tar_target(fname_meta, "_targets/objects/l_meta", format = "file"),
  tar_target(l_meta, qread(fname_meta)),

  tar_target(fname_dt_flux, "_targets/objects/dt_flux", format = "file"),
  tar_target(dt_flux, qread(fname_dt_flux)[expt_id == "divine1"]),

  tar_target(test, summary(dt_flux)),

  tar_target(
    name = p_flux_n2o,
    command = plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
                              sigma_name = "sigma_f_n2o", xvar_name = "datect",
                              colour_name = "chamber_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = TRUE,
                              mult = 1000)
  ),
  # # this takes ages - needs checking
  #   tar_target(
  # name = p_flux_n2o_T,
  #   command = plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "TSoil",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  tar_target(
    name = p_flux_n2o_with_Nappl,
    command = plot_n2o_flux(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = "SHP", this_expt_id = "divine1",
      l_meta, mult = 1000)
  ),
  tar_target(
    name = p_flux_n2o_diurnal,
    command = plot_n2o_flux_diurnal(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = "SHP", this_expt_id = "divine1",
      mult = 1000, y_min = -2, y_max = 2.5)
  ),
  tar_target(
    name = p_bar_n2o,
    command = bar_means_by_trmt(dt_flux,
      flux_name = "f_n2o", mult = 1000)
  ),
  # check basic environmental variables
  tar_target(
    name = p_T,
    command = plot_flux_vs_xvar(dt_flux, flux_name = "TA",
                              sigma_name = "f_n2o", xvar_name = "datect",
                              colour_name = "f_n2o", facet_name = "chamber_id",
                              colour_is_factor = FALSE, rows_only = FALSE)
  ),

  # report file:
  tar_render(report_html, here("analysis", "skyline_analysis_divine1.Rmd"))
)