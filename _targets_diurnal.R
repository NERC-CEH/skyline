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
  tar_target(dt_flux_in, qread(fname_dt_flux)),
  # choose sign convention for CO2 fluxes
  tar_target(dt_flux, switch_sign_co2(dt_flux_in, convention_in = "meterological")),

  tar_target(test, summary(dt_flux)),

  # tar_target(
  #   name = p_flux_co2,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id != "divine1"], flux_name = "f_co2",
  #                             sigma_name = "sigma_f_co2", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1)
  # ),

  # tar_target(
  #   name = p_flux_n2o,
  #   command = plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),

    # plot diurnals
  tar_target(
    name = p_diurnal_lumped,
    command = plot_diurnal(dt_flux, split_by_day = FALSE, split_by_expt = FALSE, split_by_chamber = FALSE)
  ),
  tar_target(
    name = p_diurnal_by_expt,
    command = plot_diurnal(dt_flux, split_by_day = FALSE, split_by_expt = TRUE, split_by_chamber = FALSE)
  ),
  tar_target(
    name = p_diurnal_by_day,
    command = plot_diurnal(dt_flux, split_by_day = TRUE,  split_by_expt = FALSE, split_by_chamber = FALSE)
  ),
  tar_target(
    name = p_diurnal_by_day_by_expt_not_chamber,
    command = plot_diurnal(dt_flux, split_by_day = TRUE,  split_by_expt = TRUE, split_by_chamber = FALSE)
  ),

  tar_target(
    name = dt,
    command = finding_Nema(dt_flux, l_meta)
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

  # report file:
  tar_render(report_html, here("analysis", "skyline_analysis_diurnal.Rmd"))
)