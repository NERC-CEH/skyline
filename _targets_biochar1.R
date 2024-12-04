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
  tar_target(l_meta, qread("_targets/objects/l_meta")),

  tar_target(fname_dt_flux, "_targets/objects/dt_flux", format = "file"),
  tar_target(dt_flux, qread(fname_dt_flux)[expt_id == "biochar1"]),
  tar_target(test, summary(dt_flux)),

  # biochar1
  tar_target(
    name = p_flux_co2_biochar1,
    command = plot_flux_vs_xvar(dt_flux, flux_name = "f_co2",
                              sigma_name = "sigma_f_co2", xvar_name = "datect",
                              colour_name = "chamber_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = TRUE,
                              mult = 1)
  ),
  tar_target(
    name = p_flux_ch4_biochar1,
    command = plot_flux_vs_xvar(dt_flux, flux_name = "f_ch4",
                              sigma_name = "sigma_f_ch4", xvar_name = "datect",
                              colour_name = "chamber_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = TRUE,
                              mult = 1)
  ),
  tar_target(
    name = p_flux_n2o_biochar1,
    command = plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
                              sigma_name = "sigma_f_n2o", xvar_name = "datect",
                              colour_name = "chamber_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = TRUE,
                              mult = 1000)
  ),
  # # this takes ages - needs checking
  #   tar_target(
  # name = p_flux_n2o_T_biochar1,
  #   command = plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "TSoil",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  tar_target(
    name = p_flux_n2o_with_Nappl_biochar1,
    command = plot_n2o_flux(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "biochar1",
      l_meta, mult = 1000)
  ),
  tar_target(
    name = p_flux_n2o_diurnal_biochar1,
    command = plot_n2o_flux_diurnal(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "biochar1",
      mult = 1000, y_min = -2, y_max = 2.5)
  ),
  tar_target(
    name = p_bar_n2o_biochar1,
    command = bar_means_by_trmt(dt_flux,
      flux_name = "f_n2o", mult = 1000)
  )
)