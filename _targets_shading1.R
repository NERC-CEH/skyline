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
  tar_target(dt_flux_in, qread(fname_dt_flux)[expt_id == "shading1"]),
  # choose sign convention for CO2 fluxes
  tar_target(dt_flux, switch_sign_co2(dt_flux_in, convention_in = "meterological")),

  tar_target(test, summary(dt_flux)),

  tar_target(
    name = p_flux_co2,
    command = plot_flux_vs_xvar(dt_flux, flux_name = "f_co2",
                              sigma_name = "sigma_f_co2", xvar_name = "datect",
                              colour_name = "chamber_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = TRUE,
                              mult = 1)
  ),
  tar_target(
    name = p_flux_ch4,
    command = plot_flux_vs_xvar(dt_flux, flux_name = "f_ch4",
                              sigma_name = "sigma_f_ch4", xvar_name = "datect",
                              colour_name = "chamber_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = TRUE,
                              mult = 1)
  ),
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
      sigma_name = "sigma_f_n2o", this_site_id = "HRG", this_expt_id = "shading1",
      l_meta, mult = 1000)
  ),
  tar_target(
    name = p_flux_n2o_diurnal,
    command = plot_n2o_flux_diurnal(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = "HRG", this_expt_id = "shading1",
      mult = 1000, y_min = -2, y_max = 2.5)
  ),
  tar_target(
    name = p_bar_n2o,
    command = bar_means_by_trmt(dt_flux,
      flux_name = "f_n2o", mult = 1000)
  ),
  # flux partitioning
  tar_target(
    name = dt,
    command = partition_fluxes(dt_flux, method = "regression")
  ),
  # check basic environmental variables
  tar_target(
    name = p_T,
    command = plot_flux_vs_xvar(dt, flux_name = "TA",
                              sigma_name = "sigma_f_ch4", xvar_name = "datect",
                              colour_name = "VWC", facet_name = "chamber_id",
                              colour_is_factor = FALSE, rows_only = FALSE)
  ),
  tar_target(
    name = p_Q,
    command = plot_flux_vs_xvar(dt, flux_name = "PPFD_IN",
                              sigma_name = "sigma_f_ch4", xvar_name = "datect",
                              colour_name = "TA", facet_name = "chamber_id",
                              colour_is_factor = FALSE, rows_only = FALSE)
  ),
  tar_target(
    name = p_VWC,
    command = plot_flux_vs_xvar(dt, flux_name = "VWC",
                              sigma_name = "sigma_f_ch4", xvar_name = "datect",
                              colour_name = "PPFD_IN", facet_name = "chamber_id",
                              colour_is_factor = FALSE, rows_only = FALSE)
  ),
  tar_target(
    name = p_reco_T_response,
    command = plot_flux_vs_xvar(dt[light == FALSE], flux_name = "f_co2",
                              sigma_name = "sigma_f_co2", xvar_name = "TA",
                              colour_name = "chamber_id", facet_name = "chamber_id",
                              colour_is_factor = TRUE, rows_only = FALSE) +
                              geom_line(aes(y = R, colour = as.factor(month)))
  ),
  tar_target(
    name = p_gpp_Q_response,
    command = plot_flux_vs_xvar(dt[light == TRUE], flux_name = "P",
                              sigma_name = "sigma_f_co2", xvar_name = "PPFD_IN",
                              colour_name = "TA", facet_name = "chamber_id",
                              colour_is_factor = FALSE, rows_only = FALSE) +
                              geom_line(aes(y = R, colour = TA))
  ),
  tar_target(
    name = p_gpp_date,
    command = plot_flux_vs_xvar(dt, flux_name = "P",
                              sigma_name = "sigma_f_co2", xvar_name = "datect",
                              colour_name = "chamber_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = TRUE,
                              mult = 1)
  ),

  tar_target(
    name = dt_ts,
    command = expand_to_complete_ts(dt)
  ),

  tar_target(
    name = dt_gf,
    command = fill_gaps_PPFD_dTA_VWC(dt_ts)
  ),

  tar_target(
    name = dt_cum,
    command = get_cum_f_co2(dt_gf)
  ),

  tar_target(
    name = p_cum,
    command = plot_cum_f_co2(dt_cum)
  ),

  # report file:
  tar_render(report_html, here("analysis", "skyline_analysis_shading1.Rmd"))
)