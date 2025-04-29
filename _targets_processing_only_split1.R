library(targets)
library(tarchetypes)
library(here)
library(qs)
library(rmarkdown)
library(data.table)
# Uncomment the following code and use tar_make_future(workers = 5L) to make
# the targets in parallel sessions on the local machine.
# library(future)
# future::plan("multicore")

# Always use GMT, never BST
Sys.setenv(TZ = "GMT")
set.seed(448)

# Set target options:
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2",
           "lubridate", "dplyr", "future", "viridis", "lme4", "ggeffects",
           "photobiology", "mgcv", "ggpmisc", "fastmap")
tar_option_set(
  packages = v_pkgs,
  format = "qs"
)

tar_source()

data.table::setDTthreads(threads = 1)
data.table::getDTthreads()

#### SPECIFY ####
# site, experiment, and dates to process:
site_id <- "EHD"
expt_id <- "split1"


# default to process all dates in experiment
v_dates <- NULL
# or uncomment lines below to specify a subset of dates
# start_date <-  "2023-07-02"
# end_date   <-  "2023-07-06"
# v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))

seq_id_to_plot <- 1   # default to 1 as night/dark flux so should be clear if something is wrong with deadbands
save_plots <- TRUE   # save plots for all gases containing every flux per chamber each day
write_all <- TRUE   # combine files for days processed
diagnostic_plots <- TRUE

n_min <- 100
method <-  "time fit"  # "time fit" or "specified deadband only"
# dryrun <- FALSE  # FALSE = remove deadbands and calculate fluxes, TRUE = plots showing deadbands for visual checking before calculating fluxes





#### list of targets: ####

  list(
    tar_target(fname_meta_xlsx, "data-raw/skyline_meta-data.xlsx",
      cue = tar_cue(mode = "always")
    ),

    tar_target(
      name = v_fname_meta_csv, format = "file",
      command = convert_metadata_csv(fname_meta_xlsx)
    ),

    tar_target(
      name = l_meta,
      command = read_metadata(v_fname_meta_csv)
    ),

    tar_target(
      name = dt_chi,
      command = get_data(v_dates, this_site_id = site_id, this_expt_id = expt_id,
                         l_meta,seq_id_to_plot = seq_id_to_plot, diagnostic_plots = diagnostic_plots,
                         method = method, save_plots = save_plots,
                         write_all = write_all, n_min = n_min)
    ),

    tar_target(
      name = dt,
      command = get_flux(dt_chi)
    ),

    tar_target(
      name = null_1,
      command = fwrite(dt, file = paste0(here("output", dt[1, .(site_id, expt_id)]), "/dt_flux_unfilt.csv"))
    ),

    tar_target(
      name = dt_flux,
      command = filter_fluxes2(dt, save_file = TRUE, fname = "dt_flux", rmse_threshold = 5)
    # ),

  ## post-processing plots

  # tar_target(
  # name = p_flux_co2,
  # command = plot_flux(dt_flux, flux_name = "f_co2",
  # sigma_name = "sigma_f_co2", site_id, expt_id,
  # mult = 1, y_min = -25, y_max = 25)
  # ),
  # tar_target(
  # name = p_flux_ch4,
  # command = plot_flux(dt_flux, flux_name = "f_ch4",
  # sigma_name = "sigma_f_ch4", site_id, expt_id,
  # mult = 1000, y_min = -5, y_max = 5)
  # ),
  # tar_target(
  # name = p_flux_n2o,
  # command = plot_flux(dt_flux, flux_name = "f_n2o",
  # sigma_name = "sigma_f_n2o", site_id, expt_id,
  # mult = 1000, y_min = -2, y_max = 10)
  # ),
  # tar_target(
  # name = p_flux_n2o_T,
  # command = plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "TSoil",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  # name = p_flux_n2o_with_Nappl,
  # command = plot_n2o_flux(dt_flux, flux_name = "f_n2o",
  # sigma_name = "sigma_f_n2o",
  # l_meta, mult = 1000, y_min = -2, y_max = 10)
  # ),
  # tar_target(
  # name = p_flux_n2o_diurnal,
  # command = plot_n2o_flux_diurnal(dt_flux, flux_name = "f_n2o",
  # sigma_name = "sigma_f_n2o", this_site_id = site_id, this_expt_id = expt_id,
  # mult = 1000, y_min = -2, y_max = 2.5)
  # ),
  # tar_target(
  #   name = p_bar_n2o,
  #   command = bar_means_by_trmt(dt_flux, flux_name = "f_n2o", mult = 1000)
  # ),
  #
  # # nonlinearity filter plots
  # tar_target(
  #   name = dt_p_nonlinearity,
  #   command = plot_chi_co2_with_rmse(dt, n = 50, save_plot = TRUE)
  # # ),

  # # manuscript file:
  # tar_render(manuscript_pdf, here("manuscripts", site_id, expt_id, "skyline_analysis.Rmd")
))

