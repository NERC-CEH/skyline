library(targets)
library(tarchetypes)
library(here)
library(qs)
# Uncomment the following code and use tar_make_future(workers = 5L) to make
# the targets in parallel sessions on the local machine.
library(future)
future::plan("multicore")
# Set futures options:
options(future.globals.maxSize = 21474836480)

# Always use GMT, never BST
Sys.setenv(TZ = "GMT")
set.seed(448)

# Set target options:
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2",
  "lubridate", "dplyr", "future", "viridis", "lme4", "ggeffects",
  # "mgcv", "ggpmisc")
  "photobiology", "mgcv", "ggpmisc")
tar_option_set(
  # envir = getNamespace("skyline"), use source code from installed package
  packages = v_pkgs,
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

data.table::setDTthreads(threads = 1)
data.table::getDTthreads()

#### SPECIFY ####
# site, experiment, and dates to process:
# site_id <- "EHD"
# expt_id <- "digestate1"

# default to process all dates in experiment
v_dates <- NULL
# or uncomment lines below to specify a subset of dates
# start_date <-  "2021-05-01"
# end_date   <-  "2021-05-05"
# v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))

seq_id_to_plot <- 5   # default to 1 as night/dark flux so should be clear if something is wrong with deadbands
save_plots <- FALSE   # save plots for all gases containing every flux per chamber each day
write_all <- FALSE   # combine files for days processes

n_min <- 100
method <-  "time fit"  # "time fit" or "specified deadband only"
# dryrun FALSE = remove deadbands and calculate fluxes, TRUE = plots showing deadbands for visual checking before calculating fluxes
dryrun <- FALSE

#### list of targets: ####
list(
  # an excel file containing all metadata; we track only the path name
  tar_target(fname_meta_xlsx, "data-raw/skyline_meta-data.xlsx"),

 # convert to csv files whose contents we track
  tar_target(
    name = v_fname_csv, format = "file",
    command = convert_metadata_csv(fname_meta_xlsx)
  ),

  # read these csv into a list of data tables
  tar_target(
    name = l_meta,
    command = read_metadata(v_fname_csv)
  ),

  tar_target(
    name = dt_chi_yield1,
    command = get_data(v_dates, this_site_id = "EHD", this_expt_id = "yield1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots,
      write_all = write_all, n_min = n_min)
  ),

  tar_target(
    name = dt_chi_split1,
    command = get_data(v_dates, this_site_id = "EHD", this_expt_id = "split1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots,
      write_all = write_all, n_min = n_min)
  ),

  tar_target(
    name = dt_chi_biochar1,
    command = get_data(v_dates, this_site_id = "EHD", this_expt_id = "biochar1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots,
      write_all = write_all, n_min = n_min)
  ),

  tar_target(
    name = dt_chi_digestate1,
    command = get_data(v_dates, this_site_id = "EHD", this_expt_id = "digestate1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots,
      write_all = write_all, n_min = n_min)
  ),

  tar_target(
    name = dt_chi_diurnal1,
    command = get_data(v_dates, this_site_id = "HRG", this_expt_id = "diurnal1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots,
      write_all = write_all, n_min = n_min)
  ),

  # take the mid-point day as an example to plot
  tar_target(
    name = example_date,
    command = dt_chi_biochar1[floor(dim(dt_chi_biochar1)[1] / 2),
                as.POSIXct(lubridate::date(datect))]
  ),

  # plot concentration against time for every mmnt sequence that day
  tar_target(
    name = p_chi_co2_biochar1,
    command = plot_chi(dt_chi_biochar1[
      example_date == as.POSIXct(lubridate::date(datect))],
      gas_name = "chi_co2")
  ),

  # post-processing - separate script or give prefix?
  tar_target(
    name = dt_flux_biochar1,
    command = get_flux(dt_chi_biochar1)
  ),

  tar_target(
    name = dt_flux_yield1,
    command = get_flux(dt_chi_yield1)
  ),

  tar_target(
    name = dt_flux_split1,
    command = get_flux(dt_chi_split1)
  ),

  tar_target(
    name = dt_flux_digestate1,
    command = get_flux(dt_chi_digestate1)
  ),

  tar_target(
    name = dt_flux_diurnal1,
    command = get_flux(dt_chi_diurnal1)
  ),

  tar_target(
    name = dt_flux_all,
    command = rbindlist(list(dt_flux_biochar1,
                             dt_flux_yield1,
                             dt_flux_split1,
                             dt_flux_digestate1,
                             dt_flux_diurnal1),
                             fill = TRUE)
  ),

  # tar_target(
  #   name = dt_flux_unfilt,
  #   command = dt_flux_all[, .SD[1], by = mmnt_id]
  # ),
  tar_target(
    name = dt_flux,
    command = filter_fluxes(dt_flux_all, save_file = FALSE, fname = "dt_flux")
  ),
  tar_target(
    name = null_1,
    command = qsave(dt_flux, file = here("output/dt_flux.qs"))
  )

  # # biochar1
  # tar_target(
  #   name = p_flux_co2_biochar1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "biochar1"], flux_name = "f_co2",
  #                             sigma_name = "sigma_f_co2", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1)
  # ),
  # tar_target(
  #   name = p_flux_ch4_biochar1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "biochar1"], flux_name = "f_ch4",
  #                             sigma_name = "sigma_f_ch4", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1)
  # ),
  # tar_target(
  #   name = p_flux_n2o_biochar1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "biochar1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_T_biochar1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "biochar1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "TSoil",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_with_Nappl_biochar1,
  #   command = plot_n2o_flux(dt_flux[expt_id == "biochar1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "biochar1",
  #     l_meta, mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_diurnal_biochar1,
  #   command = plot_n2o_flux_diurnal(dt_flux[expt_id == "biochar1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "biochar1",
  #     mult = 1000, y_min = -2, y_max = 2.5)
  # ),
  # tar_target(
  #   name = p_bar_n2o_biochar1,
  #   command = bar_means_by_trmt(dt_flux[expt_id == "biochar1"],
  #     flux_name = "f_n2o", mult = 1000)
  # ),

  # # yield1
  # tar_target(
  #   name = p_flux_co2_yield1,
  #   command = plot_flux(dt_flux[expt_id == "yield1"], flux_name = "f_co2",
  #     sigma_name = "sigma_f_co2", site_id = "EHD", expt_id = "yield1",
  #     mult = 1, y_min = -20, y_max = 25)
  # ),
  # tar_target(
  #   name = p_flux_ch4_yield1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "yield1"], flux_name = "f_ch4",
  #                             sigma_name = "sigma_f_ch4", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1)
  # ),
  # tar_target(
  #   name = p_flux_n2o_yield1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "yield1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_T_yield1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "yield1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "TSoil",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_with_Nappl_yield1,
  #   command = plot_n2o_flux(dt_flux[expt_id == "yield1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "yield1",
  #     l_meta, mult = 1000, y_min = -1, y_max = 5)
  # ),
  # tar_target(
  #   name = p_flux_n2o_diurnal_yield1,
  #   command = plot_n2o_flux_diurnal(dt_flux[expt_id == "yield1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "yield1",
  #     mult = 1000, y_min = -2, y_max = 2.5)
  # ),
  # tar_target(
  #   name = p_bar_n2o_yield1,
  #   command = bar_means_by_trmt(dt_flux[expt_id == "yield1"],
  #     flux_name = "f_n2o", mult = 1000)
  # ),

  # # split1
  # tar_target(
  #   name = p_flux_co2_split1,
  #   command = plot_flux(dt_flux[expt_id == "split1"], flux_name = "f_co2",
  #     sigma_name = "sigma_f_co2", site_id = "EHD", expt_id = "split1",
  #     mult = 1, y_min = -20, y_max = 25)
  # ),
  # tar_target(
  #   name = p_flux_ch4_split1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "split1"], flux_name = "f_ch4",
  #                             sigma_name = "sigma_f_ch4", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1)
  # ),
  # tar_target(
  #   name = p_flux_n2o_split1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "split1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_T_split1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "split1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "TSoil",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_with_Nappl_split1,
  #   command = plot_n2o_flux(dt_flux[expt_id == "split1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "split1",
  #     l_meta, mult = 1000, y_min = -1, y_max = 5)
  # ),
  # tar_target(
  #   name = p_flux_n2o_diurnal_split1,
  #   command = plot_n2o_flux_diurnal(dt_flux[expt_id == "split1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "split1",
  #     mult = 1000, y_min = -2, y_max = 2.5)
  # ),
  # tar_target(
  #   name = p_bar_n2o_split1,
  #   command = bar_means_by_trmt(dt_flux[expt_id == "split1"],
  #     flux_name = "f_n2o", mult = 1000)
  # ),

  # # digestate1
  # tar_target(
  #   name = p_flux_co2_digestate1,
  #   command = plot_flux(dt_flux[expt_id == "digestate1"], flux_name = "f_co2",
  #     sigma_name = "sigma_f_co2", site_id = "EHD", expt_id = "digestate1",
  #     mult = 1, y_min = -20, y_max = 25)
  # ),
  # tar_target(
  #   name = p_flux_ch4_digestate1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "digestate1"], flux_name = "f_ch4",
  #                             sigma_name = "sigma_f_ch4", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1)
  # ),
  # tar_target(
  #   name = p_flux_n2o_digestate1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "digestate1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_T_digestate1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "digestate1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "TSoil",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_with_Nappl_digestate1,
  #   command = plot_n2o_flux(dt_flux[expt_id == "digestate1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "digestate1",
  #     l_meta, mult = 1000, y_min = -1, y_max = 5)
  # ),
  # tar_target(
  #   name = p_flux_n2o_diurnal_digestate1,
  #   command = plot_n2o_flux_diurnal(dt_flux[expt_id == "digestate1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "digestate1",
  #     mult = 1000, y_min = -2, y_max = 2.5)
  # ),
  # tar_target(
  #   name = p_bar_n2o_digestate1,
  #   command = bar_means_by_trmt(dt_flux[expt_id == "digestate1"],
  #     flux_name = "f_n2o", mult = 1000)
  # ),

  # # diurnal1
  # tar_target(
  #   name = p_flux_co2_diurnal1,
  #   command = plot_flux(dt_flux[expt_id == "diurnal1"], flux_name = "f_co2",
  #     sigma_name = "sigma_f_co2", site_id = "HRG", expt_id = "diurnal1",
  #     mult = 1, y_min = -20, y_max = 25)
  # ),
  # tar_target(
  #   name = p_flux_ch4_diurnal1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "diurnal1"], flux_name = "f_ch4",
  #                             sigma_name = "sigma_f_ch4", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1)
  # ),
  # tar_target(
  #   name = p_flux_n2o_diurnal1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "diurnal1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "datect",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_T_diurnal1,
  #   command = plot_flux_vs_xvar(dt_flux[expt_id == "diurnal1"], flux_name = "f_n2o",
  #                             sigma_name = "sigma_f_n2o", xvar_name = "TSoil",
  #                             colour_name = "chamber_id", facet_name = "trmt_id",
  #                             colour_is_factor = TRUE, rows_only = TRUE,
  #                             mult = 1000)
  # ),
  # tar_target(
  #   name = p_flux_n2o_with_Nappl_diurnal1,
  #   command = plot_n2o_flux(dt_flux[expt_id == "diurnal1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "HRG", this_expt_id = "diurnal1",
  #     l_meta, mult = 1000, y_min = -5, y_max = 20)
  # ),
  # tar_target(
  #   name = p_flux_n2o_diurnal_diurnal1,
  #   command = plot_n2o_flux_diurnal(dt_flux[expt_id == "diurnal1"], flux_name = "f_n2o",
  #     sigma_name = "sigma_f_n2o", this_site_id = "HRG", this_expt_id = "diurnal1",
  #     mult = 1000, y_min = -2, y_max = 2.5)
  # ),
  # tar_target(
  #   name = p_bar_n2o_diurnal1,
  #   command = bar_means_by_trmt(dt_flux[expt_id == "diurnal1"],
  #     flux_name = "f_n2o", mult = 1000)
  # ),
  # # plot diurnals
  # tar_target(
  #   name = p_diurnal_lumped,
  #   command = plot_diurnal(dt_flux, split_by_day = FALSE, split_by_expt = FALSE)
  # ),
  # tar_target(
  #   name = p_diurnal_by_expt,
  #   command = plot_diurnal(dt_flux, split_by_day = FALSE, split_by_expt = TRUE)
  # ),
  # tar_target(
  #   name = p_diurnal_by_day,
  #   command = plot_diurnal(dt_flux, split_by_day = TRUE,  split_by_expt = FALSE)
  # ),
  # tar_target(
  #   name = p_diurnal_by_day_by_expt,
  #   command = plot_diurnal(dt_flux, split_by_day = TRUE,  split_by_expt = TRUE)
  # ),
  # # nonlinearity filter plots
  # tar_target(
  #   name = p_nonlinearity_biochar1,
  #   command = plot_chi_co2_with_rmse(dt[expt_id == "biochar1"], n = 9, save_plot = TRUE)
  # ),
  # tar_target(
  #   name = p_nonlinearity_split1,
  #   command = plot_chi_co2_with_rmse(dt[expt_id == "split1"], n = 9, save_plot = TRUE)
  # ),
  # tar_target(
  #   name = p_nonlinearity_yield1,
  #   command = plot_chi_co2_with_rmse(dt[expt_id == "yield1"], n = 9, save_plot = TRUE)
  # ),
  # tar_target(
  #   name = p_nonlinearity_digestate1,
  #   command = plot_chi_co2_with_rmse(dt[expt_id == "digestate1"], n = 9, save_plot = TRUE)
  # ),
  # tar_target(
  #   name = p_nonlinearity_diurnal1,
  #   command = plot_chi_co2_with_rmse(dt[expt_id == "diurnal1"], n = 9, save_plot = TRUE)
  # ),
  # # manuscript file:
  # tar_render(manuscript_pdf, here("manuscripts", "skyline_analysis.Rmd"))
)

