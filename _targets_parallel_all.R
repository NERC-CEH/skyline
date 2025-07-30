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
v_pkgs = c(
  "here",
  "fs",
  "data.table",
  "readxl",
  "units",
  "qs",
  "ggplot2",
  "lubridate",
  "dplyr",
  "future",
  "viridis",
  "lme4",
  "ggeffects",
  # "mgcv", "ggpmisc")
  "photobiology",
  "mgcv",
  "ggpmisc"
)
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

seq_id_to_plot <- 5 # default to 1 as night/dark flux so should be clear if something is wrong with deadbands
save_plots <- TRUE # save plots for all gases containing every flux per chamber each day
write_all <- TRUE # combine files for days processes

n_min <- 100
method <- "time fit" # "time fit" or "specified deadband only"
# dryrun FALSE = remove deadbands and calculate fluxes, TRUE = plots showing deadbands for visual checking before calculating fluxes
dryrun <- FALSE

#### list of targets: ####
list(
  # an excel file containing all metadata; we track only the path name
  tar_target(fname_meta_xlsx, "data-raw/skyline_meta-data.xlsx"),

  # convert to csv files whose contents we track
  tar_target(
    name = v_fname_meta_csv,
    format = "file",
    command = convert_metadata_csv(fname_meta_xlsx)
  ),

  # read these csv into a list of data tables
  tar_target(
    name = l_meta,
    command = read_metadata(v_fname_meta_csv)
  ),

  tar_target(
    name = dt_chi_yield1,
    command = get_data(
      v_dates,
      this_site_id = "EHD",
      this_expt_id = "yield1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method,
      dryrun = dryrun,
      save_plots = save_plots,
      write_all = write_all,
      n_min = n_min
    )
  ),

  tar_target(
    name = dt_chi_split1,
    command = get_data(
      v_dates,
      this_site_id = "EHD",
      this_expt_id = "split1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method,
      dryrun = dryrun,
      save_plots = save_plots,
      write_all = write_all,
      n_min = n_min
    )
  ),

  tar_target(
    name = dt_chi_biochar1,
    command = get_data(
      v_dates,
      this_site_id = "EHD",
      this_expt_id = "biochar1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method,
      dryrun = dryrun,
      save_plots = save_plots,
      write_all = write_all,
      n_min = n_min
    )
  ),

  tar_target(
    name = dt_chi_digestate1,
    command = get_data(
      v_dates,
      this_site_id = "EHD",
      this_expt_id = "digestate1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method,
      dryrun = dryrun,
      save_plots = save_plots,
      write_all = write_all,
      n_min = n_min
    )
  ),

  tar_target(
    name = dt_chi_shading1,
    command = get_data(
      v_dates,
      this_site_id = "HRG",
      this_expt_id = "shading1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method,
      dryrun = dryrun,
      save_plots = save_plots,
      write_all = write_all,
      n_min = n_min
    )
  ),

  tar_target(
    name = dt_chi_mix1,
    command = get_data(
      v_dates,
      this_site_id = "PDF",
      this_expt_id = "mix1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method,
      dryrun = dryrun,
      save_plots = save_plots,
      write_all = write_all,
      n_min = n_min
    )
  ),

  tar_target(
    name = dt_chi_divine1,
    command = get_data(
      v_dates,
      this_site_id = "SHP",
      this_expt_id = "divine1",
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method,
      dryrun = dryrun,
      save_plots = save_plots,
      write_all = write_all,
      n_min = n_min
    )
  ),

  # take the mid-point day as an example to plot
  tar_target(
    name = example_date,
    command = dt_chi_biochar1[
      floor(dim(dt_chi_biochar1)[1] / 2),
      as.POSIXct(lubridate::date(datect))
    ]
  ),

  # plot concentration against time for every mmnt sequence that day
  tar_target(
    name = p_chi_co2_biochar1,
    command = plot_chi(
      dt_chi_biochar1[
        example_date == as.POSIXct(lubridate::date(datect))
      ],
      gas_name = "chi_co2"
    )
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
    name = dt_flux_shading1,
    command = get_flux(dt_chi_shading1)
  ),

  tar_target(
    name = dt_flux_mix1,
    command = get_flux(dt_chi_mix1)
  ),

  tar_target(
    name = dt_flux_divine1,
    command = get_flux(dt_chi_divine1)
  ),

  tar_target(
    name = dt_flux_all,
    command = rbindlist(
      list(
        dt_flux_biochar1,
        dt_flux_yield1,
        dt_flux_split1,
        dt_flux_digestate1,
        dt_flux_shading1,
        dt_flux_mix1,
        dt_flux_divine1
      ),
      fill = TRUE
    )
  ),

  tar_target(
    name = dt_flux,
    command = filter_fluxes(dt_flux_all, save_file = FALSE, fname = "dt_flux")
  ),

  tar_target(
    name = null_1,
    command = qsave(dt_flux, file = here("output/dt_flux.qs"))
  )
)
