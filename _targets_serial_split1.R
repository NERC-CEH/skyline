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
  "photobiology",
  "mgcv",
  "ggpmisc",
  "fastmap"
)
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

seq_id_to_plot <- 1 # default to 1 as night/dark flux so should be clear if something is wrong with deadbands
save_plots <- TRUE # save plots for all gases containing every flux per chamber each day
write_all <- TRUE # combine files for days processed
diagnostic_plots <- TRUE

n_min <- 100
method <- "time fit" # "time fit" or "specified deadband only"
# dryrun <- FALSE  # FALSE = remove deadbands and calculate fluxes, TRUE = plots showing deadbands for visual checking before calculating fluxes

#### list of targets: ####

list(
  tar_target(
    fname_meta_xlsx,
    "data-raw/skyline_meta-data.xlsx",
    cue = tar_cue(mode = "always")
  ),

  tar_target(
    name = v_fname_meta_csv,
    format = "file",
    command = convert_metadata_csv(fname_meta_xlsx)
  ),

  tar_target(
    name = l_meta,
    command = read_metadata(v_fname_meta_csv)
  ),

  tar_target(
    name = dt_chi,
    command = get_data(
      v_dates,
      this_site_id = site_id,
      this_expt_id = expt_id,
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      diagnostic_plots = diagnostic_plots,
      method = method,
      save_plots = save_plots,
      write_all = write_all,
      n_min = n_min
    )
  ),

  tar_target(
    name = dt,
    command = get_flux(dt_chi)
  ),

  tar_target(
    name = null_1,
    command = fwrite(
      dt,
      file = paste0(
        here("output", dt[1, .(site_id, expt_id)]),
        "/dt_flux_unfilt.csv"
      )
    )
  ),

  tar_target(
    name = dt_flux,
    command = filter_fluxes2(
      dt,
      save_file = TRUE,
      fname = "dt_flux",
      rmse_threshold = 5
    )
  )
)
