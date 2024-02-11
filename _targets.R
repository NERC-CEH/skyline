library(targets)
library(here)
library(qs)
Sys.setenv(TZ = "GMT")

# Set target options:
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2",
  "lubridate", "dplyr")
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
data_location <- "local drive"  # local or network drive
# site, experiment, and dates to process:
# site_id <- "EHD"
# expt_id <- "digestate1"

# default to process all dates in experiment
v_dates <- NULL
# or uncomment lines below to specify a subset of dates
# start_date <-  "2023-05-11"
# end_date   <-  "2023-08-12"
# v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))

seq_id_to_plot <- 1   # default to 1 as night/dark flux so should be clear if something is wrong with deadbands
save_plots <- TRUE   # save plots for all gases containing every flux per chamber each day
write_all <- TRUE   # combine files for days processes

min_dp <- 100
method <-  "time fit"  # "time fit" or "specified deadband only"
# dryrun FALSE = remove deadbands and calculate fluxes, TRUE = plots showing deadbands for visual checking before calculating fluxes
dryrun <- TRUE  

#### list of targets: ####
list(
  tar_target(fname_meta, "data-raw/skyline_meta-data.xlsx", format = "file"),
  tar_target(
    name = l_meta,
    command = read_metadata(fname_meta)
  ),

  tar_target(
    name = l_out_yield1,
    command = get_data(v_dates, this_site_id = "EHD", this_expt_id = "yield1", 
      data_location, l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots, 
      write_all = write_all, min_dp = min_dp)
  ),

  tar_target(
    name = l_out_split1,
    command = get_data(v_dates, this_site_id = "EHD", this_expt_id = "split1", 
      data_location, l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots, 
      write_all = write_all, min_dp = min_dp)
  ),

  tar_target(
    name = l_out_biochar1,
    command = get_data(v_dates, this_site_id = "EHD", this_expt_id = "biochar1", 
      data_location, l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots, 
      write_all = write_all, min_dp = min_dp)
  ),

  tar_target(
    name = l_out_digestate1,
    command = get_data(v_dates, this_site_id = "EHD", this_expt_id = "digestate1", 
      data_location, l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots, 
      write_all = write_all, min_dp = min_dp)
  ),
      
  tar_target(
    name = l_out_diurnal1,
    command = get_data(v_dates, this_site_id = "HRG", this_expt_id = "diurnal1", 
      data_location, l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, dryrun = dryrun, save_plots = save_plots, 
      write_all = write_all, min_dp = min_dp)
  )
)
