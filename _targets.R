library(targets)
library(here)
library(qs)
Sys.setenv(TZ = "GMT")

# Set target options:
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2", 
  "lubridate")
tar_option_set(
  # envir = getNamespace("skyline"), use source code from installed package
  packages = v_pkgs,
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

data.table::setDTthreads(threads = 1)
data.table::getDTthreads()

data_location <- "local drive"
# site, experiment, and dates to process:
site_id <- "EHD"
expt_id <- "yield1"
seq_id_to_plot <- 6

# default to process all dates in experiment
v_dates <- NULL
# or uncomment lines below to specify a subset of dates
# start_date <- "2019-05-02" # "2023-04-01" # "2023-03-16" 
# end_date   <- "2019-05-04" # "2023-04-04" # "2023-08-12" 
# v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))
save_plots <- FALSE

# list of targets:
list(
  tar_target(fname_meta, "data-raw/skyline_meta-data.xlsx", format = "file"),
  tar_target(
    name = l_meta,
    command = read_metadata(fname_meta)
  ),
  tar_target(
    name = l_out,
    command = get_data(v_dates, site_id, expt_id, data_location, l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = "time fit", dryrun = TRUE, save_plots = save_plots)
  ),
  
  # take the first day as an example to plot
  tar_target(
    name = example_date,
    command = l_out$dt_chi[1, as.POSIXct(lubridate::date(datect))]
  ),
  # plot concentration against time for every mmnt sequence that day      
  tar_target(
    name = p_chi_co2,
    command = plot_chi(l_out$dt_chi[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "chi_co2")
  ),
  tar_target(
    name = p_chi_ch4,
    command = plot_chi(l_out$dt_chi[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "chi_ch4")
  ),
  tar_target(
    name = p_chi_h2o,
    command = plot_chi(l_out$dt_chi[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "chi_h2o")
  ),
  tar_target(
    name = p_chi_n2o,
    command = plot_chi(l_out$dt_chi[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "chi_n2o")
  ) # ,

  # # post-processing - separate script or give prefix?
  # tar_target(
    # name = dt_flux,
    # command = combine_fluxes(site_id, expt_id)
  # ),
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
    # name = p_flux_n2o_with_Nappl,
    # command = plot_n2o_flux(dt_flux, flux_name = "f_n2o",
      # sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "digestate1", 
      # l_meta, mult = 1000, y_min = -2, y_max = 10)
  # ),
  # tar_target(
    # name = p_flux_n2o_diurnal,
    # command = plot_n2o_flux_diurnal(dt_flux, flux_name = "f_n2o",
      # sigma_name = "sigma_f_n2o", this_site_id = "EHD", this_expt_id = "digestate1", 
      # mult = 1000, y_min = -2, y_max = 2.5)
  # )
)
