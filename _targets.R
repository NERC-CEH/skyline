library(targets)
Sys.setenv(TZ = "GMT")

# Set target options:
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2")
tar_option_set(
  packages = v_pkgs,
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# site, experiment, and dates to process:
data_location <- "local drive"
site_id <- "EHD"
expt_id <- "split1"
start_date <- "2020-01-24"
end_date   <- "2020-08-11"
initial_deadband_width <- 150
final_deadband_width   <- 150

example_date   <- as.POSIXct("2020-05-02")
v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))
save_plots <- TRUE

# Replace the target list below with your own:
list(
  tar_target(fname_meta, "data-raw/skyline_meta-data.xlsx", format = "file"),
  tar_target(
    name = l_meta,
    command = read_metadata(fname_meta)
  ),
  tar_target(
    name = l_out,
    command = get_data(v_dates, site_id, expt_id, data_location, l_meta,
      initial_deadband_width = initial_deadband_width, 
      final_deadband_width = final_deadband_width,
      method = "time fit", dryrun = FALSE, save_plots = save_plots)
  ),
  tar_target(
    name = l_out_example,
    command = get_data(example_date, site_id, expt_id, data_location, l_meta, 
      initial_deadband_width = initial_deadband_width, 
      final_deadband_width = final_deadband_width,
      method = "time fit", dryrun = TRUE, save_plots = save_plots)
  ),
  tar_target(
    name = dt_unfilt,
    command = remove_deadband(l_out_example$dt_chi, 
      initial_deadband_width = initial_deadband_width, 
      final_deadband_width = final_deadband_width,
      method = "time fit", dryrun = TRUE)
  ),
  tar_target(
    name = p_unfilt,
    command = plot_data_unfiltered(dt_unfilt, 
      initial_deadband_width = initial_deadband_width, 
      final_deadband_width = final_deadband_width, this_seq_id = 4)
  ),
  tar_target(
    name = p_chi_co2,
    command = plot_chi(l_out$dt_chi[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "CO2_dry")
  ),
  tar_target(
    name = p_chi_ch4,
    command = plot_chi(l_out$dt_chi[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "CH4_dry")
  ),
  tar_target(
    name = p_chi_n2o,
    command = plot_chi(l_out$dt_chi[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "N2O_dry")
  ) # ,
  # # post-processing - separate script or give prefix?
  # tar_target(
    # name = dt_flux,
    # command = combine_fluxes(site_id, expt_id)
  # ),
  # tar_target(
    # name = p_flux_co2,
    # command = plot_flux(dt_flux, flux_name = "f_CO2_dry", 
      # sigma_name = "sigma_CO2_dry", site_id, expt_id, 
      # mult = 1, y_min = -25, y_max = 25)
  # ),
  # tar_target(
    # name = p_flux_ch4,
    # command = plot_flux(dt_flux, flux_name = "f_CH4_dry", 
      # sigma_name = "sigma_CH4_dry", site_id, expt_id, 
      # mult = 1000, y_min = -5, y_max = 5)
  # ),
  # tar_target(
    # name = p_flux_n2o,
    # command = plot_flux(dt_flux, flux_name = "f_N2O_dry", 
      # sigma_name = "sigma_N2O_dry", site_id, expt_id, 
      # mult = 1000, y_min = -2, y_max = 10)
  # ),
  # tar_target(
    # name = p_flux_n2o_with_Nappl,
    # command = plot_n2o_flux(dt_flux, flux_name = "f_N2O_dry",
      # sigma_name = "sigma_N2O_dry", this_site_id = "EHD", this_expt_id = "digestate1", 
      # l_meta, mult = 1000, y_min = -2, y_max = 10)
  # ),
  # tar_target(
    # name = p_flux_n2o_diurnal,
    # command = plot_n2o_flux_diurnal(dt_flux, flux_name = "f_N2O_dry",
      # sigma_name = "sigma_N2O_dry", this_site_id = "EHD", this_expt_id = "digestate1", 
      # mult = 1000, y_min = -2, y_max = 2.5)
  # )
)
