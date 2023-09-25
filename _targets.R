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
site_id <- "EH"
expt_id <- "digestate1"
start_date <- "2023-03-28"
end_date   <- "2023-08-31"
example_date   <- "2023-04-04"
v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))
save_plots <- FALSE

# Replace the target list below with your own:
list(
  tar_target(fname_meta, "data-raw/skyline_meta-data.xlsx", format = "file"),
  tar_target(
    name = l_meta,
    command = read_metadata(fname_meta)
  ),
  tar_target(
    name = dt_chi,
    command = get_data(v_dates, site_id, expt_id, data_location, l_meta, 
      save_plots = save_plots)
  ),
  tar_target(
    name = dt_unfilt,
    command = remove_deadband(dt_chi[
      example_date == as.POSIXct(lubridate::date(datect))], 
      method = "time fit", dryrun = TRUE)
  ),
  tar_target(
    name = p_unfilt,
    command = plot_data_unfiltered(dt_unfilt)
  ),
  tar_target(
    name = dt_filt,
    command = remove_deadband(dt_chi, method = "time fit")
  ),
  tar_target(
    name = p_chi_co2,
    command = plot_chi(dt_filt[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "CO2_dry")
  ),
  tar_target(
    name = p_chi_ch4,
    command = plot_chi(dt_filt[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "CH4_dry")
  ),
  tar_target(
    name = p_chi_n2o,
    command = plot_chi(dt_filt[
      example_date == as.POSIXct(lubridate::date(datect))], 
      gas_name = "N2O_dry")
  ),
  tar_target(
    name = dt_flux1,
    command = calc_flux(dt_filt, gas_name = "H2O")
  ),
  tar_target(
    name = dt_flux2,
    command = calc_flux(dt_filt, gas_name = "CO2_dry")
  ),
  tar_target(
    name = dt_flux3,
    command = calc_flux(dt_filt, gas_name = "CH4_dry")
  ),
  tar_target(
    name = dt_flux4,
    command = calc_flux(dt_filt, gas_name = "N2O_dry")
  ),
  tar_target(
    name = dt_fluxa,
    command = join_fluxes(dt_flux1, dt_flux2)
  ),
  tar_target(
    name = dt_fluxb,
    command = join_fluxes(dt_fluxa, dt_flux3)
  ),
  tar_target(
    name = dt_flux,
    command = join_fluxes(dt_fluxb, dt_flux4)
  ),
  tar_target(
    name = p_flux_co2,
    command = plot_flux(dt_flux, flux_name = "f_CO2_dry", 
      sigma_name = "sigma_CO2_dry", site_id, expt_id)
  ),
  tar_target(
    name = p_flux_ch4,
    command = plot_flux(dt_flux, flux_name = "f_CH4_dry", 
      sigma_name = "sigma_CH4_dry", site_id, expt_id)
  ),
  tar_target(
    name = p_flux_n2o,
    command = plot_flux(dt_flux, flux_name = "f_N2O_dry", 
      sigma_name = "sigma_N2O_dry", site_id, expt_id)
  )
)
