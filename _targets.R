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
start_date <- "2023-04-04"
end_date   <- start_date
v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))

# Replace the target list below with your own:
list(
  tar_target(fname_meta, "data-raw/DIVINE_meta-data_AgZero.xlsx", format = "file"),
  tar_target(
    name = l_meta,
    command = read_metadata(fname_meta)
  ),
  tar_target(
    name = dt_ghg,
    command = get_data(v_dates[1], site_id, expt_id, data_location, l_meta)
  ),
  tar_target(
    name = dt_unfilt,
    command = remove_deadband(dt_ghg, method = "time fit", dryrun = TRUE)
  ),
  tar_target(
    name = p_unfilt,
    command = plot_data_unfiltered(dt_unfilt)
  ),
  tar_target(
    name = dt,
    command = remove_deadband(dt_ghg, method = "time fit")
  ),
  tar_target(
    name = p_ghg,
    command = plot_data(dt)
  ),
  tar_target(
    name = dt_flux,
    command = process_data(v_dates, dt_ghg)
  )
)
