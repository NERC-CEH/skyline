here::i_am("./run.R")
library(targets)
Sys.setenv(TZ = "GMT")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

tar_outdated()

# for debugging
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2")
lapply(v_pkgs, require, character.only = TRUE)

# site, experiment, and dates to process:
data_location <- "local drive"
site_id <- "EHD"
expt_id <- "digestate1"
start_date <- "2023-04-01"
end_date   <- "2023-04-04"
initial_deadband_width <- 150
final_deadband_width   <- 150

# site, experiment, and dates to process:
data_location <- "local drive"
site_id <- "EHD"
expt_id <- "biochar1"
start_date <- "2021-05-01"
end_date   <- "2021-05-05"
initial_deadband_width <- 300
final_deadband_width   <- 250

# site, experiment, and dates to process:
data_location <- "local drive"
site_id <- "EHD"
expt_id <- "split1"
start_date <- "2020-05-01"
end_date   <- "2020-05-05"
initial_deadband_width <- 150
final_deadband_width   <- 150

example_date   <- as.POSIXct(start_date)
v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))
save_plots <- FALSE
this_date <- example_date

fname_meta <- "data-raw/skyline_meta-data.xlsx"
l_meta <- read_metadata(fname_meta)

l_files <- check_data_available(this_date, site_id, expt_id, data_location, l_meta)
length(l_files$v_fnames_ghg)
length(l_files$v_fnames_pos)
length(l_files$v_fnames_met)

dt_ghg <- get_data(v_dates[1], site_id, expt_id, data_location, l_meta)
dt_unfilt <- remove_deadband(dt_ghg, method = "time fit", dryrun = TRUE)

dt_chi <- tar_read(dt_chi)
dt <- remove_deadband(dt_ghg, method = "time fit")
plot_data(dt)

# post-processing - separate script or give prefix?
dt_flux <- combine_fluxes(site_id, expt_id)
p_flux_co2 <- plot_flux(dt_flux, flux_name = "f_CO2_dry", 
      sigma_name = "sigma_CO2_dry", site_id, expt_id, 
      mult = 1, y_min = -25, y_max = 25)
p_flux_ch4 <- plot_flux(dt_flux, flux_name = "f_CH4_dry", 
      sigma_name = "sigma_CH4_dry", site_id, expt_id, 
      mult = 1000, y_min = -5, y_max = 5)
p_flux_n2o <- plot_flux(dt_flux, flux_name = "f_N2O_dry", 
      sigma_name = "sigma_N2O_dry", site_id, expt_id, 
      mult = 1000, y_min = -2, y_max = 10)
p_flux_n2o_with_Nappl <- plot_n2o_flux(dt_flux, flux_name = "f_N2O_dry",
      sigma_name = "sigma_N2O_dry", this_site_id = "EHD", this_expt_id = "split1", 
      l_meta, mult = 1000, y_min = -2, y_max = 10)
p_flux_n2o_diurnal <- plot_n2o_flux_diurnal(dt_flux, flux_name = "f_N2O_dry",
      sigma_name = "sigma_N2O_dry", this_site_id = "EHD", this_expt_id = "split1", 
      mult = 1000, y_min = -2, y_max = 2.5)
