here::i_am("./run.R")
library(targets)
Sys.setenv(TZ = "GMT")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

tar_outdated()

# for debugging
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2")
lapply(v_pkgs, require, character.only = TRUE)
data_location <- "local drive"

# site, experiment, and dates to process:
site_id <- "HRG"
expt_id <- "diurnal1"
start_date <- "2023-05-20"
end_date   <- "2023-06-05"
initial_deadband_width <- 75
final_deadband_width   <- 20

# site, experiment, and dates to process:
site_id <- "EHD"
expt_id <- "digestate1"
start_date <- "2023-04-01"
end_date   <- "2023-04-04"
initial_deadband_width <- 150
final_deadband_width   <- 150

# site, experiment, and dates to process:
site_id <- "EHD"
expt_id <- "biochar1"
start_date <- "2021-05-01"
end_date   <- "2021-05-05"
initial_deadband_width <- 300
final_deadband_width   <- 250

# site, experiment, and dates to process:
site_id <- "EHD"
expt_id <- "split1"
start_date <- "2020-05-01"
end_date   <- "2020-05-05"
initial_deadband_width <- 150
final_deadband_width   <- 150

# site, experiment, and dates to process:
site_id <- "EHD"
expt_id <- "yield1"
start_date <- "2019-05-01"
end_date   <- "2019-05-05"
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

dt_ghg <- get_data(v_dates[1], site_id, expt_id, l_meta)
dt_unfilt <- remove_deadband(dt_ghg, method = "time fit", dryrun = TRUE)

dt_chi <- tar_read(dt_chi)
dt <- remove_deadband(dt_ghg, method = "time fit")
plot_data(dt)

# post-processing - separate script or give prefix?
dt_flux <- combine_fluxes(site_id, expt_id)
p_flux_co2 <- plot_flux(dt_flux, flux_name = "f_co2", 
      sigma_name = "sigma_f_co2", site_id, expt_id, 
      mult = 1, y_min = min(dt_flux$f_co2), y_max = max(dt_flux$f_co2))
# p_flux_ch4 <- plot_flux(dt_flux, flux_name = "f_ch4", 
      # sigma_name = "sigma_f_ch4", site_id, expt_id, 
      # mult = 1000, y_min = min(dt_flux$f_ch4), y_max = max(dt_flux$f_ch4))
p_flux_n2o <- plot_flux(dt_flux, flux_name = "f_n2o", 
      sigma_name = "sigma_f_n2o", site_id, expt_id, 
      mult = 1, y_min = min(dt_flux$f_n2o), y_max = max(dt_flux$f_n2o))

p_flux_n2o_with_Nappl <- plot_n2o_flux(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = "HRG", this_expt_id = "diurnal1", 
      l_meta, mult = 1, y_min = min(dt_flux$f_n2o), y_max = max(dt_flux$f_n2o))
p_flux_n2o_diurnal <- plot_n2o_flux_diurnal(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = "HRG", this_expt_id = "diurnal1", 
      mult = 1, y_min = min(dt_flux$f_n2o), y_max = max(dt_flux$f_n2o))
