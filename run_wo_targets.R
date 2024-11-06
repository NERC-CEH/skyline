here::i_am("./run.R")
library(targets)
Sys.setenv(TZ = "GMT")

# Run the R scripts in the R/ folder with your custom functions:
source("_targets.R")
tar_source()

tar_outdated()

# for debugging
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2",
           "lubridate", "dplyr", "future", "viridis", "lme4", "ggeffects",
           "photobiology", "mgcv", "ggpmisc", "fastmap")
lapply(v_pkgs, require, character.only = TRUE)

# # site, experiment, and dates to process:
# site_id <- "HRG"
# expt_id <- "diurnal1"
# start_date <- "2023-07-01" # "2023-05-04"
# end_date   <- "2023-07-04" # "2023-09-15"
# initial_deadband_width <- 75
# final_deadband_width   <- 20
#
# # site, experiment, and dates to process:
# site_id <- "EHD"
# expt_id <- "digestate1"
# start_date <- "2023-04-01" # "2023-03-16"
# end_date   <- "2023-04-04" # "2023-08-12"
# initial_deadband_width <- 150
# final_deadband_width   <- 150
#
# # site, experiment, and dates to process:
# site_id <- "EHD"
# expt_id <- "biochar1"
# start_date <- "2021-05-01" # "2021-04-30" # "2021-05-01"
# end_date   <- "2021-05-05" # "2021-06-14" # "2021-05-05"
# initial_deadband_width <- 300
# final_deadband_width   <- 250
#
# # site, experiment, and dates to process:
# site_id <- "EHD"
# expt_id <- "split1"
# start_date <- "2020-02-03" # "2020-01-24" # "2020-05-01"
# end_date   <- "2020-02-03" # "2020-08-11" # "2020-05-05"
# initial_deadband_width <- 150
# final_deadband_width   <- 150
#
# # site, experiment, and dates to process:
# site_id <- "EHD"
# expt_id <- "yield1"
# start_date <- "2019-03-06" # "2019-05-01"
# end_date   <- "2019-09-25" # "2019-05-05"
# initial_deadband_width <- 150
# final_deadband_width   <- 150
#
this_site_id <- site_id
this_expt_id <- expt_id
# example_date   <- as.POSIXct(start_date)
# example_date   <- as.POSIXct(end_date)
# this_date <- example_date
# v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))
# save_plots <- FALSE

fname_meta <- "data-raw/skyline_meta-data.xlsx"
l_meta <- read_metadata(fname_meta)

# l_files <- check_data_available(this_date, site_id, expt_id, l_meta)
# length(l_files$v_fnames_ghg)
# length(l_files$v_fnames_pos)
# length(l_files$v_fnames_met)

dt_chi <- get_data(v_dates, site_id, expt_id,
      l_meta,
      seq_id_to_plot = seq_id_to_plot,
      method = method, save_plots = save_plots,
      write_all = write_all, n_min = n_min)

dt_chi <- get_flux(dt_chi)

# pname_csv <- here("output", site_id, expt_id, "csv")
# pattern <- paste0(.Platform$file.sep, "dt_chi.*\\.csv$")
# v_fnames <- dir_ls(pname_csv, regexp = pattern)
# l_dt <- lapply(v_fnames, fread)
# dt_chi <- rbindlist(l_dt)

dt_flux <- get_flux(dt_chi)
dt_flux <- get_flux_condensed(dt_flux)
######### for testing different rmse thresholds
dt_flux <- fread(input=here("output", site_id, expt_id, "csv","dt_flux.csv"))
filter_fluxes <- function(dt, save_file = FALSE, fname = "dt_flux") {
  # remove days during experiment when no flux measurements
  dt <- dt[!is.na(site_id)]

  # # crude filtering of extreme outliers; units of umol/m2/s
  # # add thresholds as arguments
  dt <- dt[rmse_f_co2 < 5]
  # dt <- dt[f_co2 > -50 & f_co2 < 50]
  # dt <- dt[f_n2o > -0.1 & f_n2o < 0.1]
  # dt <- dt[rmse_f_n2o < 0.021]
  if (save_file) fwrite(dt, file = here("output", dt$site_id[1], dt$expt_id[1], paste0(fname, ".csv")))
  # if (save_file)  qsave(dt, file = here("output", paste0(fname, ".qs")))
  return(dt)
}
dt_flux_filtered <- filter_fluxes(dt_flux, save_file = TRUE, fname = "dt_flux2")
############


dts <- dt_chi[example_date == as.POSIXct(lubridate::date(datect)) &
  chamber_id == 2 &
  seq_id <= 24]

dts <- dt_chi[as.POSIXct("2021-05-03") == as.POSIXct(lubridate::date(datect)) &
   chamber_id == 11 &
  seq_id == 7]
table(dts$mmnt_id)
dts[mmnt_id == "2021-05-04_03_02"]
dts <- get_flux(dts)
dts[exclude == FALSE]

dt[concave_up == TRUE, coef(lm(form, w = w, data = .SD))[2], by = mmnt_id]
dt[concave_up == TRUE, dchi_dt := coef(lm(form, w = w, data = .SD))[2], by = mmnt_id]
dt[concave_up == TRUE, .(dchi_dt)]
summary(dts[exclude == FALSE])
any(is.na(dt[concave_up == TRUE, dchi_dt]))
any(is.na(dt[concave_up == FALSE, dchi_dt]))


plot_chi(dts[as.POSIXct("2021-05-04") == as.POSIXct(lubridate::date(datect))], gas_name = "co2")
dts <- get_gamdiff(dts)
plot_chi_co2_with_rmse(dts, n = 12)
plot_chi_with_gamdiff(dts[as.POSIXct("2021-05-02") == as.POSIXct(lubridate::date(datect))], n = 9)
plot_chi(dts, gas_name = "ch4")
plot_chi(dts, gas_name = "n2o")

dt_flux <- dts[exclude == FALSE, .SD[1], by = mmnt_id]
with(dt_flux, plot(gamdiff, sigma_lm))

dts <- dt_chi[example_date == as.POSIXct(lubridate::date(datect)) &
  chamber_id == 2 &
  seq_id <= 2]
dts <- get_flux(dts[exclude == FALSE])
plot_chi(dts[exclude == FALSE], gas_name = "co2")

dts <- dt_chi[example_date == as.POSIXct(lubridate::date(datect)) &
  chamber_id == 3 &
  seq_id <= 24]
dts[, start_t := which(exclude == FALSE, arr.ind=TRUE)[1], by = mmnt_id]
dts[, t := t - start_t]

dts <- get_flux(dts)
plot_chi(dts, gas_name = "co2")
dts[, .SD[1], by = mmnt_id]

dt_unfilt <- remove_deadband(dt_ghg, method = "time fit", dryrun = TRUE)

dt_chi <- tar_read(dt_chi)
dt <- remove_deadband(dt_ghg, method = "time fit")
plot_data(dt)

# post-processing - separate script or give prefix?
dt_flux <- combine_fluxes(site_id, expt_id)

p_flux_co2 <- plot_flux(dt_flux, flux_name = "f_co2",
      sigma_name = "sigma_f_co2", site_id, expt_id,
      mult = 1, y_min = -30, y_max = 30)
      # mult = 1, y_min = min(dt_flux$f_co2), y_max = max(dt_flux$f_co2))
p_flux_ch4 <- plot_flux(dt_flux, flux_name = "f_ch4",
      sigma_name = "sigma_f_ch4", site_id, expt_id,
      mult = 1000, y_min = min(dt_flux$f_ch4), y_max = max(dt_flux$f_ch4))
p_flux_n2o <- plot_flux(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", site_id, expt_id,
      mult = 1, y_min = -0.001, y_max = 0.01)
      # mult = 1, y_min = min(dt_flux$f_n2o), y_max = max(dt_flux$f_n2o))

p_flux_n2o_with_Nappl <- plot_n2o_flux(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = site_id, this_expt_id = expt_id,
      l_meta,
      mult = 1, y_min = -0.001, y_max = 0.025)
      # mult = 1, y_min = min(dt_flux$f_n2o), y_max = max(dt_flux$f_n2o))
p_flux_n2o_diurnal <- plot_n2o_flux_diurnal(dt_flux, flux_name = "f_n2o",
      sigma_name = "sigma_f_n2o", this_site_id = site_id, this_expt_id = expt_id,
      mult = 1, y_min = min(dt_flux$f_n2o), y_max = max(dt_flux$f_n2o))

p_flux_co2_vs_time <- plot_flux_vs_xvar(dt_flux, flux_name = "f_co2",
  sigma_name = "sigma_f_co2", xvar_name = "datect",
  colour_name = "trmt_id", facet_name = "chamber_id",
  site_id, expt_id,
  y_min = -30, y_max = 30)

p_flux_co2_vs_ppfd <- plot_flux_vs_xvar(dt_flux, flux_name = "f_co2",
  sigma_name = "sigma_f_co2", xvar_name = "PPFD_IN",
  colour_name = "chamber_id", facet_name = "trmt_id",
  site_id, expt_id,
  y_min = -30, y_max = 30)

p_flux_co2_vs_swc <- plot_flux_vs_xvar(dt_flux, flux_name = "f_co2",
  sigma_name = "sigma_f_co2", xvar_name = "SWC",
  colour_name = "trmt_id", facet_name = "chamber_id",
  site_id, expt_id,
  y_min = -30, y_max = 30)

p_flux_co2_vs_ta <- plot_flux_vs_xvar(dt_flux, flux_name = "f_co2",
  sigma_name = "sigma_f_co2", xvar_name = "TA",
  colour_name = "trmt_id", facet_name = "chamber_id",
  site_id, expt_id,
  y_min = -30, y_max = 30)

p_flux_n2o_vs_swc <- plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
  sigma_name = "sigma_f_n2o", xvar_name = "SWC",
  colour_name = "trmt_id", facet_name = "chamber_id",
  site_id, expt_id,
  y_min = -0.001, y_max = 0.025)

p_flux_n2o_vs_ta <- plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
  sigma_name = "sigma_f_n2o", xvar_name = "TA",
  colour_name = "trmt_id", facet_name = "chamber_id",
  site_id, expt_id,
  y_min = -0.001, y_max = 0.025)

p_flux_n2o_vs_fco2 <- plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
  sigma_name = "sigma_f_n2o", xvar_name = "f_co2",
  colour_name = "trmt_id", facet_name = "chamber_id",
  site_id, expt_id,
  y_min = -0.001, y_max = 0.025)

p_flux_n2o_vs_fh2o <- plot_flux_vs_xvar(dt_flux, flux_name = "f_n2o",
  sigma_name = "sigma_f_n2o", xvar_name = "f_h2o",
  colour_name = "trmt_id", facet_name = "chamber_id",
  site_id, expt_id,
  y_min = -0.001, y_max = 0.025)
