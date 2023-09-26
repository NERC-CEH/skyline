# define function to read Campbell Scientific TOA5 data
read_cs_data <- function(filename, return_option = "data"){
	if (return_option == "info") {
		# bring in entire header of CSI TOA5 data file for metadata
		info <- scan(file = filename, nlines = 4, what = character(), sep = "\r")
		return(info)
	} else {
		# second line of header contains variable names
		header <- scan(file = filename, skip = 1, nlines = 1, 
      what = character(), sep=",") 
		# bring in data
		df <- read.table(file = filename, skip = 4, header = FALSE, 
      na.strings = c("NAN"), sep = ",")
		names(df) <- header
		# add column of R-formatted date/timestamps
		df$TIMESTAMP <- as.POSIXct(strptime(df$TIMESTAMP, "%Y-%m-%d %H:%M:%S"))
    setDT(df)
		return(df)
  }
}

read_metadata <- function(fname_meta = "data-raw/skyline_meta-data.xlsx") {
  dt_site <- as.data.table(readxl::read_excel(fname_meta, sheet = "site"),      key=c("site_id"))
  dt_expt <- as.data.table(readxl::read_excel(fname_meta, sheet = "experiment"),key=c("site_id", "expt_id"))
  dt_trmt <- as.data.table(readxl::read_excel(fname_meta, sheet = "treatment"))
  dt_mgmt <- as.data.table(readxl::read_excel(fname_meta, sheet = "management_event"))
  dt_cham <- as.data.table(readxl::read_excel(fname_meta, sheet = "chamber"))
  dt_badd <- as.data.table(readxl::read_excel(fname_meta, sheet = "bad_data"))
  dt_ancl <- as.data.table(readxl::read_excel(fname_meta, sheet = "ancilliay_timeseries_byChamber"))

  dt_cham[, chamber_id := as.factor(chamber_id)]

  return(list(
    dt_site = dt_site,
    dt_expt = dt_expt,
    dt_trmt = dt_trmt,
    dt_mgmt = dt_mgmt,
    dt_cham = dt_cham,
    dt_badd = dt_badd,
    dt_ancl = dt_ancl
    )
  )
}

check_data_available <- function(this_date, this_site_id = "EHD", 
  this_expt_id = "digestate1", this_data_location = "local drive", l_meta) {
  # subset metadata to site, experiment and data_location
  dt <- l_meta$dt_expt[
    this_site_id == site_id & 
    this_expt_id == expt_id & 
    this_data_location == data_location]
  if (nrow(dt) != 1) stop(paste("Duplicate meta-data rows found in", fname_meta))
  
  # find the raw ghg files
  v_fnames <- dir_ls(dt[, path_to_ghg_data])
  v_fnames <- sort(v_fnames)
  
  v_dates <- substr(path_file(v_fnames), dt$time_start_ghg, dt$time_end_ghg)
  v_dates <- strptime(v_dates, dt$time_format_ghg, tz = "GMT")  
  v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates)))
  # some GHG data for this day may be in the last file from the previous day
  # so add this to the files read
  # do not do if this is the first file or if there are no files that day
  if (!is.na(v_ind[1]) & v_ind[1] > 1) v_ind <- c(v_ind[1] - 1, v_ind)
  v_fnames_ghg <- v_fnames[v_ind]
  
  # find the chamber position files
  v_fnames <- dir_ls(dt[, path_to_chamber_position_data])
  v_fnames <- sort(v_fnames)
  v_dates <- substr(path_file(v_fnames), dt$time_start_pos, dt$time_end_pos)
  v_dates <- strptime(v_dates, dt$time_format_pos, tz = "GMT")  
  v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates)))
  v_fnames_pos <- v_fnames[v_ind]
  
  # find the soil met files
  v_fnames <- dir_ls(dt[, path_to_soilmet_data])
  v_fnames <- sort(v_fnames) 
  v_dates <- substr(path_file(v_fnames), dt$time_start_met, dt$time_end_met)
  v_dates <- strptime(v_dates, dt$time_format_met, tz = "GMT")  
  v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates)))
  v_fnames_met <- v_fnames[v_ind]

  return(list(v_fnames_ghg = v_fnames_ghg, v_fnames_pos = v_fnames_pos, 
    v_fnames_met = v_fnames_met))
}

get_ghg_data <- function(v_fnames, this_date) {
  l_dt <- lapply(v_fnames, fread)
  dt_ghg <- rbindlist(l_dt)

  dt_ghg[, datect := as.POSIXct(EPOCH_TIME, origin = "1970-01-01")]
  dt_ghg[, datect := as.POSIXct(round(datect, "secs"))]
  # aggregate to 1 Hz i.e. do 1-sec averaging
  dt_ghg <- dt_ghg[, lapply(.SD, mean), .SDcols = c("CavityPressure", "CavityTemp", 
    "N2O_dry", "CO2_dry", "CH4_dry", "H2O"),  by = datect]
  # subset to this_date before returning
  dt_ghg <- dt_ghg[this_date == as.POSIXct(lubridate::date(datect))]
  return(dt_ghg)
}

get_ch_position_data <- function(v_fnames) {
  l_dt <- lapply(v_fnames, read_cs_data)
  dt <- rbindlist(l_dt)
  
  dt[, datect := as.POSIXct(round(TIMESTAMP, "secs"))]
  # rename position voltage consistently, depending whether sampled or averaged
  if ("C_Voltage_Avg" %in% names(dt)) {
    dt[, C_Voltage := C_Voltage_Avg]
    dt[, C_Voltage_Avg := NULL]
  }
  
  # aggregate to 1 Hz - prob not needed as 1 Hz anyway (but is it always?)
  dt <- dt[, lapply(.SD, mean), .SDcols = c("C_Voltage"),  by = datect]
  # convert chamber position voltage to chamber ID
  dt[, chamber_id := as.factor(round(C_Voltage * 0.01, 0))]  
  return(dt)
}

## WIP this works, but we want to rehape dt_met to long format, by datect and chamber_id
get_soilmet_data <- function(v_fnames) {
  l_dt <- lapply(v_fnames, read_cs_data)
  dt <- rbindlist(l_dt)
  
  dt[, datect := as.POSIXct(round(TIMESTAMP, "mins"))]
  dt[, TIMESTAMP := NULL]
  dt[, RECORD := NULL]
  return(dt)
}

get_data <- function(v_dates, this_site_id = "EHD", 
  this_expt_id = "digestate1", data_location, l_meta, 
  filter_deadband = TRUE, 
  initial_deadband_width = 150, final_deadband_width = 150,
  method = "time fit", dryrun = FALSE,
  save_plots = TRUE, write_all = FALSE) {
  # create directories for output
  pname_csv <- here("output", this_site_id, this_expt_id, "csv")
  pname_png <- here("output", this_site_id, this_expt_id, "png")
  fs::dir_create(pname_csv)
  fs::dir_create(pname_png)
  
  
  n_days <- length(v_dates)
  l_dt_chi  <- list()
  l_dt_flux <- list()
  for (i in seq_along(v_dates)) {
    this_date <- v_dates[i]
    l_files <- check_data_available(this_date, this_site_id, this_expt_id, data_location, l_meta)
    # if no data today, move on to next day
    if (length(l_files$v_fnames_ghg) == 0 | length(l_files$v_fnames_pos) == 0) next

    dt_ghg <- get_ghg_data(l_files$v_fnames_ghg, this_date)
    dt_pos <- get_ch_position_data(l_files$v_fnames_pos)
    # this works, but we want to rehape dt_met to long format, by datect and chamber_id
    # dt_met <- get_soilmet_data(l_files$v_fnames_met)

    dt <- dt_pos[dt_ghg, on = .(datect = datect), roll = TRUE]
    # remove where chamber_id data is missing
    dt <- dt[!is.na(chamber_id)]
    ## WIP I used the line below for 1 Hz ch pos data. Does the above rolling join work for both?
    # dt <- dt_ghg[dt_pos, on = .(datect = datect)]
    # dt <-  dt[dt_met, on = .(datect = datect)]

    # find unique mmnt_id from sequence of chamber_id
    dt[, seq_id  := rleid(chamber_id)] # enumerate the sequence
    # then enumerate the sequence for a given chamber
    dt[, seq_id := rleid(seq_id), by = chamber_id]
    dt[, mmnt_id := paste(round(datect, "day"), chamber_id, seq_id, sep = "_")]

    # enumerate the records within a mmnt sequence
    dt[, t := 1:.N, by = mmnt_id]
    dt[, n := .N, by = mmnt_id]
    dt <- dt[!is.na(H2O)] # subset to valid ghg data only

    # join with chamber data  
    dt_cham <- l_meta$dt_cham[this_site_id == site_id & this_expt_id == expt_id]
    dt_cham <- dt_cham[this_date >= start_date & this_date < end_date ]
    dt <- dt[dt_cham, on = .(chamber_id = chamber_id)]
    # remove where ghg data is missing
    dt <- dt[!is.na(datect)]
    # skip if chpos data is invalid - stuck on single value all day
    if (length(unique(dt$chamber_id)) < 2) next
    # remove deadband and plot
    if (filter_deadband) dt <- remove_deadband(dt, 
      initial_deadband_width = initial_deadband_width, 
      final_deadband_width = final_deadband_width, 
      method = method, dryrun = dryrun)
    # re-check how many data are left
    dt[, n_filt := .N, by = mmnt_id]
    # if too few data left (100?), remove the whole measurement sequence
    dt <- dt[n_filt > 100]
    
    # save to file and list
    fname <- paste0(pname_csv, "/dt_chi_", round(this_date, "day"), ".csv")
    fwrite(dt, file = fname)
    l_dt_chi[[i]] <- dt
    
    if (save_plots) {
      p <- plot_chi(dt, gas_name = "H2O")    
      fname <- paste0(pname_png, "/h2o_", round(this_date, "day"), ".png")
      ggsave(p, file = fname)
      p <- plot_chi(dt, gas_name = "CO2_dry")    
      fname <- paste0(pname_png, "/co2_", round(this_date, "day"), ".png")
      ggsave(p, file = fname)
      p <- plot_chi(dt, gas_name = "CH4_dry")    
      fname <- paste0(pname_png, "/ch4_", round(this_date, "day"), ".png")
      ggsave(p, file = fname)
      p <- plot_chi(dt, gas_name = "N2O_dry")    
      fname <- paste0(pname_png, "/n2o_", round(this_date, "day"), ".png")
      ggsave(p, file = fname)
    }
    # calculate fluxes each day    
    dt <- calc_flux(dt, gas_name = "H2O")
    dt <- calc_flux(dt, gas_name = "CO2_dry")
    dt <- calc_flux(dt, gas_name = "CH4_dry")
    dt <- calc_flux(dt, gas_name = "N2O_dry")
    # subset to just the first record 
    dt_flux <- dt[, .SD[1], by = mmnt_id]

    # save to file and list
    fname <- paste0(pname_csv, "/dt_flux_", round(this_date, "day"), ".csv")
    fwrite(dt_flux, file = fname)
    l_dt_flux[[i]] <- dt_flux
  }
  dt_chi  <- rbindlist(l_dt_chi)
  dt_flux <- rbindlist(l_dt_flux)

  if (write_all) {
    # save to files
    fname <- paste0(pname_csv, "/dt_chi_", round(v_dates[1], "day"), "_", 
      round(v_dates[n_days], "day"), ".csv")
    fwrite(dt_chi, file = fname)
    fname <- paste0(pname_csv, "/dt_flux_", round(v_dates[1], "day"), "_", 
      round(v_dates[n_days], "day"), ".csv")
    fwrite(dt_flux, file = fname)
  }
  return(list(dt_chi = dt_chi, dt_flux = dt_flux))
}

# dt <- remove_deadband(dt, method = "time fit")
remove_deadband <- function(dt, initial_deadband_width = 150, final_deadband_width = 150,
  method = c("time fit", "specified deadband only"), dryrun = FALSE) {
  dt[, exclude := FALSE]
  # add mmnt-specific latter deadband
  dt[, start_final_deadband := n - final_deadband_width, by = mmnt_id]

  # method <- match.arg()
  if (method == "specified deadband only") {
    dt[t < initial_deadband_width | t > start_final_deadband, exclude := TRUE]
  } else if (method == "time fit") {
    # x <- seq(0, 1, 0.05)
    # w <- dbeta(x, shape1 = 1.5, shape2 = 1.5)
    # plot(x, w)
    dt[, w := dbeta(t/n, shape1 = 1.5, shape2 = 1.5)]
    dt[t < initial_deadband_width | t > start_final_deadband, w := 0]
    # ggplot(dt, aes(t, w, colour = mmnt_id)) + geom_point()
  
    # predict time based on all GHG concentrations
    # and use this to filter out the nonlinear part
    if (length(unique(dt$mmnt_id)) > 1) {
      form <- formula(t ~ CO2_dry + CH4_dry + N2O_dry + H2O)
      # very slow on JASMIN: system.time(m <- lm(t ~ CO2_dry + CH4_dry + N2O_dry + H2O + mmnt_id, w = w, data = dt))
      dt[, t_pred := predict(lm(form, w = w, data = .SD)), by = mmnt_id]
      dt[, t_resid := abs(scale(resid(lm(form, w = w, data = .SD)))), by = mmnt_id]
    } else { ## WIP would the above fail with only one mmnt_id? just in case:
      dt[, t_pred := predict(lm(form, w = w, data = .SD))]
      dt[, t_resid := abs(scale(resid(lm(form, w = w, data = .SD))))]
    }
    dt[t/n > 0.25 & t/n < 0.75, t_resid := 0]
    # set the exclusion criteria
    dt[t < initial_deadband_width | t > start_final_deadband |
      t_resid > 1, exclude := TRUE]
  }
  # unless it is a dryrun, subset the data
  if (!dryrun) dt <- dt[exclude == FALSE]
  return(dt)
}

plot_data_unfiltered <- function(dt_unfilt, initial_deadband_width = 150, final_deadband_width = 150) {
  dt1 <- dt_unfilt[seq_id == 1]
  dt_sfdband <- dt1[, .(start_final_deadband = .SD[1, start_final_deadband]), by = mmnt_id] 
  p <- ggplot(dt1, aes(t, CO2_dry, colour = exclude)) 
  p <- p + geom_point(aes(size = t_resid))
  p <- p + facet_wrap(~ mmnt_id) + xlim(0, NA)
  p <- p + geom_vline(xintercept = initial_deadband_width)
  p <- p + geom_vline(data = dt_sfdband, aes(xintercept = start_final_deadband))
p
  return(p)
}
  
plot_chi <- function(dt, gas_name = "N2O_dry", initial_deadband_width = 150, final_deadband_width = 150) {
  p <- ggplot(dt, aes(t, get(gas_name), colour = as.factor(seq_id), group = mmnt_id)) 
  p <- p + geom_point(alpha = 0.1) ## WIP setting alpha adds computation time - try without
  p <- p + xlim(0, NA) + ylab(gas_name)
  p <- p + stat_smooth(method = "lm")
  p <- p + facet_wrap(~ chamber_id)

  # p <- ggplot(dt, aes(datect, C_Voltage, colour = mmnt_id)) + geom_point()
  # p <- ggplot(dt, aes(datect, t, colour = mmnt_id)) + geom_point()
  # p <- ggplot(dt, aes(t, CH4_dry, colour = mmnt_id)) + geom_point()
  # p <- p + facet_wrap(~ mmnt_id)
  # p
  
  # p <- ggplot(dt, aes(t, t_pred, colour = mmnt_id)) + geom_point()
  # p <- ggplot(dt[abs(t_resid) < 1000], aes(t, t_resid, colour = mmnt_id)) + geom_point()
  # # p <- p + geom_abline()
  # p <- p + facet_wrap(~ mmnt_id)
  # p
  # p <- ggplot(dt, aes(t, N2O_dry, colour = as.factor(seq_id))) + geom_point()
  # p <- ggplot(dt, aes(t, CH4_dry, colour = as.factor(seq_id))) + geom_point()
  return(p)
}

calc_flux <- function(dt, gas_name = "CO2_dry", use_STP = TRUE, PA = 1000, TA = 15) {
  form <- formula(paste(gas_name, "~ t"))
  flux_var_name <- paste0("f_", gas_name)
  sigma_var_name <- paste0("sigma_", gas_name)
  dt[, dchi_dt := coef(lm(form, data = .SD))[2], by = mmnt_id]
  dt[, sigma_dchi_dt := summary(lm(form, data = .SD))$coefficients[2, 2], by = mmnt_id]
  if (use_STP) {
    rho <- PA * 100 / (8.31447 * (TA + 273.15))
  } else {
    # calculate mean air density for each mmnt if we have the specific data
    dt[, rho := PA * 100 / (8.31447 * (TA + 273.15))]
  }
  dt[, (flux_var_name) := dchi_dt        * rho * volume_m3 / area_m2]
  dt[, (sigma_var_name) := sigma_dchi_dt * rho * volume_m3 / area_m2]

  # remove un-needed variables
  dt[, dchi_dt := NULL]
  dt[, sigma_dchi_dt := NULL]

  return(dt)
}

calc_flux_daily <- function(dt, gas_name = "CO2_dry", use_STP = TRUE, PA = 1000, TA = 15) {
  form <- formula(paste(gas_name, "~ t"))
  flux_var_name <- paste0("f_", gas_name)
  sigma_var_name <- paste0("sigma_", gas_name)
  dt[, dchi_dt := coef(lm(form, data = .SD))[2], by = mmnt_id]
  dt[, sigma_dchi_dt := summary(lm(form, data = .SD))$coefficients[2, 2], by = mmnt_id]
  if (use_STP) {
    rho <- PA * 100 / (8.31447 * (TA + 273.15))
  } else {
    # calculate mean air density for each mmnt if we have the specific data
    dt[, rho := PA * 100 / (8.31447 * (TA + 273.15))]
  }
  dt[, (flux_var_name) := dchi_dt        * rho * volume_m3 / area_m2]
  dt[, (sigma_var_name) := sigma_dchi_dt * rho * volume_m3 / area_m2]
  
  # subset to just the first record 
  dt <- dt[, .SD[1], by = mmnt_id]
  return(dt)
}

combine_fluxes <- function(site_id, expt_id) {
  pname_csv <- here("output", site_id, expt_id, "csv")
  pattern <- paste0(.Platform$file.sep, "dt_flux.*\\.csv$")
  v_fnames <- dir_ls(pname_csv, regexp = pattern)
  l_dt <- lapply(v_fnames, fread)
  dt <- rbindlist(l_dt)
  return(dt)
}

plot_flux <- function(dt_flux, flux_name = "f_N2O_dry",
  sigma_name = "sigma_N2O_dry", site_id, expt_id, 
  mult = 1, y_min = NA, y_max = NA) {
  
  dt_flux[, f     := get(flux_name) * mult]
  dt_flux[, sigma := get(sigma_name) * mult]
  dt_flux[, ci_lo := f - (sigma * 1.96)]
  dt_flux[, ci_hi := f + (sigma * 1.96)]

  p <- ggplot(dt_flux, aes(datect, f, colour = as.factor(chamber_id)))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_point()
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi))
  p <- p + facet_wrap(~ trmt_id)
  p <- p + ylab(flux_name)
  p <- p + ylim(y_min, y_max)
  p
  
  fname <- here("output", site_id, expt_id,
    paste0(flux_name, ".png"))
  ggsave(p, file = fname)
  
  return(p)
}

plot_n2o_flux <- function(dt_flux, flux_name = "f_N2O_dry",
  sigma_name = "sigma_N2O_dry", this_site_id = "EHD", this_expt_id = "digestate1", 
  l_meta, mult = 1000, y_min = -2, y_max = 10) {
  
  l_meta$dt_mgmt
    # subset metadata to site, experiment and data_location
  dt_mgmt <- l_meta$dt_mgmt[
    this_site_id == site_id & 
    this_expt_id == expt_id]
  names(dt_mgmt) <- make.names(names(dt_mgmt))
  str(dt_mgmt)

  dt_flux[, f     := get(flux_name) * mult]
  dt_flux[, sigma := get(sigma_name) * mult]
  dt_flux[, ci_lo := f - (sigma * 1.96)]
  dt_flux[, ci_hi := f + (sigma * 1.96)]

  p <- ggplot(dt_flux, aes(datect, f))
  p <- p + geom_vline(data = dt_mgmt, aes(xintercept = start))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_point(aes(colour = as.factor(chamber_id)))
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, colour = as.factor(chamber_id)))
  p <- p + facet_wrap(~ trmt_id)
  p <- p + ylab(flux_name)
  p <- p + ylim(y_min, y_max)
  p
  
  
  fname <- here("output", site_id, expt_id,
    paste0(flux_name, "_with_Nappl.png"))
  ggsave(p, file = fname)
  
  return(p)
}


plot_n2o_flux_diurnal <- function(dt_flux, flux_name = "f_N2O_dry",
  sigma_name = "sigma_N2O_dry", this_site_id = "EHD", this_expt_id = "digestate1", 
  mult = 1000, y_min = -2, y_max = 2.5) {
  
  dt_flux[, f     := get(flux_name) * mult]
  dt_flux[, sigma := get(sigma_name) * mult]
  dt_flux[, ci_lo := f - (sigma * 1.96)]
  dt_flux[, ci_hi := f + (sigma * 1.96)]
  dt_flux[, h := lubridate::hour(datect) + lubridate::minute(datect)/60]

  p <- ggplot(dt_flux, aes(h, f))
  p <- p + geom_hline(yintercept = 0)
  # p <- p + geom_point(aes(colour = as.factor(chamber_id)))
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), 
    colour = "light yellow")
   p <- p + stat_smooth()
  # p <- p + facet_wrap(~ trmt_id)
  p <- p + ylab(flux_name)
  p <- p + ylim(y_min, y_max)
  p
  
  
  fname <- here("output", site_id, expt_id,
    paste0(flux_name, "_diurnal.png"))
  ggsave(p, file = fname)
  
  return(p)
}


# get_ghg_data <- function(this_date, this_site_id = "EHD", 
  # this_expt_id = "digestate1", this_data_location = "local drive", l_meta) {
  # # subset metadata to site, experiment and data_location
  # dt <- l_meta$dt_expt[
    # this_site_id == site_id & 
    # this_expt_id == expt_id & 
    # this_data_location == data_location]
    
  # # find the raw chi files
  # v_fnames <- dir_ls(dt[, path_to_ghg_data])
  # v_fnames <- sort(v_fnames)
  
  # v_dates_ghg <- substr(path_file(v_fnames), 12, 26)
  # v_dates_ghg <- strptime(v_dates_ghg, "%Y%m%d-%H%M%S", tz = "GMT")  
  # v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates_ghg)))
  # if (length(v_ind) < 1) stop(paste("No GHG files on", this_date))
  # # some data for this day may be in the last file from the previous day
  # # so add this to the files read; do not do on first day
  # if (v_ind[1] > 1) v_ind <- c(v_ind[1] - 1, v_ind)
  # v_fnames[v_ind]
  # l_dt <- lapply(v_fnames[v_ind], fread)
  # dt_ghg <- rbindlist(l_dt)

  # dt_ghg[, datect := as.POSIXct(EPOCH_TIME, origin = "1970-01-01")]
  # dt_ghg[, datect := as.POSIXct(round(datect, "secs"))]
  # # aggregate to 1 Hz i.e. do 1-sec averaging
  # dt_ghg <- dt_ghg[, lapply(.SD, mean), .SDcols = c("CavityPressure", "CavityTemp", 
    # "N2O_dry", "CO2_dry", "CH4_dry", "H2O"),  by = datect]
  # # subset to this_date before returning
  # dt_ghg <- dt_ghg[this_date == as.POSIXct(lubridate::date(datect))]
  # return(dt_ghg)
# }


# get_ch_position_data <- function(this_date, this_site_id = "EHD", 
  # this_expt_id = "digestate1", this_data_location = data_location, l_meta) {
  # # subset metadata to site, experiment and data_location
  # dt <- l_meta$dt_expt[
    # this_site_id == site_id & 
    # this_expt_id == expt_id & 
    # this_data_location == data_location]

  # # find the raw chpos files
  # v_fnames <- dir_ls(dt[, path_to_chamber_position_data])
  # v_fnames <- sort(v_fnames)
  
  # v_dates_ghg <- substr(path_file(v_fnames), 21, 35)
  # v_dates_ghg <- strptime(v_dates_ghg, "%Y_%m_%d_%H%M", tz = "GMT")  
  # v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates_ghg)))
  # # some data for this day may be in the last file from the previous day
  # # so add this to the files read; do not do on first day
  # #if (v_ind[1] > 1) v_ind <- c(v_ind[1] - 1, v_ind)
  # dt <- read_cs_data(v_fnames[v_ind])
  # l_dt <- lapply(v_fnames[v_ind], read_cs_data)
  # dt <- rbindlist(l_dt)
  
  # dt[, datect := as.POSIXct(round(TIMESTAMP, "secs"))]
  # # aggregate to 1 Hz i.e. do 1-sec averaging
  # dt <- dt[, lapply(.SD, mean), .SDcols = c("C_Voltage"),  by = datect]
  # # subset to this_date before returning
  # dt <- 
  # # dt[this_date == as.POSIXct(round(datect, "days"))]
  # dt[, chamber_id := as.factor(round(C_Voltage * 0.01, 0))]  
  # return(dt)
# }

# ## WIP this works, but we want to rehape dt_met to long format, by datect and chamber_id
# get_soilmet_data <- function(this_date = v_dates[1], this_site_id = "EHD", 
  # this_expt_id = "digestate1", this_data_location = data_location, l_meta) {
  # # subset metadata to site, experiment and data_location
  # dt <- l_meta$dt_expt[
    # this_site_id == site_id & 
    # this_expt_id == expt_id & 
    # this_data_location == data_location]

  # # find the raw chpos files
  # v_fnames <- dir_ls(dt[, path_to_soilmet_data])
  # v_fnames <- sort(v_fnames)

  # v_dates_ghg <- substr(path_file(v_fnames), 40, 49)
  # v_dates_ghg <- strptime(v_dates_ghg, "%Y_%m_%d", tz = "GMT")  
  # v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates_ghg)))
  # # some data for this day may be in the last file from the previous day
  # # so add this to the files read; do not do on first day
  # #if (v_ind[1] > 1) v_ind <- c(v_ind[1] - 1, v_ind)
  # dt <- read_cs_data(v_fnames[v_ind])
  # l_dt <- lapply(v_fnames[v_ind], read_cs_data)
  # dt <- rbindlist(l_dt)
  
  # dt[, datect := as.POSIXct(round(TIMESTAMP, "mins"))]
  # dt[, TIMESTAMP := NULL]
  # dt[, RECORD := NULL]
  # return(dt)
# }

# join_fluxes <- function(dt_1, dt_2) {
  # common_names <- intersect(names(dt_1), names(dt_2))
  # # then, use setdiff to find the column names that are found in 'dt_2' 
  # # and not in the 'common_names' while including the joining column 'mmnt_id'

  # new_names <- c(setdiff(names(dt_2), common_names), "mmnt_id")
  # # Now, we do the join

  # dt <- 
  # dt_1[dt_2[, ..new_names], on = .(mmnt_id), nomatch = 0]
  # dt_1[dt_2[, ..new_names], on = .(mmnt_id), nomatch = 0]
  # return(dt)
# }
