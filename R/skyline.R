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

read_metadata <- function(fname_meta = "data-raw/DIVINE_meta-data_AgZero.xlsx") {
  dt_site <- as.data.table(readxl::read_excel(fname_meta, sheet = "site"),      key=c("site_id"))
  dt_expt <- as.data.table(readxl::read_excel(fname_meta, sheet = "experiment"),key=c("site_id", "expt_id"))
  dt_trmt <- as.data.table(readxl::read_excel(fname_meta, sheet = "treatment"))
  dt_mngt <- as.data.table(readxl::read_excel(fname_meta, sheet = "management_event"))
  dt_cham <- as.data.table(readxl::read_excel(fname_meta, sheet = "chamber"))
  dt_badd <- as.data.table(readxl::read_excel(fname_meta, sheet = "bad_data"))
  dt_ancl <- as.data.table(readxl::read_excel(fname_meta, sheet = "ancilliay_timeseries_byChamber"))

  dt_cham[, chamber_id := as.factor(chamber_id)]

  return(list(
    dt_site = dt_site,
    dt_expt = dt_expt,
    dt_trmt = dt_trmt,
    dt_mngt = dt_mngt,
    dt_cham = dt_cham,
    dt_badd = dt_badd,
    dt_ancl = dt_ancl
    )
  )
}

get_ghg_data <- function(this_date = v_dates[1], this_site_id = "EH", 
  this_expt_id = "digestate1", this_data_location = "local drive", l_meta) {
  # subset metadata to site, experiment and data_location
  dt <- l_meta$dt_expt[
    this_site_id == site_id & 
    this_expt_id == expt_id & 
    this_data_location == data_location]
    
  # find the raw chi files
  v_fnames <- dir_ls(dt[, path_to_GHG_data])
  v_fnames <- sort(v_fnames)
  
  v_dates_ghg <- substr(path_file(v_fnames), 12, 26)
  v_dates_ghg <- strptime(v_dates_ghg, "%Y%m%d-%H%M%S", tz = "GMT")  
  v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates_ghg)))
  if (length(v_ind) < 1) stop(paste("No GHG files on", this_date))
  # some data for this day may be in the last file from the previous day
  # so add this to the files read; do not do on first day
  if (v_ind[1] > 1) v_ind <- c(v_ind[1] - 1, v_ind)
  v_fnames[v_ind]
  l_dt <- lapply(v_fnames[v_ind], fread)
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

get_ch_position_data <- function(this_date = v_dates[1], this_site_id = "EH", 
  this_expt_id = "digestate1", this_data_location = data_location, l_meta) {
  # subset metadata to site, experiment and data_location
  dt <- l_meta$dt_expt[
    this_site_id == site_id & 
    this_expt_id == expt_id & 
    this_data_location == data_location]

  # find the raw chpos files
  v_fnames <- dir_ls(dt[, path_to_plot_data])
  v_fnames <- sort(v_fnames)
  
  v_dates_ghg <- substr(path_file(v_fnames), 21, 35)
  v_dates_ghg <- strptime(v_dates_ghg, "%Y_%m_%d_%H%M", tz = "GMT")  
  v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates_ghg)))
  # some data for this day may be in the last file from the previous day
  # so add this to the files read; do not do on first day
  #if (v_ind[1] > 1) v_ind <- c(v_ind[1] - 1, v_ind)
  dt <- read_cs_data(v_fnames[v_ind])
  l_dt <- lapply(v_fnames[v_ind], read_cs_data)
  dt <- rbindlist(l_dt)
  
  dt[, datect := as.POSIXct(round(TIMESTAMP, "secs"))]
  # aggregate to 1 Hz i.e. do 1-sec averaging
  dt <- dt[, lapply(.SD, mean), .SDcols = c("C_Voltage"),  by = datect]
  # subset to this_date before returning
  dt <- dt[this_date == as.POSIXct(round(datect, "days"))]
  dt[, chamber_id := as.factor(round(C_Voltage * 0.01, 0))]  
  return(dt)
}

get_data <- function(this_date = v_dates[1], this_site_id = "EH", 
  this_expt_id = "digestate1", data_location, l_meta) {
  
  dt_ghg <- get_ghg_data(        this_date = v_dates[1], this_site_id = "EH", 
    this_expt_id = "digestate1", this_data_location = data_location, l_meta)
  dt_pos <- get_ch_position_data(this_date = v_dates[1], this_site_id = "EH", 
    this_expt_id = "digestate1", this_data_location = data_location, l_meta)
  dt <- dt_ghg[dt_pos, on = .(datect = datect)]

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

  return(dt)
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
      m <- lm(t ~ CO2_dry + CH4_dry + N2O_dry + H2O + mmnt_id, w = w, data = dt)
    } else { # above fails with only one mmnt_id
      m <- lm(t ~ CO2_dry + CH4_dry + N2O_dry + H2O, w = w, data = dt)
    }
    summary(m)
    dt[, t_pred := predict(m)]
    dt[, t_resid := abs(scale(resid(m)))]
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
  p <- ggplot(dt1, aes(t, CO2_dry, colour = exclude)) + geom_point()
  p <- p + facet_wrap(~ mmnt_id) + xlim(0, NA)
  p <- p + geom_vline(xintercept = initial_deadband_width)
  p <- p + geom_vline(data = dt_sfdband, aes(xintercept = start_final_deadband))
p
  return(p)
}
  
plot_data <- function(dt, initial_deadband_width = 150, final_deadband_width = 150) {
  p <- ggplot(dt, aes(datect, C_Voltage, colour = mmnt_id)) + geom_point()
  p <- ggplot(dt, aes(datect, t, colour = mmnt_id)) + geom_point()
  p <- ggplot(dt, aes(t, CH4_dry, colour = mmnt_id)) + geom_point()
  p <- p + facet_wrap(~ mmnt_id)
  p
  
  p <- ggplot(dt, aes(t, t_pred, colour = mmnt_id)) + geom_point()
  p <- ggplot(dt[abs(t_resid) < 1000], aes(t, t_resid, colour = mmnt_id)) + geom_point()
  # p <- p + geom_abline()
  p <- p + facet_wrap(~ mmnt_id)
  p
  p <- ggplot(dt, aes(t, N2O_dry, colour = as.factor(seq_id))) + geom_point()
  p <- ggplot(dt, aes(t, CH4_dry, colour = as.factor(seq_id))) + geom_point()
  p <- ggplot(dt, aes(t, CO2_dry, colour = as.factor(chamber_id))) + geom_point()
  p <- p + facet_wrap(~ trmt_id) + xlim(0, NA)
  p
  return(p)
}



process_data <- function(v_dates, dt_chi) {}