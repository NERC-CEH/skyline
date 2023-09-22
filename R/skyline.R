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

get_ghg_data <- function(this_date = v_dates[1], this_site_id = "EH", this_expt_id = "digestate1", l_meta) {
  # subset metadata to site and experiment
  dt <- l_meta$dt_expt[.(this_site_id, this_expt_id)]
  # find the raw chi files
  v_fnames <- dir_ls(dt[, path_to_GHG_data])
  v_fnames <- sort(v_fnames)
  
  v_dates_ghg <- substr(path_file(v_fnames), 12, 26)
  v_dates_ghg <- strptime(v_dates_ghg, "%Y%m%d-%H%M%S", tz = "GMT")  
  v_ind <- which(this_date == as.POSIXct(lubridate::date(v_dates_ghg)))
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

get_ch_position_data <- function(this_date = v_dates[1], this_site_id = "EH", this_expt_id = "digestate1", l_meta) {
  # subset metadata to site and experiment
  dt <- l_meta$dt_expt[.(this_site_id, this_expt_id)]
  # find the raw chi files
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

get_data <- function(this_date = v_dates[1], this_site_id = "EH", this_expt_id = "digestate1", l_meta) {
  dt_ghg <- get_ghg_data(        this_date = v_dates[1], this_site_id = "EH", this_expt_id = "digestate1", l_meta)
  dt_pos <- get_ch_position_data(this_date = v_dates[1], this_site_id = "EH", this_expt_id = "digestate1", l_meta)
  dt_ghg <- dt_ghg[dt_pos, on = .(datect = datect)]
  dt <- dt_ghg[!is.na(H2O)] # subset to valid ghg data only
  # dt_ghg <- dt_ghg[1:10000]
  dt[, i := 1:.N, by = chamber_id]
  p <- ggplot(dt, aes(datect, C_Voltage, colour = chamber_id)) + geom_point()
  p <- ggplot(dt, aes(datect, i, colour = chamber_id)) + geom_point()
  p <- ggplot(dt, aes(i, CH4_dry, colour = chamber_id)) + geom_point()
  p <- p + facet_wrap(~ chamber_id)
  p
  
  # predict time based on all GHG concentrations
  # and use this to filter out the nonlinear part
  m <- lm(i ~ CO2_dry + CH4_dry + N2O_dry + H2O + chamber_id, data = dt)
  summary(m)
  dt[, i_pred := predict(m)]
  dt[, i_resid := resid(m)]
  p <- ggplot(dt, aes(i, i_pred, colour = chamber_id)) + geom_point()
  p <- ggplot(dt[abs(i_resid) < 10], aes(i, i_resid, colour = chamber_id)) + geom_point()
  # p <- p + geom_abline()
  p <- p + facet_wrap(~ chamber_id)
  p
  p <- ggplot(dt[abs(i_resid) < 10], aes(i, CO2_dry, colour = chamber_id)) + geom_point()
  p <- ggplot(dt[abs(i_resid) < 10], aes(i, N2O_dry, colour = chamber_id)) + geom_point()
  p <- ggplot(dt[abs(i_resid) < 10], aes(i, CH4_dry, colour = chamber_id)) + geom_point()
  p <- p + facet_wrap(~ chamber_id)
  p

  return(p)
}

process_data <- function(v_dates, dt_chi) {}