#' @import ggplot2

# constants
# define the conversion unit between g N and moles of N2O
units::install_unit("mol_n2o", "28 g", "mol wt of N in N2O")
units::install_unit(symbol = "ratio", def = "unitless",
  name = "dimensionless ratio")

#' @title read_cs_data
#' @description Read CSI TOA5 data from a file.
#'
#' Reads CSI TOA5 data from a file and returns a data table.
#'
#' @param filename The path to the file containing the CSI TOA5 data.
#' @param return_option A string indicating what to return. Can be either "data" or
#'      "info". If "data", the function will return a data table containing the
#'      CSI TOA5 data. If "info", the function will return a list containing the
#'      first four lines of the file, which contain metadata about the data.
#'
#' @return A data table containing the CSI TOA5 data, or a list containing the
#'    first four lines of the file, depending on the value of the `return_option`
#'    argument.
#'
#' @examples
#' \dontrun{
#' read_cs_data("csi_toa5_data.csv")
#' # A data table containing the CSI TOA5 data
#'
#' read_cs_data("csi_toa5_data.csv", return_option = "info")
#' # A list containing the first four lines of the file, which contain metadata about the data
#' }
#' @rdname read_cs_data
#' @export
read_cs_data <- function(filename, return_option = "data") {
  if (return_option == "info") {
    # bring in entire header of CSI TOA5 data file for metadata
    info <- scan(file = filename, nlines = 4, what = character(), sep = "\r")
    return(info)
  } else {
    # second line of header contains variable names
    header <- scan(file = filename, skip = 1, nlines = 1,
      what = character(), sep = ",")
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
  dt_site <- as.data.table(readxl::read_excel(fname_meta, sheet = "site"),
    key = c("site_id"))
  dt_expt <- as.data.table(readxl::read_excel(fname_meta, sheet = "experiment"),
    key = c("site_id", "expt_id"))
  dt_time <- as.data.table(readxl::read_excel(fname_meta, sheet = "analyser_time"))
  dt_trmt <- as.data.table(readxl::read_excel(fname_meta, sheet = "treatment"))
  dt_mgmt <- as.data.table(readxl::read_excel(fname_meta, sheet = "management_event"))
  dt_cham <- as.data.table(readxl::read_excel(fname_meta, sheet = "chamber"))
  dt_band <- as.data.table(readxl::read_excel(fname_meta, sheet = "deadbands"))
  dt_badd <- as.data.table(readxl::read_excel(fname_meta, sheet = "bad_data"))
  dt_ancl <- as.data.table(readxl::read_excel(fname_meta, sheet = "ancilliay_timeseries_byChamber"))

  dt_cham[, chamber_id := as.factor(chamber_id)]

  return(list(
    dt_site = dt_site,
    dt_expt = dt_expt,
    dt_time = dt_time,
    dt_trmt = dt_trmt,
    dt_mgmt = dt_mgmt,
    dt_cham = dt_cham,
    dt_band = dt_band,
    dt_badd = dt_badd,
    dt_ancl = dt_ancl))
}

check_data_available <- function(this_date, this_site_id = "EHD",
                                 this_expt_id = "digestate1",
                                 this_data_location = "local drive", l_meta) {
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
  if (!is.na(v_ind[1]) && v_ind[1] > 1) v_ind <- c(v_ind[1] - 1, v_ind)
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

get_ghg_data <- function(v_fnames, this_date, this_site_id, this_expt_id, l_meta) {
  l_dt <- lapply(v_fnames, fread)
  dt_ghg <- rbindlist(l_dt)

  # subset metadata to site, experiment and data_location
  dt_expt <- l_meta$dt_expt[this_site_id == site_id & this_expt_id == expt_id][1]
  dt_time <- l_meta$dt_time[this_site_id == site_id & this_expt_id == expt_id]
  dt_time <- dt_time[this_date >= start_date & this_date <= end_date]

  # standardise instrument-specific variable names
  v_names <- c("chi_h2o", "chi_co2", "chi_ch4", "chi_n2o", "P_cavity", "T_cavity")

  if (dt_expt$GHG_instrument == "Aeris MIRA Ultra") {
    dt_ghg[, datect := DateTime]
    dt_ghg[, chi_ch4 := 0] # add dummy variable for methane - not measured
    setnames(dt_ghg, c("H2O_ppm", "CO2_ppm", "chi_ch4", "N2O_ppm",
      "CavityPressure", "Tgas_degC"), v_names)
    ## TODO: set instrument-specific units here too
  } else { # default to Picarro
    dt_ghg[, datect := as.POSIXct(EPOCH_TIME, origin = "1970-01-01")]
    setnames(dt_ghg, c("H2O", "CO2_dry", "CH4_dry", "N2O_dry",
      "CavityPressure", "CavityTemp"), v_names, skip_absent = TRUE)
    ## TODO: set instrument-specific units here too
  }


  dt_ghg[, datect := datect + seconds(dt_time$t_offset)]

  dt_ghg[, datect := lubridate::round_date(datect, "secs")]
  # aggregate to 1 Hz i.e. do 1-sec averaging
  dt_ghg <- dt_ghg[, lapply(.SD, mean), .SDcols = v_names, by = datect]

  # subset to this_date before returning
  dt_ghg <- dt_ghg[this_date == as.POSIXct(lubridate::date(datect))]
  return(dt_ghg)
}

get_ch_position_data <- function(v_fnames, chpos_multiplier, this_date, this_site_id, this_expt_id, l_meta) {

  if (fs::path_ext(v_fnames[1]) == "csv") l_dt <- lapply(v_fnames, fread)
  if (fs::path_ext(v_fnames[1]) == "dat") l_dt <- lapply(v_fnames, read_cs_data)
  dt <- rbindlist(l_dt)

  # standardise time names
  if ("DateTime"  %in% names(dt)) dt[, datect := as.POSIXct(DateTime)]
  if ("TIMESTAMP" %in% names(dt)) dt[, datect := as.POSIXct(TIMESTAMP)]

  # rename position voltage consistently, depending whether sampled or averaged
  if ("C_Voltage_Avg" %in% names(dt)) {
    dt[, C_Voltage := C_Voltage_Avg]
    dt[, C_Voltage_Avg := NULL]
  }

  # strip out excess columns and aggregate to 1 Hz (latter prob not needed
  # as 1 Hz anyway (but is it always?)
  dt[, datect := lubridate::round_date(datect, "secs")]
  dt <- dt[, lapply(.SD, mean), .SDcols = c("C_Voltage"),  by = datect]
  # convert chamber position voltage to chamber ID
  dt[, chamber_id := round(C_Voltage * chpos_multiplier, 0)]
  # residual measures how far we are from expected mV
  dt[, C_mV_residual := abs(C_Voltage - (chamber_id / chpos_multiplier))]
  # do we need this as a factor?
  # dt[, chamber_id := as.factor(chamber_id)]

  dt_band <- l_meta$dt_band[this_site_id == site_id & this_expt_id == expt_id]
  dt_band <- dt_band[this_date >= start_date & this_date <= end_date]

  if (dt_band$ch_pos_change){
    dt[, seq_id  := rleid(chamber_id)]
    dt[, n := .N, by = seq_id]
    dt[n == 1, ':='(chamber_id = NA, C_mV_residual = NA)]
    dt <- setnafill(dt, type = "nocb", cols = c("chamber_id", "C_mV_residual"))
    dt[, ':='(seq_id=NULL, n=NULL)]
  }

  return(dt)
}

get_soilmet_data <- function(v_fnames, o2_data = TRUE) {
  if (fs::path_ext(v_fnames[1]) == "csv") l_dt <- lapply(v_fnames, fread,
    na.strings = c("NAN", "NA", "NaN"))
  if (fs::path_ext(v_fnames[1]) == "dat") l_dt <- lapply(v_fnames, read_cs_data)
  dt <- rbindlist(l_dt)

  # standardise time names
  if ("DateTime"  %in% names(dt)) dt[, datect := as.POSIXct(DateTime)]
  if ("TIMESTAMP" %in% names(dt)) dt[, datect := as.POSIXct(TIMESTAMP)]

  dt[, datect := lubridate::round_date(datect, "mins")]
  dt[, RECORD := NULL]
  setnames(dt, c("C_Temp_C_Avg", "QR_Avg", "QR_C_Avg"),
    c("TA", "PPFD_IN", "PPFD_IN_ch"))

  # If any column contains only NAs, it gets logical type and crashes melt
  # by trying to combine logical and numeric types in one column.
  # If any column contains only 0s, it gets integer type; melt gives a warning
  # about combining integer and numeric types in one column.
  # So, convert any logicals or integers to numeric.
  v_logical <- sapply(dt, is.logical)
  v_integer <- sapply(dt, is.integer)
  v_logical <- v_logical | v_integer
  if (any(v_logical)) {
    v_logical <- names(which(v_logical))
    dt[,  paste0(v_logical) := lapply(.SD, as.numeric), .SDcols = v_logical]
  }

  # reshape wide to long
  if (o2_data) {
    dt <- melt(dt,
               id.vars = c("datect", "TA", "PPFD_IN", "PPFD_IN_ch"),
               measure.vars = patterns("VWC", "TSoil", "SoilPerm", "SoilEC", "Concentration", "Sensor_mV", "Sensor_TC"),
               variable.name = "chamber_id",
               value.name = c("VWC", "TSoil", "SoilPerm", "SoilEC", "O_Concentration", "O_Sensor_mV", "O_Sensor_TC"))
  } else { # default to TDTs only
    dt <- melt(dt,
               id.vars = c("datect", "TA", "PPFD_IN", "PPFD_IN_ch"),
               measure.vars = patterns("VWC", "TSoil", "SoilPerm", "SoilEC"),
               variable.name = "chamber_id",
               value.name = c("VWC", "TSoil", "SoilPerm", "SoilEC"))
  }
  return(dt)
}

get_data <- function(v_dates = NULL, this_site_id = "HRG",
                     this_expt_id = "diurnal1", data_location, l_meta,
                     seq_id_to_plot = 1,
                     method = "time fit", dryrun = FALSE,
                     save_plots = FALSE, write_all = FALSE, n_min = 300) {
  # create directories for output
  pname_csv        <- here("output", this_site_id, this_expt_id, "csv")
  pname_png        <- here("output", this_site_id, this_expt_id, "png")
  pname_png_daily  <- here("output", this_site_id, this_expt_id, "png", "daily_plots")
  # subdirectory for unfiltered data, including deadbands
  pname_csv_unfilt <- here("output", this_site_id, this_expt_id, "csv", "unfilt")
  pname_png_unfilt <- here("output", this_site_id, this_expt_id, "png", "unfilt")
  pname_png_unfilt_daily <- here("output", this_site_id, this_expt_id, "png", "unfilt", "daily_plots")
  fs::dir_create(pname_csv)
  fs::dir_create(pname_png)
  fs::dir_create(pname_png_daily)
  fs::dir_create(pname_csv_unfilt)
  fs::dir_create(pname_png_unfilt)
  fs::dir_create(pname_png_unfilt_daily)

  # subset metadata to site, experiment and data_location
  dt_expt <- l_meta$dt_expt[this_site_id == site_id & this_expt_id == expt_id][1]

  # if not provided, use experiment start & end dates from metadata
  if (is.null(v_dates)) {
    start_date <- as.POSIXct(dt_expt$start_date)
    end_date   <- as.POSIXct(dt_expt$end_date)
    v_dates <- seq(from = start_date, to = end_date, by="day")
  }

  n_days <- length(v_dates)
  l_dt_chi  <- list()
  l_dt_flux <- list()
  for (i in seq_along(v_dates)) {  #seq_along equivalent to 1:length(v_dates), so processing one day at a time (/per loop)
    this_date <- v_dates[i]
    print(paste("Processing ", this_date))
    l_files <- check_data_available(this_date, this_site_id, this_expt_id, data_location, l_meta) # check if data is available (function above)
    if (length(l_files$v_fnames_ghg) == 0 || length(l_files$v_fnames_pos) == 0) next  # if no data today, move on to next day

    # load data - ghg, position, soilmet (functions above)
    dt_ghg <- get_ghg_data(l_files$v_fnames_ghg, this_date, this_site_id, this_expt_id, l_meta)
    dt_pos <- get_ch_position_data(l_files$v_fnames_pos, dt_expt$chpos_multiplier, this_date, this_site_id, this_expt_id, l_meta)
    dt_met <- get_soilmet_data(l_files$v_fnames_met, o2_data = dt_expt$o2_data)

    dt <- dt_pos[dt_ghg, on = .(datect = datect), roll = TRUE] # right rolling join of dt_pos and dt_ghg into dt by datetime, i.e. where there is
                                                               # missing values in certain columns (e.g. soilmet) carry on repeating the previous
                                                               # value until there is the next value in that column

    # find relevant deadband data
    dt_band <- l_meta$dt_band[this_site_id == site_id & this_expt_id == expt_id]
    dt_band <- dt_band[this_date >= start_date & this_date <= end_date]

    # for data without high frequency chamber position fill missing chamber ids with last occurring value
    if (dt_band$ch_pos_fill) {dt <- setnafill(dt, type = "locf", cols = c("chamber_id", "C_Voltage", "C_mV_residual"))}

    dt[, chamber_id := as.factor(chamber_id)]
    dt <- dt_met[dt, on = .(chamber_id = chamber_id, datect = datect), roll = TRUE] # rolling join of dt_met with dt based on chamber id column

    # shift chamber id down to fix lag
    dt <- dt[,chamber_id:=shift(chamber_id, dt_band$t_shift, type = "lag")]

    dt <- dt[!is.na(chamber_id)]  # remove where chamber_id data is missing

    # find unique mmnt_id from sequence of chamber_id
    dt[, seq_id  := rleid(chamber_id)] # enumerate the sequence, run-length id, through the column chamber_id each time it changes the r.leid +1
    # then enumerate the sequence for a given chamber
    dt[, seq_id := rleid(seq_id), by = chamber_id]
    # dt <- dt[chamber_id!=0] # remove rows where chamber_id
    dt[, mmnt_id := paste(lubridate::date(dt$datect),  # create measurement id consisting of date, chamber id, and sequence id (i.e. the  second or
      formatC(as.numeric(as.character(chamber_id)), width = 2, format = "d", flag = "0"),
      formatC(seq_id, width = 2, format = "d", flag = "0"),     # third time that day that a measurement was taken for that chamber position)
      sep = "_")]

    # remove first and last mmnt_id - as often partial
    dt <- dt[mmnt_id!=first(dt$mmnt_id),][mmnt_id!=last(dt$mmnt_id),]

    # enumerate the records within a mmnt sequence
    dt[, t := seq_len(.N), by = mmnt_id] # add column t with numbers 1:length(dt split by mmnt_id)
    # remove records beyond the maximum mmnt length - this assumes measurements taken every second!!
    dt <- dt[t < dt_band$t_max] # i.e. 5 minutes => t_max = 300

    dt[, n := .N, by = mmnt_id] # n contains a value for the number of times each mmnt_id appears (i.e. .N provides a variable for number of instances)
    # dt <- dt[!is.na(chi_h2o)] # subset to valid ghg data only ???

    # add day/night factor create light/dark ID column based on start of enclosure (sometimes crosses sunrise/sunset threshold in the middle of longer enclosures)
    dt_site <- l_meta$dt_site[this_site_id == site_id]
    my.geocode <- data.frame(lon = dt_site$longitude, lat = dt_site$latitude, address = dt_site$site_name)
    dt_dayf <- dt[, .SD[1], by = mmnt_id]
    dt_dayf <- dt_dayf[, light := is_daytime(date = dt_dayf$datect, tz = "GMT", geocode = my.geocode)]
    dt_dayf <- dt_dayf[, .(mmnt_id, light)]

    dt <- dt[dt_dayf, on = .(mmnt_id = mmnt_id)]

    # join with chamber data
    dt_cham <- l_meta$dt_cham[this_site_id == site_id & this_expt_id == expt_id] # read chamber sheet from metadata for corresponding site and expt id
    dt_cham <- dt_cham[this_date >= start_date & this_date < end_date] # retain only data relevant to day processing
    dt <- dt[dt_cham, on = .(chamber_id = chamber_id)] # join data to dt
    # remove where ghg data is missing
    dt <- dt[!is.na(datect)]
    # skip if chpos (chamber position) data is invalid - stuck on single value all day
    if (length(unique(dt$chamber_id)) < 2) next #
    # remove deadband and plot (function below)
    dt <- remove_deadband(dt,
      initial_deadband_width = dt_band$initial_deadband_width,
      final_deadband_width   = dt_band$final_deadband_width,
      chpos_tolerance_mV = dt_expt$chpos_tolerance_mV,
      t_resid_threshold = dt_band$t_resid_threshold,
      method = method, dryrun = dryrun)
    # re-check how many data are left
    dt[, n_filt := .N, by = mmnt_id] # n contains a value for the number of times each mmnt_id appears (i.e. .N provides a variable for number of instances)
    # if too few data left (100?), or too many (ch position sensor stuck)
    # then remove the whole measurement sequence
    dt <- dt[n_filt > n_min & n_filt < 1800] # do we need n_filt < 1800 if already filtering out above where chamber position gets stuck??
    # skip if no data left after filtering
    if (nrow(dt) == 0) next

    # save to file and list
    if (dryrun) {
      fname <- paste0(pname_csv_unfilt, "/dt_chi_", lubridate::date(this_date), ".csv")
      p <- plot_data_unfiltered(dt, gas_name = "chi_co2",
        initial_deadband_width = dt_band$initial_deadband_width,
        final_deadband_width   = dt_band$final_deadband_width, seq_id_to_plot = seq_id_to_plot)
      p <- plot_data_unfiltered(dt, gas_name = "chi_ch4",
        initial_deadband_width = dt_band$initial_deadband_width,
        final_deadband_width   = dt_band$final_deadband_width, seq_id_to_plot = seq_id_to_plot)
      p <- plot_data_unfiltered(dt, gas_name = "chi_n2o",
        initial_deadband_width = dt_band$initial_deadband_width,
        final_deadband_width   = dt_band$final_deadband_width, seq_id_to_plot = seq_id_to_plot)
      fwrite(dt, file = fname)
      l_dt_chi[[i]] <- dt
    }


    if (save_plots) {
      if (dryrun){
        # p <- plot_chi(dt, gas_name = "chi_h2o")
        # fname <- paste0(pname_png_unfilt, "/h2o_", lubridate::date(this_date), ".png")
        # ggsave(p, file = fname, type = "cairo")
        p <- plot_chi(dt, gas_name = "chi_co2")
        fname <- paste0(pname_png_unfilt_daily, "/co2_", lubridate::date(this_date), ".png")
        ggsave(p, file = fname, type = "cairo")
        p <- plot_chi(dt, gas_name = "chi_ch4")
        fname <- paste0(pname_png_unfilt_daily, "/ch4_", lubridate::date(this_date), ".png")
        ggsave(p, file = fname, type = "cairo")
        p <- plot_chi(dt, gas_name = "chi_n2o")
        fname <- paste0(pname_png_unfilt_daily, "/n2o_", lubridate::date(this_date), ".png")
        ggsave(p, file = fname, type = "cairo")
      } else {
          p <- plot_chi(dt, gas_name = "chi_h2o")
          fname <- paste0(pname_png_daily, "/h2o_", lubridate::date(this_date), ".png")
          ggsave(p, file = fname, type = "cairo")
          p <- plot_chi(dt, gas_name = "chi_co2")
          fname <- paste0(pname_png_daily, "/co2_", lubridate::date(this_date), ".png")
          ggsave(p, file = fname, type = "cairo")
          p <- plot_chi(dt, gas_name = "chi_ch4")
          fname <- paste0(pname_png_daily, "/ch4_", lubridate::date(this_date), ".png")
          ggsave(p, file = fname, type = "cairo")
          p <- plot_chi(dt, gas_name = "chi_n2o")
          fname <- paste0(pname_png_daily, "/n2o_", lubridate::date(this_date), ".png")
          ggsave(p, file = fname, type = "cairo")
        }
      }


    # calculate fluxes each gas and save to file and add to list
# TODO: why not calculate fluxes in dryrun? Just write to unfilt folder
    if (!dryrun) {
      # dt <- calc_flux(dt, gas_name = "chi_h2o", use_STP = dt_expt$use_STP)
      dt <- calc_flux(dt, gas_name = "chi_co2", t_co2_cut = dt_band$t_co2_cut, t_max_co2 = dt_band$t_co2_max,
                      n_min = n_min, use_STP = dt_expt$use_STP)
      dt <- calc_flux(dt, gas_name = "chi_ch4", use_STP = dt_expt$use_STP)
      dt <- calc_flux(dt, gas_name = "chi_n2o", use_STP = dt_expt$use_STP)
      fname <- paste0(pname_csv, "/dt_chi_", lubridate::date(this_date), ".csv")
      fwrite(dt, file = fname)
      l_dt_chi[[i]] <- dt
      # subset to just the first record
      dt_flux <- dt[, .SD[1], by = mmnt_id]
      dt_flux[, mmnt_id := paste(datect, #lubridate::date(dt_flux$datect),
                                 # formatC(lubridate::hour(dt_flux$datect), width = 2, format = "d", flag = "0"),
                                 # formatC(lubridate::minute(dt_flux$datect), width = 2, format = "d", flag = "0"),
                                 formatC(as.numeric(as.character(chamber_id)), width = 2, format = "d", flag = "0"),
                                 sep = "_")]
      fname <- paste0(pname_csv, "/dt_flux_", lubridate::date(this_date), ".csv")
      fwrite(dt_flux, file = fname)
      l_dt_flux[[i]] <- dt_flux
    }

  }
  dt_chi  <- rbindlist(l_dt_chi)
  dt_flux <- rbindlist(l_dt_flux)

  if (write_all & !dryrun) {
    # save to files
    fname <- paste0(pname_csv, "/dt_chi_", lubridate::date(v_dates[1]), "_",
      lubridate::date(v_dates[n_days]), ".csv")
    fwrite(dt_chi, file = fname)
    fname <- paste0(pname_csv, "/dt_flux_", lubridate::date(v_dates[1]), "_",
      lubridate::date(v_dates[n_days]), ".csv")
    fwrite(dt_flux, file = fname)
  }
  return(list(dt_chi = dt_chi, dt_flux = dt_flux))
}

# TODO: this function is never called, WIP for filtering data where chanber didn't close
filter_data <- function(site_id, expt_id, l_meta){
  pname_csv <- here("output", site_id, expt_id, "csv")
  pattern <- paste0(.Platform$file.sep, "dt_chi.*\\.csv$")
  v_fnames <- dir_ls(pname_csv, regexp = pattern)
  l_dt <- lapply(v_fnames, fread)
  dt <- rbindlist(l_dt)
  gas_name <- "chi_co2"
  dt[, Date := lubridate::date(datect)]
  # dt <- dt[t<200]

  dt[, exclude := FALSE]
  dt[PPFD_IN < 2 & rmse_f_co2 > 1 & r2_f_co2 < 0.85, exclude := TRUE]
  dt[PPFD_IN >= 2 & rmse_f_co2 > 1.5 & r2_f_co2 < 0.7, exclude := TRUE]
  this_date <- "2023-06-24"
  dt1 <- dt[this_date == Date]


  rm(p)
  p <- ggplot(dt1, aes(t, get(gas_name), colour = as.factor(exclude), group = mmnt_id))
  p <- p + geom_point(alpha = 0.1) ## WIP setting alpha adds computation time - try without
  p <- p + ylab(gas_name)
  p <- p + facet_wrap(~ chamber_id)
  p


}

remove_deadband <- function(dt, initial_deadband_width = 150, final_deadband_width = 150,
                            chpos_tolerance_mV = 6, t_resid_threshold = 1,
                            method = c("time fit", "specified deadband only"), dryrun = FALSE) {

  dt[, exclude := FALSE]
  dt[C_mV_residual > chpos_tolerance_mV, exclude := TRUE]
  # add mmnt-specific latter deadband
  dt[, start_final_deadband := n - final_deadband_width, by = mmnt_id]

  if (method == "specified deadband only") {
    dt[t < initial_deadband_width | t > start_final_deadband, exclude := TRUE]
  } else if (method == "time fit") {
    dt[, w := dbeta(t / n, shape1 = 1.5, shape2 = 1.5)]
    dt[t < initial_deadband_width | t > start_final_deadband, w := 0]

    # predict time based on all GHG concentrations, weighted towards the middle
    # and use this to filter out the nonlinear part
    if (length(unique(dt$mmnt_id)) > 1) {
      form <- formula(t ~ chi_co2 + chi_ch4 + chi_n2o + chi_h2o + C_Voltage)
      # very slow on JASMIN:
      # m <- lm(t ~ chi_co2 + chi_ch4 + chi_n2o + chi_h2o + mmnt_id, w = w, data = dt) # nolint
      dt[, t_pred := predict(lm(form, w = w, data = .SD)), by = mmnt_id]
      dt[, t_resid := abs(scale(resid(lm(form, w = w, data = .SD)))), by = mmnt_id]
    } else { ## WIP would the above fail with only one mmnt_id? just in case:
      dt[, t_pred := predict(lm(form, w = w, data = .SD))]
      dt[, t_resid := abs(scale(resid(lm(form, w = w, data = .SD))))]
    }
    # Leave middle half of the data untouched - not deadband.
    # In the first and last quarters, exclude if residual is too high.
    dt[(t / n <= 0.25 | t / n >= 0.75) &
      t_resid > t_resid_threshold, exclude := TRUE]
    # exclude the pre-defined deadband
    dt[t < initial_deadband_width | t > start_final_deadband,
      exclude := TRUE]
  }
  # subset the data to only include those with exclude = FALSE, unless it is a dryrun, where keep all the data with exclude = TRUE or FALSE
  if (!dryrun) dt <- dt[exclude == FALSE]
  return(dt)
}

plot_data_unfiltered <- function(dt_unfilt, gas_name = "chi_co2",
                                 initial_deadband_width = 150,
                                 final_deadband_width = 150, seq_id_to_plot = 1) {
  # if the requested seq_id is not available, set to first value in list
  if (seq_id_to_plot %!in% dt_unfilt$seq_id) seq_id_to_plot <-
    unique(dt_unfilt$seq_id)[1]
  dt1 <- dt_unfilt[seq_id_to_plot == seq_id]
  dt_sfdband <- dt1[, .(start_final_deadband = .SD[1, start_final_deadband]),
    by = mmnt_id]

  p <- ggplot(dt1, aes(t, get(gas_name), colour = exclude))
  p <- p + geom_point(aes(size = t_resid))
  p <- p + geom_point()
  p <- p + facet_wrap(~ mmnt_id) + xlim(0, NA)
  p <- p + geom_vline(xintercept = initial_deadband_width)
  p <- p + geom_vline(data = dt_sfdband, aes(xintercept = start_final_deadband))

  fname <- here("output", dt1$site_id[1], dt1$expt_id[1], "png", "unfilt",
    paste0(gas_name, "_", as.character(lubridate::date(dt1$datect[1])),
      "_", seq_id_to_plot, ".png"))
  ggsave(p, file = fname, type = "cairo")
  return(p)
}

plot_chi <- function(dt, gas_name = "chi_n2o") {
  p <- ggplot(dt, aes(t, get(gas_name), colour = as.factor(seq_id), group = mmnt_id))
  p <- p + geom_point(alpha = 0.1) ## WIP setting alpha adds computation time - try without
  p <- p + ylab(gas_name)
  p <- p + facet_wrap(~ chamber_id)
  # alternative option - add save_plot argument
  # fname <- here("output", dt$site_id[1], dt$expt_id[1], "png",
                # paste0(gas_name, "_",
                # as.character(lubridate::date(dt$datect[1])), ".png"))
  # if (save_plot) ggsave(p, file = fname, type = "cairo")
  return(p)
}

# TODO: remove this function add geom_smooth as an option in plot_chi, this plots all fluxes
# for a specified date only separately
plot_chi_lm <- function(dt, gas_name = "chi_n2o") {
  v_chamber <- as.numeric(unique(dt$chamber_id))
  for (i in seq_along(v_chamber)) {
  dt1 <- dt[i == chamber_id]
  p <- ggplot(dt1, aes(t, get(gas_name))) + geom_smooth(method = stats::lm)
  p <- p + geom_point()
  p <- p + facet_wrap(~ mmnt_id)

  gas <- substr(gas_name, nchar(gas_name) - 2, nchar(gas_name))
  fname <- here("output", dt1$site_id[1], dt1$expt_id[1], "png", "flux", gas, as.character(lubridate::date(dt1$datect[1])),
                paste0("f_", gas, "_", as.character(lubridate::date(dt1$datect[1])),
                       "_", i, ".png"))
  ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

calc_flux <- function(dt, gas_name = "chi_co2", t_co2_cut = TRUE, t_max_co2 = 120, n_min = 120, use_STP = TRUE, PA = 1000, TA = 15) {
  # TODO: I don't see the need for this code block - just subset on t <= t_max_co2? EA added comments to explain what code is doing
  if (gas_name == "chi_co2" && t_co2_cut) {
    ## only need to shorten day/light fluxes so split by PAR
    dt_1 <- dt[, t:= seq_len(.N), by = mmnt_id]
    # split by light/dark ID and shorten light fluxes
    dt_dark <- dt_1[light == FALSE,] # subset all dark data
    dt_light <- dt_1[light == TRUE,] # subset all light data
    dt_light <- dt_light[t <= t_max_co2] # remove excess rows in light dt
    dt_co2 <- rbind(dt_light, dt_dark) # combine dark and shortened light dt into one dt for flux calculation
    dt_co2 <- dt_co2 %>% dplyr::arrange(mmnt_id)
    dt_co2[, n_f_co2 := .N, by = mmnt_id]
    # dt_co2 <- dt_co2[n_f_co2 > n_min]

    form <- formula(paste(gas_name, "~ t"))
    # use substr to get rid of "chi" in gas name
    flux_var_name  <- paste0("f_",     substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    sigma_var_name <- paste0("sigma_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    rmse_var_name <- paste0("rmse_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    r2_var_name <- paste0("r2_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    pvalue_var_name <- paste0("p_value_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    dt_co2[, dchi_dt := coef(lm(form, data = .SD))[2], by = mmnt_id]
    dt_co2[, sigma_dchi_dt := summary(lm(form, data = .SD))$coefficients[2, 2], by = mmnt_id]
    if (use_STP) {
      rho <- PA * 100 / (8.31447 * (TA + 273.15))
    } else {
      # calculate mean air density for each mmnt if we have the specific data
      dt_co2[, rho := PA * 100 / (8.31447 * (TA + 273.15))]
    }
    dt_co2[, (flux_var_name) := dchi_dt        * rho * volume_m3 / area_m2]
    dt_co2[, (sigma_var_name) := sigma_dchi_dt * rho * volume_m3 / area_m2]
    dt_co2[, (r2_var_name) := summary(lm(form, data = .SD))$r.squared, by = mmnt_id]
    dt_co2[, (rmse_var_name) := sqrt(mean(summary(lm(form, data = .SD))$residuals^2)), by = mmnt_id]
    dt_co2[, (pvalue_var_name) := summary(lm(form, data = .SD))$coefficients[2,4]]

    # remove un-needed variables
    dt_co2[, dchi_dt := NULL]
    dt_co2[, sigma_dchi_dt := NULL]

    dt_co2 <- dt_co2[, .SD[1], by = mmnt_id] # subset to only first record of each mmnt_id
    dt_co2 <- dt_co2 %>% select("mmnt_id", contains("f_co2"))

    #rolling join with unshortened dataset
    dt <- dt[dt_co2, on = "mmnt_id", roll = TRUE]

  } else {
    form <- formula(paste(gas_name, "~ t"))
    # use substr to get rid of "chi" in gas name
    flux_var_name  <- paste0("f_",     substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    sigma_var_name <- paste0("sigma_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    rmse_var_name <- paste0("rmse_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    r2_var_name <- paste0("r2_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    pvalue_var_name <- paste0("p_value_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
    # adj.r2_var_name <- paste0("adj_r2_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
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
    dt[, (r2_var_name) := summary(lm(form, data = .SD))$r.squared, by = mmnt_id]
    dt[, (rmse_var_name) := sqrt(mean(summary(lm(form, data = .SD))$residuals^2)), by = mmnt_id]
    dt[, (pvalue_var_name) := summary(lm(form, data = .SD))$coefficients[2,4]]
    # dt[, (adj.r2_var_name) := summary(lm(form, data = .SD))$adj.r.squared, by = mmnt_id]

    # remove un-needed variables
    dt[, dchi_dt := NULL]
    dt[, sigma_dchi_dt := NULL]
  }
  return(dt)
}

combine_fluxes <- function(site_id, expt_id) {
  pname_csv <- here("output", site_id, expt_id, "csv")
  pattern <- paste0(.Platform$file.sep, "dt_flux.*\\.csv$")
  v_fnames <- dir_ls(pname_csv, regexp = pattern)
  l_dt <- lapply(v_fnames, fread)
  dt <- rbindlist(l_dt)
  fname <- here("output", site_id, expt_id, paste0("dt_flux_", lubridate::date(dt$datect[1]), "_",
                  lubridate::date(dt$datect[length(dt$datect)]), ".csv"))
  fwrite(dt, file = fname)
  return(dt)
}

# TODO: this function is never called, same as combine_fluxes function above but uses bad_data
# sheet in metadata to remove bad data
final_fluxes <- function(site_id, expt_id, l_meta){
  pname_csv <- here("output", site_id, expt_id, "csv")
  pattern <- paste0(.Platform$file.sep, "dt_flux.*\\.csv$")
  v_fnames <- dir_ls(pname_csv, regexp = pattern)
  l_dt <- lapply(v_fnames, fread)
  dt <- rbindlist(l_dt)
  dt_badd <- l_meta$dt_badd[site_id == site_id & expt_id == expt_id] # read chamber sheet from metadata for corresponding site and expt id
  for (i in 1:nrow(dt_badd)){
    dt <- dt %>% dplyr::filter(!(chamber_id == dt_badd$chamber_id[i] & datect > dt_badd$start_time[i] & datect < dt_badd$end_time[i]))
  }
  fname <- here("output", site_id, expt_id, paste0("dt_flux_", lubridate::date(dt$datect[1]), "_",
                                                   lubridate::date(dt$datect[length(dt$datect)]), ".csv"))
  fwrite(dt, file = fname)
  return(dt)
}

plot_flux <- function(dt_flux, flux_name = "f_co2",
                      sigma_name = "sigma_f_co2", site_id, expt_id,
                      mult = 1, y_min = NA, y_max = NA,
                      save_plot = FALSE) {

  dt_flux[, f     := get(flux_name) * mult]
  dt_flux[, sigma := get(sigma_name) * mult]
  dt_flux[, ci_lo := f - (sigma * 1.96)]
  dt_flux[, ci_hi := f + (sigma * 1.96)]
  if (is.na(y_max)) y_max <- max(dt_flux[, ci_hi])
  if (is.na(y_min)) y_min <- min(dt_flux[, ci_lo])

  p <- ggplot(dt_flux, aes(datect, f, colour = as.factor(chamber_id)))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_point()
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi))
  p <- p + facet_wrap(~trmt_id, nrow = length(unique(dt_flux$trmt_id)))
  p <- p + ylab(flux_name)
  p <- p + ylim(c(y_min, y_max))

  if (save_plot) {
    fname <- here("output", site_id, expt_id, "png",
      paste0(flux_name, "_timeseries.png"))
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

plot_n2o_flux <- function(dt, flux_name = "f_n2o",
                          sigma_name = "sigma_n2o",
                          this_site_id = "HRG", this_expt_id = "diurnal1",
                          l_meta, mult = 1000, y_min = NA, y_max = NA,
                          save_plot = FALSE) {
  # subset metadata to site, experiment and data_location
  dt_mgmt <- l_meta$dt_mgmt[
    this_site_id == site_id &
      this_expt_id == expt_id]
  names(dt_mgmt) <- make.names(names(dt_mgmt))

  # subset to just this experiment
  dt <- dt[site_id == this_site_id & expt_id == this_expt_id]

  dt[, f     := get(flux_name) * mult]
  dt[, sigma := get(sigma_name) * mult]
  dt[, ci_lo := f - (sigma * 1.96)]
  dt[, ci_hi := f + (sigma * 1.96)]
  if (is.na(y_max)) y_max <- max(dt[, ci_hi])
  if (is.na(y_min)) y_min <- min(dt[, ci_lo])

  p <- ggplot(dt, aes(datect, f))
  p <- p + geom_vline(data = dt_mgmt, aes(xintercept = start))
  p <- p + geom_hline(yintercept = 0)
  # p <- p + geom_point(aes(colour = as.factor(chamber_id)))
  p <- p + geom_point(aes(colour = trmt_id))
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, colour = trmt_id))
  # p <- p + facet_wrap(~ trmt_id)
  p <- p + ylab(flux_name)
  p <- p + stat_smooth()
  p <- p + ylim(y_min, y_max)

  if (save_plot) {
    fname <- here("output", this_site_id, this_expt_id, "png",
      paste0(flux_name, "_timeseries_with_treatment.png"))
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

plot_n2o_flux_diurnal <- function(dt_flux, flux_name = "f_N2O_dry",
                                  sigma_name = "sigma_N2O_dry", this_site_id, this_expt_id,
                                  mult = 1000, y_min = -2, y_max = 2.5) {

  dt_flux[, f     := get(flux_name) * mult]
  dt_flux[, sigma := get(sigma_name) * mult]
  dt_flux[, ci_lo := f - (sigma * 1.96)]
  dt_flux[, ci_hi := f + (sigma * 1.96)]
  dt_flux[, h := lubridate::hour(datect) + lubridate::minute(datect) / 60]
  if (is.na(y_max)) y_max <- max(dt[, ci_hi])
  if (is.na(y_min)) y_min <- min(dt[, ci_lo])

  p <- ggplot(dt_flux, aes(h, f))
  p <- p + geom_point(colour = "yellow")
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
    colour = "dark orange")
  p <- p + geom_hline(yintercept = 0)
  p <- p + stat_smooth(method = "gam")
  p <- p + ylab(flux_name)
  p <- p + ylim(y_min, y_max)


  fname <- here("output", this_site_id, this_expt_id, "png",
    paste0(flux_name, "_diurnal.png"))
  ggsave(p, file = fname, type = "cairo")

  return(p)
}

plot_flux_vs_xvar <- function(dt, flux_name = "f_co2",
                              sigma_name = "sigma_f_co2", xvar_name = "datect",
                              colour_name = "trmt_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = FALSE,
                              mult = 1, y_min = NA, y_max = NA,
                              save_plot = FALSE) {

  dt[, f     := get(flux_name)  * mult]
  dt[, sigma := get(sigma_name) * mult]
  dt[, x     := get(xvar_name)]
  dt[, ci_lo := f - (sigma * 1.96)]
  dt[, ci_hi := f + (sigma * 1.96)]
  if (is.na(y_max)) y_max <- max(dt[, ci_hi])
  if (is.na(y_min)) y_min <- min(dt[, ci_lo])

  if (colour_is_factor) {
    p <- ggplot(dt, aes(x, f, colour = as.factor(get(colour_name))))
    p <- p + scale_colour_viridis(option="viridis", discrete = TRUE)
  } else {
    p <- ggplot(dt, aes(x, f, colour = get(colour_name)))
    p <- p + scale_colour_viridis(option="viridis")
  }
  p <- p + geom_point()
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi))
  p <- p + geom_hline(yintercept = 0)
  #p <- p + facet_wrap(~ get(facet_name))
  if (rows_only) {
    p <- p + facet_wrap(~ get(facet_name), nrow = length(unique(dt[, get(facet_name)])))
  } else {
    p <- p + facet_wrap(~ get(facet_name))
  }
  p <- p + ylab(flux_name) + xlab(xvar_name) + labs(colour = NULL)
  p <- p + ylim(y_min, y_max)

  if (save_plot) {
    # use site & expt in first row for file name
    fname <- here("output", dt[1, .(site_id, expt_id)],
      paste0(flux_name, "_vs_", xvar_name, ".png"))
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

# function for opposite of "%in%"
"%!in%" <- function(x, y) !("%in%"(x, y))

# function for exponential moving average
ema <- function (x, ratio) {
  c(stats::filter(x * ratio, 1 - ratio, "recursive", init = x[1]))
}

finding_Nema <- function(dt_flux, l_meta, save_file = FALSE) {
  dt_mgmt <- l_meta$dt_mgmt
  names(dt_mgmt) <- make.names(names(dt_mgmt))
  dt_mgmt <- dt_mgmt[N_appl_amount_kg.ha > 0]
  dim(dt_mgmt)
  # get the start and end dates for the experiment by joining on expt table
  dt_expt <- l_meta$dt_expt[data_location == "local drive",
    .( site_id, expt_id, expt_name, start_date, end_date)]
  dim(dt_expt)
  dt_mgmt <- dt_mgmt[dt_expt, on = .(site_id = site_id, expt_id = expt_id)]
  dt_mgmt <- dt_mgmt[N_appl_amount_kg.ha > 0]

  # create a daily sequence covering all experiments in mgmt data
  first_date_mgmt <- min(as.POSIXct(dt_mgmt$start_date))
  last_date_mgmt  <- max(as.POSIXct(dt_mgmt$end_date))
  # create a daily sequence covering all experiments in flux data
  first_date_flux <- lubridate::floor_date(min(as.POSIXct(dt_flux$datect)), "day")
  last_date_flux  <- lubridate::floor_date(max(as.POSIXct(dt_flux$datect)), "day")
  first_date <- min(first_date_mgmt, first_date_flux)
  last_date  <- max(last_date_mgmt, last_date_flux)
  v_date <- seq(from = first_date, to = last_date, by = "days")
  n_date <- length(v_date)

  # create a data table in long format with all N application events, n_event x n_date
  # restrict to the N application variables
  dt_mgmt <- dt_mgmt[, .(site_id, expt_id, trmt_id, event_id, datect = as.POSIXct(round(start, "days")), N_appl_amount_kg.ha)]
  # make a unique event id - could use event_id instead of start date if available
  dt_mgmt[,  trmt_uid := paste0(expt_id, "_", trmt_id)]
  dt_mgmt[, event_uid := paste0(expt_id, "_", trmt_id, "_", datect)]
  n_event <- length(unique(dt_mgmt$event_uid))
  v_event <- rep(dt_mgmt$event_uid, times = 1, length.out = NA, each = n_date)
  v_dates <- rep(v_date,            times = n_event, length.out = NA, each = 1)
  dt_date <- data.table(event_uid = v_event, datect = v_dates)
  setkey(dt_mgmt, event_uid, datect)
  setkey(dt_date, event_uid, datect)
  # dt_date[dt_mgmt, ddate := i.datect]
  dt_date[dt_mgmt, trmt_uid := trmt_uid, on = .(event_uid = event_uid)]
  dt_date[dt_mgmt, N_appl := N_appl_amount_kg.ha]
  dt_date[is.na(N_appl), N_appl := 0]

  # do ema averaging of Nappl
  dt_date[, N_ema_010 := round(ema(N_appl, 0.10), 6), by = event_uid]
  dt_date[, N_ema_025 := round(ema(N_appl, 0.25), 6), by = event_uid]
  dt_date[, N_ema_050 := round(ema(N_appl, 0.50), 6), by = event_uid]

  # set units
  dt_date[, N_ema_010 := set_units(N_ema_010, kg/ha)]
  dt_date[, N_ema_025 := set_units(N_ema_025, kg/ha)]
  dt_date[, N_ema_050 := set_units(N_ema_050, kg/ha)]

  dt_date[, N_ema_010 := set_units(N_ema_010, mol_n2o/m^2)]
  dt_date[, N_ema_025 := set_units(N_ema_025, mol_n2o/m^2)]
  dt_date[, N_ema_050 := set_units(N_ema_050, mol_n2o/m^2)]

  dt_date[, .(mean(N_appl), mean(N_ema_050)), by = event_uid]

  # add up all events for each trmt, so they can overlap
  dt_date[, N_ema_010 := sum(N_ema_010), by = .(trmt_uid, datect)]
  dt_date[, N_ema_025 := sum(N_ema_025), by = .(trmt_uid, datect)]
  dt_date[, N_ema_050 := sum(N_ema_050), by = .(trmt_uid, datect)]

  # shift function for time lag
  dt_date[, N_ema_010_1 := shift(N_ema_010, n = 1, fill = 0), by = trmt_uid]
  dt_date[, N_ema_010_2 := shift(N_ema_010, n = 2, fill = 0), by = trmt_uid]
  dt_date[, N_ema_010_3 := shift(N_ema_010, n = 3, fill = 0), by = trmt_uid]
  dt_date[, N_ema_010_4 := shift(N_ema_010, n = 4, fill = 0), by = trmt_uid]
  #
  dt_date[, N_ema_025_1 := shift(N_ema_025, n = 1, fill = 0), by = trmt_uid]
  dt_date[, N_ema_025_2 := shift(N_ema_025, n = 2, fill = 0), by = trmt_uid]
  dt_date[, N_ema_025_3 := shift(N_ema_025, n = 3, fill = 0), by = trmt_uid]
  dt_date[, N_ema_025_4 := shift(N_ema_025, n = 4, fill = 0), by = trmt_uid]
  #
  dt_date[, N_ema_050_1 := shift(N_ema_050, n = 1, fill = 0), by = trmt_uid]
  dt_date[, N_ema_050_2 := shift(N_ema_050, n = 2, fill = 0), by = trmt_uid]
  dt_date[, N_ema_050_3 := shift(N_ema_050, n = 3, fill = 0), by = trmt_uid]
  dt_date[, N_ema_050_4 := shift(N_ema_050, n = 4, fill = 0), by = trmt_uid]


  # dt_date[10:100]
  # dt_date[(869+10):(869+100)]
  # dt_date[19890:19900]

  dt_flux[,  trmt_uid := paste0(expt_id, "_", trmt_id)]
  dt_flux[,  datect := lubridate::floor_date(datect, "day")]
  unique(dt_flux$trmt_uid)
  unique(dt_date$trmt_uid)
  names(dt_flux)
  names(dt_date)
  dt_flux <- dt_flux[dt_date, on = .(trmt_uid = trmt_uid, datect = datect)]
  return(dt_flux)
}

filter_fluxes <- function(dt, save_file = FALSE, fname = "dt_flux") {
  # remove days during experiment when no flux measurements
  dt <- dt[!is.na(site_id)]
  # crude filtering of extreme outliers; units of umol/m2/s
  dt <- dt[f_co2 > -50 & f_co2 < 50]
  dt <- dt[f_n2o > -0.1 & f_n2o < 0.1]
  dt <- dt[rmse_f_n2o < 0.021]
  if (save_file) fwrite(dt, file = here("output", paste0(fname, ".csv")))
  if (save_file)  qsave(dt, file = here("output", paste0(fname, ".qs")))
  return(dt)
}

plot_means_by_trmt <- function(dt, flux_name = "f_n2o",
                              show_raw = TRUE,
                              by_chamber = TRUE,
                              mult = 1, save_plot = FALSE) {
  dt[, f     := get(flux_name)  * mult]

  if (by_chamber) {
    dt_summary <- dt[, .(f = mean(f, na.rm = TRUE),
                         f_sd = sd(f, na.rm = TRUE),
                         n    = .N),
                         by = .(trmt_id, chamber_id)]
    dt_summary[, chamber_id := as.factor(chamber_id)]
  } else {
    dt_summary <- dt[, .(f = mean(f, na.rm = TRUE),
                         f_sd = sd(f, na.rm = TRUE),
                         n    = .N),
                         by = trmt_id]
  }

  dt_summary[, f_se := f_sd / sqrt(n)]
  dt_summary[, ci_lo := f - (f_se * 1.96)]
  dt_summary[, ci_hi := f + (f_se * 1.96)]

  # Combine with jitter points
  p <- ggplot(dt, aes(trmt_id, f))
  if (show_raw) {
    p <- p + geom_jitter(position = position_jitter(0.2), color = "darkgray")
    p <- p + geom_violin(color = "darkgray", trim = FALSE)
  }
  if (by_chamber) {
    p <- p + geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi,
      colour = trmt_id), position = position_jitter(0.2), data = dt_summary)
  } else {
    p <- p + geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), colour = "red", data = dt_summary)
  }

  if (save_plot) {
    # use site & expt in first row for file name
    fname <- here("output", dt[1, .(site_id, expt_id)],
      paste0(flux_name, "_vs_", xvar_name, ".png"))
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

bar_means_by_trmt <- function(dt, flux_name = "f_co2",
                              mult = 1, save_plot = FALSE) {
  dt[, f := get(flux_name)  * mult]

  dt_trmt <- dt[, .(f = mean(f, na.rm = TRUE),
                    f_sd = sd(f, na.rm = TRUE),
                    n    = .N),
                    by = trmt_id]
  dt_cham <- dt[, .(f = mean(f, na.rm = TRUE),
                    f_sd = sd(f, na.rm = TRUE),
                    n    = .N),
                    by = .(trmt_id, chamber_id)]
  dt_cham[, chamber_id := as.factor(chamber_id)]

  dt_cham[, f_se := f_sd / sqrt(n)]
  dt_cham[, ci_lo := f - (f_se * 1.96)]
  dt_cham[, ci_hi := f + (f_se * 1.96)]

# Bar plots + jittered points + error bars
  p <- ggplot(dt_cham, aes(trmt_id, f, colour = trmt_id))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_col(data = dt_trmt, position = position_dodge(0.8),
           width = 0.7, fill = "white")
  # p <- p + geom_jitter(position = position_jitter(0.2))
  p <- p + geom_errorbar(
    aes(ymin = ci_lo, ymax = ci_hi),
    width = 0.1, position = position_jitter(0.2))

  m_lmer <- lmer(f ~ trmt_id + (1 | chamber_id), data = dt)
  edf_lmer <- ggpredict(m_lmer, terms = c("trmt_id"))
  df_lmer <- as.data.frame(edf_lmer)
  names(df_lmer)[1] <- "trmt_id"
  names(df_lmer)[2] <- "f"
  # plot(edf_lmer) + ylim(0, NA)
  p <- p + geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
    colour = "black", data = df_lmer)

  if (save_plot) {
    # use site & expt in first row for file name
    fname <- here("output", dt[1, .(site_id, expt_id)],
      paste0(flux_name, "_vs_", xvar_name, ".png"))
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

plot_diurnal <- function(dt_flux, split_by_day = FALSE, split_by_expt = FALSE) {

  dt_flux[, date_day := lubridate::round_date(datect, "days")]
  if (split_by_day & split_by_expt) {
    dt_flux[, f_co2_scaled   := scale(f_co2),    by = .(date_day, expt_id, chamber_id)]
    dt_flux[, f_n2o_scaled   := scale(f_n2o),    by = .(date_day, expt_id, chamber_id)]
    dt_flux[, TA_scaled      := scale(TA),       by = .(date_day, expt_id, chamber_id)]
    dt_flux[, TSoil_scaled      := scale(TSoil), by = .(date_day, expt_id, chamber_id)]
    dt_flux[, PPFD_IN_scaled := scale(PPFD_IN),  by = .(date_day, expt_id, chamber_id)]
  } else if (!split_by_day & split_by_expt) {
    dt_flux[, f_co2_scaled   := scale(f_co2),    by = .(expt_id, chamber_id)]
    dt_flux[, f_n2o_scaled   := scale(f_n2o),    by = .(expt_id, chamber_id)]
    dt_flux[, TA_scaled      := scale(TA),       by = .(expt_id, chamber_id)]
    dt_flux[, TSoil_scaled      := scale(TSoil), by = .(expt_id, chamber_id)]
    dt_flux[, PPFD_IN_scaled := scale(PPFD_IN),  by = .(expt_id, chamber_id)]
  } else if (split_by_day & !split_by_expt) {
    dt_flux[, f_co2_scaled   := scale(f_co2),   by = date_day]
    dt_flux[, f_n2o_scaled   := scale(f_n2o),   by = date_day]
    dt_flux[, TA_scaled      := scale(TA),      by = date_day]
    dt_flux[, TSoil_scaled      := scale(TSoil),by = date_day]
    dt_flux[, PPFD_IN_scaled := scale(PPFD_IN), by = date_day]
  } else {
    dt_flux[, f_co2_scaled   := scale(f_co2)  ]
    dt_flux[, f_n2o_scaled   := scale(f_n2o)  ]
    dt_flux[, TA_scaled      := scale(TA)     ]
    dt_flux[, TSoil_scaled      := scale(TSoil)     ]
    dt_flux[, PPFD_IN_scaled := scale(PPFD_IN)]  
  }
  
  dt_flux[, h := lubridate::hour(datect) + lubridate::minute(datect) / 60]

  p <- ggplot(dt_flux, aes(h, PPFD_IN_scaled, colour = Variable))
  p <- p + scale_colour_manual(name="Variable",
    values = c("PPFD" = "yellow", "TA" = "red",  "TSoil" = "orange", 
               "CO2 flux" = "green", "N2O flux" = "blue"))
  p <- p + geom_hline(yintercept = 0)
  p <- p + stat_smooth(aes(colour = "PPFD"), method = "gam")
  p <- p + stat_smooth(aes(y = TA_scaled, colour = "TA"), method = "gam")
  p <- p + stat_smooth(aes(y = TSoil_scaled, colour = "TSoil"), method = "gam")
  p <- p + stat_smooth(aes(y = f_co2_scaled * -1, colour = "CO2 flux"), method = "gam")
  p <- p + stat_smooth(aes(y = f_n2o_scaled, colour = "N2O flux"), method = "gam")
  if (split_by_expt) p <- p + facet_wrap(~ expt_id)
  p <- p + xlab("Hour") + ylab("Scaled variation (sd units)")
  p
  return(p)
}

# difference in fit is measure of linearity
get_nonlinearity <- function(dt) {
  m_gam <- gam(log(chi_co2) ~ s(t), data = dt)
  m_lm  <-  lm(log(chi_co2) ~   t,  data = dt)
  # difference in residual variance, sqrt to original units of umol/mol
  nonlinearity <- sqrt(summary(m_lm)$sigma^2 - m_gam$sig2)
  # alternative: do diff -> expected constant value
  # or predict from gam over ~ 5 t intervals
  # look at abs or rel var in these
  # sigma_slope
  # or look at diff in predictions at t = 1000 lm vs gam
  # or comparev 95% CI / sigma_analyser
  # how much more uncertainty than expected?
  # No - nonlinearity is not the problem - expected for large co2 fluxes
  # occurence of concavity or step changes at any point is the implausible issue
  # fit an asymptotic model and compare with high k gam
  # should we log or not?
  return(nonlinearity)
}

plot_chi_with_nonlinearity <- function(dt, save_plot = FALSE) {
  # dt <- l_out_biochar1$dt_chi
  v_mmnt_id <- unique(dt$mmnt_id)
  v_mmnt_id <- sample(v_mmnt_id, 20)
  dt <- dt[mmnt_id %in% v_mmnt_id]
  dt[, nonlin := get_nonlinearity(.SD) * 1000, by = mmnt_id]

  p <- ggplot(dt, aes(t, chi_co2, colour = nonlin > 10, group = mmnt_id))
  p <- p + geom_point() ## WIP setting alpha adds computation time - try without
  p <- p + stat_smooth(method = "lm", colour = "red")
  p <- p + stat_smooth(method = "gam", colour = "blue")
  npc_txt <- geom_text_npc(aes(npcx = 0.5, npcy = 0.9, label = round(nonlin)), size = 4)
  p <- p + npc_txt
  p <- p + facet_wrap(~ reorder(mmnt_id, -nonlin, mean))
  if (save_plot) {
    # use site & expt in first row for file name
    fname <- here("output", dt[1, .(site_id, expt_id)],
      "p_nonlinearity.png")
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}