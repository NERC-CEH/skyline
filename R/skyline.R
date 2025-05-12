#' @import ggplot2

# constants
# define the conversion unit between g N and moles of N2O
units::install_unit("mol_n2o", "28 g", "mol wt of N in N2O")
units::install_unit("mol_c_co2_", "12 g", "mol wt of C in CO2")
# units::install_unit(symbol = "ratio", def = "unitless",
  # name = "dimensionless ratio")

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

convert_metadata_csv <- function(
  fname_meta = "data-raw/skyline_meta-data.xlsx",
  v_sheet = c(
    "site",
    "experiment",
    "analyser_time",
    "treatment",
    "management_event",
    "chamber",
    "deadbands",
    "bad_data",
    "ancilliay_timeseries_byChamber"
  )
) {
  v_fname_csv <- here("data-raw", paste0("metadata_", v_sheet, ".csv"))
  for (i in seq_along(v_sheet)) {
    print(paste(i, v_sheet[i]))
    dt <- as.data.table(readxl::read_excel(fname_meta, sheet = v_sheet[i]))
    fwrite(dt, file = v_fname_csv[i])
  }
  return(v_fname_csv)
}

read_metadata <- function(v_fname_csv) {
  dt_site <- fread(v_fname_csv[1])
  dt_expt <- fread(v_fname_csv[2])
  dt_time <- fread(v_fname_csv[3])
  dt_trmt <- fread(v_fname_csv[4])
  dt_mgmt <- fread(v_fname_csv[5])
  dt_cham <- fread(v_fname_csv[6])
  dt_band <- fread(v_fname_csv[7])
  dt_badd <- fread(v_fname_csv[8])
  dt_ancl <- fread(v_fname_csv[9])

  setkeyv(dt_site, "site_id")
  setkeyv(dt_expt, c("site_id", "expt_id"))
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
                                 l_meta) {
  # subset metadata to site and experiment
  dt <- l_meta$dt_expt[
    this_site_id == site_id &
      this_expt_id == expt_id]
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
  dt_ghg <- rbindlist(l_dt, fill = TRUE)

  # subset metadata to site and experiment
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
  } else if (dt_expt$GHG_instrument == "Los Gatos") {
    dt_ghg[, datect := as.POSIXct(Time, format = "%m/%d/%Y %H:%M:%OS")]
    dt_ghg[, chi_co2 := 0] # add dummy variable for co2 - not measured
    dt_ghg[, chi_ch4 := 0] # add dummy variable for methane - not measured
    setnames(dt_ghg, c("[H2O]_ppm", "chi_co2", "chi_ch4", "[N2O]_ppm",
      "GasP_torr", "GasT_C"), v_names, skip_absent = TRUE)
    dt_ghg[, chi_co2 := chi_n2o] # add dummy variable for co2 - not measured
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
  if ("RECORD" %in% names(dt)) dt[, RECORD := NULL]
  setnames(dt, c("C_Temp_C_Avg", "QR_Avg", "QR_C_Avg"),
    c("TA", "PPFD_IN", "PPFD_IN_ch"), skip_absent = TRUE)

  # if missing, add dummy variables - could be improved
  if ("TA" %!in% names(dt)) dt[, TA := NA]
  if ("PPFD_IN" %!in% names(dt)) dt[, PPFD_IN := NA]
  if ("PPFD_IN_ch" %!in% names(dt)) dt[, PPFD_IN_ch := NA]
  if ("VWC" %!in% names(dt)) dt[, VWC := NA]
  if ("TSoil" %!in% names(dt)) dt[, TSoil := NA]
  if ("SoilPerm" %!in% names(dt)) dt[, SoilPerm := NA]
  if ("SoilEC" %!in% names(dt)) dt[, SoilEC := NA]
  # we want VWC as a fraction not a percentage
  if (mean(dt$VWC, na.rm = TRUE) > 1) dt[, VWC := VWC / 100]

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
                     this_expt_id = "diurnal1", l_meta,
                     seq_id_to_plot = 1, diagnostic_plots = FALSE,
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
  if (diagnostic_plots) { # default to FALSE but really helpful for detecting problems with system
    pname_diagnostic_plots <- here("output", this_site_id, this_expt_id, "png", "diagnostic plots")
    fs::dir_create(pname_diagnostic_plots)}

  # subset metadata to site and experiment
  dt_expt <- l_meta$dt_expt[this_site_id == site_id & this_expt_id == expt_id][1]

  # if not provided, use experiment start & end dates from metadata
  if (is.null(v_dates)) {
    start_date <- as.POSIXct(dt_expt$start_date)
    end_date   <- as.POSIXct(dt_expt$end_date)
    v_dates <- seq(from = start_date, to = end_date, by="day")
  }

  n_days <- length(v_dates)
  l_dt_chi  <- list()
  for (i in seq_along(v_dates)) {  #seq_along equivalent to 1:length(v_dates), so processing one day at a time (/per loop)
    this_date <- v_dates[i]
    print(paste("Processing ", this_date))
    l_files <- check_data_available(this_date, this_site_id, this_expt_id, l_meta) # check if data is available (function above)
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

    if (diagnostic_plots) {skyline_diagnostic_plot(dt, this_date, pname_diagnostic_plots)}

    # shift chamber id down to fix lag
    dt <- dt[,chamber_id:=shift(chamber_id, dt_band$t_shift, type = "lag")]

    dt <- dt[!is.na(chamber_id)]  # remove where chamber_id data is missing

    # find unique mmnt_id from sequence of chamber_id
    dt[, seq_id  := rleid(chamber_id)] # enumerate the sequence, run-length id, through the column chamber_id each time it changes the r.leid +1
    # then enumerate the sequence for a given chamber
    dt[, seq_id := rleid(seq_id), by = chamber_id]

    # create measurement id consisting of expt_id, date, chamber id, and sequence id
    dt[, mmnt_id := paste(this_expt_id, lubridate::date(dt$datect),
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
    dt <- identify_deadband(dt,
      initial_deadband_width = dt_band$initial_deadband_width,
      final_deadband_width   = dt_band$final_deadband_width,
      chpos_tolerance_mV = dt_expt$chpos_tolerance_mV,
      t_resid_threshold = dt_band$t_resid_threshold,
      method = method, remove_deadband = FALSE)
    # re-check how many data are left; count only non-excluded points
    dt[, n_filt := sum(!exclude), by = mmnt_id]
    # if too few data left (100?), or too many (ch position sensor stuck)
    # then remove the whole measurement sequence
    dt <- dt[n_filt > n_min & n_filt < 1800]

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

    if (!dryrun) {
      fname <- paste0(pname_csv, "/dt_chi_", lubridate::date(this_date), ".csv")
      fwrite(dt, file = fname)
      if (save_plots){ # for EA to double check deadbands otherwise defaults to FALSE
        p <- plot_data_unfiltered(dt, gas_name = "chi_co2",
              initial_deadband_width = dt_band$initial_deadband_width,
              final_deadband_width   = dt_band$final_deadband_width, seq_id_to_plot = seq_id_to_plot)}
      l_dt_chi[[i]] <- dt
    }

    if (!dryrun & save_plots) {
      # plots with deadbands included
      p <- plot_chi(dt, gas_name = "co2", rm_db = F)
      fname <- paste0(pname_png_unfilt_daily, "/co2_", lubridate::date(this_date), ".png")
      ggsave(p, file = fname, type = "cairo")
      # p <- plot_chi(dt, gas_name = "ch4", rm_db = F)
      # fname <- paste0(pname_png_unfilt_daily, "/ch4_", lubridate::date(this_date), ".png")
      # ggsave(p, file = fname, type = "cairo")
      p <- plot_chi(dt, gas_name = "n2o", rm_db = F)
      fname <- paste0(pname_png_unfilt_daily, "/n2o_", lubridate::date(this_date), ".png")
      ggsave(p, file = fname, type = "cairo")

      # plots with deadbands removed
      p <- plot_chi(dt, gas_name = "co2", rm_db = T)
      fname <- paste0(pname_png_daily, "/co2_", lubridate::date(this_date), ".png")
      ggsave(p, file = fname, type = "cairo")
      # p <- plot_chi(dt, gas_name = "ch4", rm_db = T)
      # fname <- paste0(pname_png_daily, "/ch4_", lubridate::date(this_date), ".png")
      # ggsave(p, file = fname, type = "cairo")
      p <- plot_chi(dt, gas_name = "n2o", rm_db = T)
      fname <- paste0(pname_png_daily, "/n2o_", lubridate::date(this_date), ".png")
      ggsave(p, file = fname, type = "cairo")
    }

  }
  dt_chi  <- rbindlist(l_dt_chi, fill=TRUE)
  # remove days during experiment when no flux measurements
  dt_chi <- dt_chi[!is.na(site_id)]

  if (write_all & !dryrun) {
    # save to files
    fname <- paste0(pname_csv, "/dt_chi_", lubridate::date(v_dates[1]), "_",
      lubridate::date(v_dates[n_days]), ".csv")
    fwrite(dt_chi, file = fname)
  }
  return(dt_chi)
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

identify_deadband <- function(dt, initial_deadband_width = 150, final_deadband_width = 150,
                            chpos_tolerance_mV = 6, t_resid_threshold = 1,
                            method = c("time fit", "specified deadband only"),
                            remove_deadband = FALSE) {

  dt[, exclude := FALSE]
  # exclude if the chamber position voltage is not close to the expected value
  dt[C_mV_residual > chpos_tolerance_mV, exclude := TRUE]
  # add mmnt-specific latter deadband
  dt[, start_final_deadband := n - final_deadband_width, by = mmnt_id]

  # exclude the pre-defined deadband
  dt[t < initial_deadband_width | t > start_final_deadband, exclude := TRUE]

  if (method == "time fit") {
    dt <- dt[!is.na(chi_co2)]
    dt[, w := dbeta(t / n, shape1 = 1.5, shape2 = 1.5)]
    # predict chi_co2, weighted towards the middle
    # and use this to filter out the nonlinear part
    if (length(unique(dt$mmnt_id)) > 1) {
      form <- formula(chi_co2 ~ t)
      # very slow on JASMIN?:
      dt[, t_resid := abs(resid(lm(form, w = w, data = .SD))), by = mmnt_id]
    }
    # Leave middle half of the data untouched - not deadband.
    # In the first and last quarters, exclude if residual is too high.
    dt[(t / n <= 0.25 | t / n >= 0.75) &
    # dt[(t / n <= 0.33 | t / n >= 0.66) &
      t_resid > t_resid_threshold, exclude := TRUE]
  }
  # set weight to zero if excluded
  dt[exclude == TRUE, w := 0]

  # shift time to start after deadband - affects nonlinear fit parameters
  # get first row in mmnt where data not excluded
  dt[, start_t := which(exclude == FALSE, arr.ind=TRUE)[1], by = mmnt_id]
  dt[, t := t - start_t]
  # remove un-needed variables
  dt[, start_t := NULL]
  # recalculate final deadband after t shift for plotting purposes
  dt[, start_final_deadband := max(t) - final_deadband_width, by = mmnt_id]

  # if removing the deadband, subset the data to only those with exclude = FALSE
  if (remove_deadband) dt <- dt[exclude == FALSE]
  return(dt)
}

plot_data_unfiltered <- function(dt_unfilt, gas_name = "chi_co2",
                                 initial_deadband_width = 150,
                                 final_deadband_width = 150, seq_id_to_plot = 1) {
  # if the requested seq_id is not available, set to first value in list
  if (seq_id_to_plot %!in% dt_unfilt$seq_id) seq_id_to_plot <-
    unique(dt_unfilt$seq_id)[1]
  dt1 <- dt_unfilt[seq_id_to_plot == seq_id]

  p <- ggplot(dt1, aes(t, get(gas_name), colour = exclude))
  p <- p + geom_point(aes(size = t_resid))
  p <- p + geom_point()
  # p <- p + facet_wrap(~ mmnt_id) + xlim(0, NA) shifted t so mmnt starts at -t values and initial deadband is at t=0
  p <- p + geom_vline(xintercept = 0) + geom_vline(data = dt1, aes(xintercept = start_final_deadband))
  # p <- p + geom_vline(data = dt_sfdband, aes(xintercept = start_final_deadband))

  fname <- here("output", dt1$site_id[1], dt1$expt_id[1], "png", "unfilt",
    paste0(gas_name, "_", as.character(lubridate::date(dt1$datect[1])),
      "_", seq_id_to_plot, ".png"))
  ggsave(p, file = fname, type = "cairo")
  return(p)
}

plot_chi <- function(dt, gas_name = "n2o", rm_db = F) {
  if (rm_db == TRUE){dt <- dt[w != 0,]} # removes deadbands for plotting if chosen
  p <- ggplot(dt, aes(t, get(paste0("chi_", gas_name)), colour = as.factor(seq_id), group = mmnt_id)) # colour as seq_id allows us to identify if fluxes were day or night and therefore identify any problems
  # p <- p + geom_point(alpha = 0.1) ## WIP setting alpha adds computation time - try without
  p <- p + geom_point() ## WIP setting alpha adds computation time - try without
  p <- p + ylab(gas_name) + scale_x_continuous(breaks = seq(0, max(dt$t), 100))
  p <- p + facet_wrap(~ chamber_id) # plotting by chamber allows us to see if any chambers are particularly problematic
  if (paste0("chi_pred_", gas_name) %in% colnames(dt)) {
    p <- p + geom_line(aes(y = get(paste0("chi_pred_", gas_name))), colour = "red")
  }
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
                          sigma_name = NULL,
                          this_site_id = "HRG", this_expt_id = "diurnal1",
                          l_meta, mult = 1000, y_min = NA, y_max = NA,
                          save_plot = FALSE) {
  # subset metadata to site and experiment
  dt_mgmt <- l_meta$dt_mgmt[
    this_site_id == site_id &
      this_expt_id == expt_id]
  names(dt_mgmt) <- make.names(names(dt_mgmt))

  # subset to just this experiment
  dt <- dt[site_id == this_site_id & expt_id == this_expt_id]

  dt[, f     := get(flux_name) * mult]
  if (!is.null(sigma_name)) {
    dt[, sigma := get(sigma_name) * mult]
    dt[, ci_lo := f - (sigma * 1.96)]
    dt[, ci_hi := f + (sigma * 1.96)]
    if (is.na(y_max)) y_max <- max(dt[, ci_hi])
    if (is.na(y_min)) y_min <- min(dt[, ci_lo])
  }

  p <- ggplot(drop_units(dt), aes(datect, f))
  p <- p + geom_vline(data = dt_mgmt, aes(xintercept = start))
  p <- p + geom_hline(yintercept = 0)
  # p <- p + geom_point(aes(colour = as.factor(chamber_id)))
  p <- p + geom_point(aes(colour = trmt_id))
  if (!is.null(sigma_name)) p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, colour = trmt_id))
  # p <- p + facet_wrap(~ trmt_id)
  p <- p + ylab(flux_name)
  p <- p + stat_smooth()
  if (!is.na(y_max) || !is.na(y_min)) p <- p + ylim(y_min, y_max)

  if (save_plot) {
    fname <- here("output", this_site_id, this_expt_id, "png",
      paste0(flux_name, "_timeseries_with_treatment.png"))
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}


plot_nema_vs_time <- function(dt, this_site_id = "EHD", this_expt_id = "biochar1",
                          l_meta, mult = 1000,
                          save_plot = FALSE) {
  # subset metadata to site and experiment
  dt_mgmt <- l_meta$dt_mgmt[
    this_site_id == site_id &
      this_expt_id == expt_id]
  names(dt_mgmt) <- make.names(names(dt_mgmt))

  # subset to just this experiment
  dt <- dt[site_id == this_site_id & expt_id == this_expt_id & chamber_id == "1"]

  p <- ggplot(drop_units(dt[site_id == this_site_id & expt_id == this_expt_id]), aes(datect, f_n2o))
  p <- p + geom_vline(data = dt_mgmt, aes(xintercept = start))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_point(colour = "black")
  p <- p + geom_point(aes(y = VWC/800), colour = "orange")
  p <- p + geom_point(aes(y = TSoil/300), colour = "red")
  p <- p + geom_line(aes(y = N_ema_010), colour = "blue")
  p <- p + geom_line(aes(y = N_ema_050_4), colour = "green")
  # p <- p + facet_wrap(~ chamber_id)
p
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
                              sigma_name = NULL, xvar_name = "datect",
                              colour_name = "trmt_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = FALSE,
                              mult = 1, y_min = NA, y_max = NA,
                              save_plot = FALSE) {

  dt[, x     := get(xvar_name)]
  dt[, f     := get(flux_name)  * mult]
  if (!is.null(sigma_name)) {
    dt[, sigma := get(sigma_name) * mult]
    dt[, ci_lo := f - (sigma * 1.96)]
    dt[, ci_hi := f + (sigma * 1.96)]
    if (is.na(y_max)) y_max <- max(dt[, ci_hi])
    if (is.na(y_min)) y_min <- min(dt[, ci_lo])
  }
  if (colour_is_factor) {
    p <- ggplot(dt, aes(x, f, colour = as.factor(get(colour_name))))
    p <- p + scale_colour_viridis(option="viridis", discrete = TRUE)
  } else {
    p <- ggplot(dt, aes(x, f, colour = get(colour_name)))
    p <- p + scale_colour_viridis(option="viridis")
  }
  p <- p + geom_point()
  if (!is.null(sigma_name)) p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi))
  p <- p + geom_hline(yintercept = 0)
  #p <- p + facet_wrap(~ get(facet_name))
  if (rows_only) {
    p <- p + facet_wrap(~ get(facet_name), nrow = length(unique(dt[, get(facet_name)])))
  } else {
    p <- p + facet_wrap(~ get(facet_name))
  }
  p <- p + ylab(flux_name) + xlab(xvar_name) + labs(colour = NULL)
  if (!is.na(y_max)) p <- p + ylim(y_min, y_max)

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
  dt_expt <- l_meta$dt_expt[,
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

# remove duplicates from dt_date
##*WIP: not sure if this should be placed earlier?
dt_date[, event_uid := NULL]
dt_date <- unique(dt_date, by = c("trmt_uid", "datect"))

  dt_flux <- dt_date[dt_flux, on = .(trmt_uid = trmt_uid, datect = datect)]

  return(dt_flux)
}

filter_env_vars <- function(dt) {
  # # remove days during experiment when no flux measurements
  # dt <- dt[!is.na(site_id)]
  # # in existing data, these are erroneous data, not actually cold/hot
  # dt[TA <= 0 | TA > 45, TA := NA]
  # dt[PPFD_IN <= 0, PPFD_IN := 0] # cannot be negative
  # dt[VWC > 100, VWC := NA] # cannot be more than 100%
  # # we want VWC as a fraction not a percentage
  if (mean(dt$VWC, na.rm = TRUE) > 1) dt[, VWC := VWC / 100]
  return(dt)
}

filter_fluxes <- function(dt, save_file = FALSE, fname = "dt_flux") {
  # remove days during experiment when no flux measurements
  dt <- dt[!is.na(site_id)]
  # in existing data, these are erroneous data, not actually cold/hot
  dt[TA <= 0 | TA > 45, TA := NA]
  dt[PPFD_IN <= 0, PPFD_IN := 0] # cannot be negative
  dt[VWC > 100, VWC := NA] # cannot be more than 100%

  # crude filtering of extreme outliers; units of umol/m2/s
  # add threshods as arguments
  dt <- dt[f_co2 > -50 & f_co2 < 50]
  dt <- dt[f_n2o > -0.01 & f_n2o < 0.1]
  dt <- dt[rmse_f_n2o < 0.021]
  if (save_file) fwrite(dt, file = here("output", paste0(fname, ".csv")))
  if (save_file)  qsave(dt, file = here("output", paste0(fname, ".qs")))
  return(dt)
}

filter_fluxes2 <- function(dt, save_file = FALSE, fname = "dt_flux", rmse_threshold = 3) { # second option for filtering fluxes for EA without altering code by PL
  # remove days during experiment when no flux measurements
  dt <- dt[!is.na(site_id)]

  # # crude filtering of extreme outliers; units of umol/m2/s
  # # add thresholds as arguments
  dt <- dt[rmse_f_co2 < rmse_threshold]
  # dt <- dt[f_co2 > -50 & f_co2 < 50]
  # dt <- dt[f_n2o > -0.1 & f_n2o < 0.1]
  # dt <- dt[rmse_f_n2o < 0.021]
  if (save_file) fwrite(dt, file = here("output", dt[1, .(site_id, expt_id)], paste0(fname, ".csv")))
  # if (save_file)  qsave(dt, file = here("output", paste0(fname, ".qs")))
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
  dt_cham[, ci_lo := f - (f_se * 1.96)] # replace with crtitical t value f(n)
  dt_cham[, ci_hi := f + (f_se * 1.96)] # replace with crtitical t value f(n)

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

plot_diurnal <- function(dt_flux, split_by_day = FALSE, split_by_expt = FALSE,
  split_by_chamber = FALSE) {

  dt_flux[, date_day := lubridate::round_date(datect, "days")]
  if (split_by_day & split_by_expt & split_by_chamber) {
    dt_flux[, f_co2_scaled   := scale(f_co2),    by = .(date_day, expt_id, chamber_id)]
    dt_flux[, f_n2o_scaled   := scale(f_n2o),    by = .(date_day, expt_id, chamber_id)]
    dt_flux[, TA_scaled      := scale(TA),       by = .(date_day, expt_id, chamber_id)]
    dt_flux[, TSoil_scaled      := scale(TSoil), by = .(date_day, expt_id, chamber_id)]
    dt_flux[, PPFD_IN_scaled := scale(PPFD_IN),  by = .(date_day, expt_id, chamber_id)]
  } else if (!split_by_day & split_by_expt & split_by_chamber) {
    dt_flux[, f_co2_scaled   := scale(f_co2),    by = .(expt_id, chamber_id)]
    dt_flux[, f_n2o_scaled   := scale(f_n2o),    by = .(expt_id, chamber_id)]
    dt_flux[, TA_scaled      := scale(TA),       by = .(expt_id, chamber_id)]
    dt_flux[, TSoil_scaled      := scale(TSoil), by = .(expt_id, chamber_id)]
    dt_flux[, PPFD_IN_scaled := scale(PPFD_IN),  by = .(expt_id, chamber_id)]
  } else if (split_by_day & split_by_expt & !split_by_chamber) {
    dt_flux[, f_co2_scaled   := scale(f_co2),    by = .(date_day, expt_id)]
    dt_flux[, f_n2o_scaled   := scale(f_n2o),    by = .(date_day, expt_id)]
    dt_flux[, TA_scaled      := scale(TA),       by = .(date_day, expt_id)]
    dt_flux[, TSoil_scaled      := scale(TSoil), by = .(date_day, expt_id)]
    dt_flux[, PPFD_IN_scaled := scale(PPFD_IN),  by = .(date_day, expt_id)]
  } else if (split_by_day & !split_by_expt & !split_by_chamber) {
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
  # p
  return(p)
}

# difference in fit is measure of structured variation in the time series, a bad thing
get_gamdiff <- function(dt) {
  # difference in residual variance, sqrt to original units of umol/mol
  dt[exclude == FALSE, sigma_lm  := summary( lm(chi_co2 ~   t,  w = w,       data = .SD))$sigma^2, by = mmnt_id]
  dt[exclude == FALSE, sigma_gam := summary(gam(chi_co2 ~ s(t), weights =  w/mean(w), data = .SD))$scale, by = mmnt_id]
  # small difference is good; big is bad
  dt[, gamdiff := sqrt(sigma_lm - sigma_gam)]
  return(dt)
}

plot_chi_co2_with_rmse <- function(dt, n = 0, add_gam = FALSE, save_plot = FALSE) {
  if (n > 0) {
    dt_mmnt <- dt[exclude == FALSE, .SD[1], by = mmnt_id]
    v_mmnt_id <- dt_mmnt[, mmnt_id]
    v_rmse    <- dt_mmnt[, rmse_f_co2]
    # mmnts with high rmse are more likely to be plotted
    v_mmnt_id <- sample(v_mmnt_id, n, prob = v_rmse)
    dt <- dt[mmnt_id %in% v_mmnt_id]
  }

  if (add_gam) dt[exclude == FALSE, chi_pred_gam := predict(gam(chi_co2 ~ s(t),
    weights = w/mean(w), data = .SD)), by = mmnt_id]

  p <- ggplot(dt[exclude == FALSE], aes(t, chi_co2, colour = rmse_f_co2, group = mmnt_id, weight = w))
  p <- p + geom_point() ## WIP setting alpha adds computation time - try without
  p <- p + geom_line(aes(y = chi_pred_co2), colour = "red", linewidth = 1)
  if (add_gam) p <- p + geom_line(aes(y = chi_pred_gam), colour = "orange", linewidth = 1)
  npc_txt <- geom_text_npc(aes(npcx = 0.5, npcy = 0.9, label = round(rmse_f_co2, 2)), size = 4)
  p <- p + npc_txt
  p <- p + facet_wrap(~ reorder(mmnt_id, -rmse_f_co2, mean))
  p
  if (save_plot) {
    # use site & expt in first row for file name
    fname <- here("output", dt[1, .(site_id, expt_id)],
      "p_nonlinearity.png")
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

get_flux <- function(dt) {
  dt <- calc_flux_nl(dt, gas_name = "co2")
  dt <- calc_flux_ln(dt, gas_name = "ch4")
  dt <- calc_flux_ln(dt, gas_name = "n2o")
  # we only need the first row - flux is duplicated
  dt <- dt[, .SD[1], by = mmnt_id]
  return(dt)
}

calc_flux_nl <- function(dt, gas_name = "co2", use_STP = TRUE, PA = 1000,
                      TA = 15, use_weights = TRUE) {

  # make variable names
  flux_var_name  <- paste0("f_",     gas_name)
  sigma_var_name <- paste0("sigma_f_", gas_name)
  rmse_var_name <- paste0("rmse_f_", gas_name)
  r2_var_name <- paste0("r2_f_", gas_name)
  pred_var_name <- paste0("chi_pred_", gas_name)

  # calculate mean air density for each mmnt if we have the specific data
  if (use_STP) {
    rho <- PA * 100 / (8.31447 * (TA + 273.15))
  } else {
    dt[, rho := PA * 100 / (8.31447 * (TA + 273.15))]
  }
  if (!use_weights) dt[, w := 1]

  form_ln <- formula(paste0("chi_", gas_name, " ~ t"))
  form_nl <- formula(paste0("chi_", gas_name, " ~ t + I(t^2)"))

  # identify whether nonlinear fit using t^2 and t term
  dt[, dchi_dt_2 := coef(lm(form_nl, w = w, data = .SD))[3], by = mmnt_id]
  dt[, dchi_dt := coef(lm(form_ln, w=w, data = .SD))[2], by = mmnt_id]
  dt[, linear := FALSE]  # do nonlinear fit by default
  dt[dchi_dt_2 > 0 & dchi_dt > 0, linear := TRUE] # do linear fit for positive fluxes when convex
  dt[dchi_dt_2 < 0 & dchi_dt < 0, linear := TRUE] # do linear fit for negative fluxes when concave
  dt[, dchi_dt := NULL]

  # do nonlinear fit
  # we need the if block in case there are no cases, which gives an error
  if (any(dt$linear == FALSE)) {
    form <- form_nl
    dt[linear == FALSE, dchi_dt   := coef(lm(form, w = w, data = .SD))[2], by = mmnt_id]
    dt[linear == FALSE, sigma_dchi_dt := summary(lm(form, w = w, data = .SD))$coefficients[2, 2], by = mmnt_id]
    dt[linear == FALSE, (flux_var_name) := dchi_dt        * rho * volume_m3 / area_m2]
    dt[linear == FALSE, (sigma_var_name) := sigma_dchi_dt * rho * volume_m3 / area_m2]
    dt[linear == FALSE, (r2_var_name) := summary(lm(form, w = w, data = .SD))$r.squared, by = mmnt_id]
    dt[linear == FALSE, (rmse_var_name) := summary(lm(form, w = w, data = .SD))$sigma, by = mmnt_id]
    dt[linear == FALSE, (pred_var_name) := predict(lm(form, w = w, data = .SD)), by = mmnt_id]
  }

  # do linear fit
  if (any(dt$linear == TRUE)) {
    form <- form_ln
    dt[linear == TRUE, dchi_dt   := coef(lm(form, w = w, data = .SD))[2], by = mmnt_id]
    dt[linear == TRUE, sigma_dchi_dt := summary(lm(form, w = w, data = .SD))$coefficients[2, 2], by = mmnt_id]
    dt[linear == TRUE, (flux_var_name) := dchi_dt        * rho * volume_m3 / area_m2]
    dt[linear == TRUE, (sigma_var_name) := sigma_dchi_dt * rho * volume_m3 / area_m2]
    dt[linear == TRUE, (r2_var_name) := summary(lm(form, w = w, data = .SD))$r.squared, by = mmnt_id]
    dt[linear == TRUE, (rmse_var_name) := summary(lm(form, w = w, data = .SD))$sigma, by = mmnt_id]
    dt[linear == TRUE, (pred_var_name) := predict(lm(form, w = w, data = .SD)), by = mmnt_id]
  }

  # remove un-needed variables
  dt[, dchi_dt := NULL]
  dt[, sigma_dchi_dt := NULL]
  return(dt)
}

calc_flux_ln <- function(dt, gas_name = "co2", use_STP = TRUE, PA = 1000,
                      TA = 15, use_weights = TRUE) {

  # make variable names
  flux_var_name  <- paste0("f_",        gas_name)
  sigma_var_name <- paste0("sigma_f_",  gas_name)
  rmse_var_name  <- paste0("rmse_f_",   gas_name)
  r2_var_name    <- paste0("r2_f_",     gas_name)
  pred_var_name  <- paste0("chi_pred_", gas_name)

  # calculate mean air density for each mmnt if we have the specific data
  if (use_STP) {
    rho <- PA * 100 / (8.31447 * (TA + 273.15))
  } else {
    dt[, rho := PA * 100 / (8.31447 * (TA + 273.15))]
  }
  if (!use_weights) dt[, w := 1]

  form <- formula(paste0("chi_", gas_name, " ~ t"))
  dt[, dchi_dt   := coef(lm(form, w = w, data = .SD))[2], by = mmnt_id]
  dt[, sigma_dchi_dt := summary(lm(form, w = w, data = .SD))$coefficients[2, 2], by = mmnt_id]

  dt[, (flux_var_name) := dchi_dt        * rho * volume_m3 / area_m2]
  dt[, (sigma_var_name) := sigma_dchi_dt * rho * volume_m3 / area_m2]
  dt[, (r2_var_name) := summary(lm(form, w = w, data = .SD))$r.squared, by = mmnt_id]
  dt[, (rmse_var_name) := summary(lm(form, w = w, data = .SD))$sigma, by = mmnt_id]
  dt[, (pred_var_name) := predict(lm(form, w = w, data = .SD)), by = mmnt_id]

  # remove un-needed variables
  dt[, dchi_dt := NULL]
  dt[, sigma_dchi_dt := NULL]
  return(dt)
}

partition_fluxes <- function(dt, method = c("subtract_nighttime_R", "regression")) {
  method  <- match.arg(method)
  dt[,  month := month(datect)]
  # normalise respiration to 10 deg C
  dt[, dTA := TA - 10]
  dt[, chamber_id := droplevels(chamber_id)]

  if (method == "subtract_nighttime_R") {
    m <- lm(f_co2 ~ dTA, data = dt[light == FALSE])
    k_T <- coefficients(m)[2]
    dt[light == FALSE, dR := dTA * k_T]
    dt[light == FALSE, R_10 := f_co2 - dR]
    dt[light == FALSE, R_10_ch := mean(R_10, na.rm = TRUE), by = .(chamber_id, month)]
    # apply these values to all rows including light
    dt[, R_10_ch := mean(R_10_ch, na.rm = TRUE), by = .(chamber_id, month)]
    dt[, R := R_10_ch + dTA * k_T]
    # calculate GPP by subtracting respiration
    dt[, P := f_co2 - R]
  } else if (method == "regression") {
    # method 2
    form <- formula(f_co2 ~ sqrt(PPFD_IN) + dTA)
    dt <- dt[!is.na(PPFD_IN) & !is.na(dTA)]
    dt[, f_co2_pred := predict(lm(form, data = .SD)), by = .(chamber_id, month)]
    dt[, R_10 := coef(lm(form, data = .SD))[1], by = .(chamber_id, month)]
    dt[, lue  := coef(lm(form, data = .SD))[2], by = .(chamber_id, month)]
    dt[, k_T  := coef(lm(form, data = .SD))[3], by = .(chamber_id, month)]
    # this line is unneccessary duplication
    dt[, f_co2_pred := lue * sqrt(PPFD_IN) + (R_10 + k_T * dTA)]
    dt[, P := lue * sqrt(PPFD_IN)]
    dt[, R := R_10 + k_T * dTA]
  }
  return(dt)
}

switch_sign_co2 <- function(dt,
                    convention_in  = c("biological", "meterological")) {
  convention_in  <- match.arg(convention_in)

  # check input convention
  # plot(f_co2 ~ TA, data = dt[PPFD_IN < 10])
  # plot(f_co2 ~ sqrt(PPFD_IN), data = dt[PPFD_IN > 10])
  slope_T <- lm(f_co2 ~ 0 + TA, data = dt[PPFD_IN < 10])$coefficients
  slope_Q <- lm(f_co2 ~ sqrt(PPFD_IN), data = dt[PPFD_IN > 10])$coefficients[2]

  if (slope_T < 0 & slope_Q > 0) {
    convention <- "biological"
  } else if (slope_T > 0 & slope_Q < 0) {
    convention <- "meterological"
  } else {
    convention <- NA
  }

  if (convention != convention_in) stop("Attempting to switch sign convention
    for CO2, but seems not to be in the sign convention you think it is.
    Check the data.")

  # switch to opposite sign convention
  dt[,  f_co2 := -1 * f_co2]
  return(dt)
}

# dtt <- expand_to_complete_ts(dt)
expand_to_complete_ts <- function(dt,
  cols = c("datect", "chamber_id", "PPFD_IN", "dTA", "VWC", "f_co2", "lue", "R_10", "k_T")
  ) {
  # get complete time series of hourly data for PPFD_IN and dTA
  # start on first full day, end on last full day
  start_ts <- round_date(min(dt$datect), "day") + days(1)
  end_ts   <- round_date(max(dt$datect), "day") - days(1)
  # generate a sequence of POSIXct values with a 1 hour interval
  v_date_byhour <- seq(from = start_ts, to = end_ts, by = "1 hour")

  # number of chambers
  n_chamber <- length(unique(dt$chamber_id))
  # final length should be equal this:
  length(v_date_byhour) * n_chamber
  # create chamber_id column
  v_chamber_id <- rep(unique(dt$chamber_id), each = length(v_date_byhour))

  # repeat the sequence for each chamber
  v_date_byhour <- rep(v_date_byhour, n_chamber)
  dt_time <- data.table(date_byhour = v_date_byhour, chamber_id = v_chamber_id)

  # merge hourly time sequence with desired columns of raw data
  # round time to nearest hour so we can merge with complete time series
  dt[, datect := lubridate::round_date(datect, "hour")]
  # cols <- c("date_byhour", "chamber_id", "PPFD_IN", "dTA", "VWC", "f_co2", "lue", "R_10", "k_T")
  dt <- dt[, ..cols][dt_time, on = .(datect = date_byhour, chamber_id = chamber_id)]

  # get numeric parts of date-time stamp
  dt[, datets := as.numeric(datect)]
  dt[, month := as.numeric(month(datect))]
  dt[, week := as.numeric(week(datect))]
  dt[, hour := as.numeric(hour(datect))]

  # remove erroneous values
  dt[PPFD_IN < 0 , PPFD_IN := 0]
  dt[VWC > 70 , VWC := NA]

  # get hourly means across all chambers for PPFD_IN and dTA
  dt[, PPFD_IN := mean(PPFD_IN, na.rm = TRUE), by = datect]
  dt[,  dTA := mean(dTA, na.rm = TRUE), by = datect]
  dt[,  VWC := mean(VWC, na.rm = TRUE), by = datect]
  # get means by chambers, weekly for CO2 flux, monthly for fitted parameters
  dt[, f_co2 := mean(f_co2, na.rm = TRUE), by = .(chamber_id, week)]
  dt[,  lue  := mean(lue, na.rm = TRUE), by = .(chamber_id, month)]
  dt[,  R_10 := mean(R_10, na.rm = TRUE), by = .(chamber_id, month)]
  dt[,  k_T  := mean(k_T, na.rm = TRUE), by = .(chamber_id, month)]

  return(dt)
}

# dt_gf <- fill_gaps_PPFD_dTA_VWC(dt_ts)
fill_gaps_PPFD_dTA_VWC <- function(dt) {
  # predict PPFD_IN on basis of hour, separately for each week
  # check the data coverage is good enough every week
  # get a hour x week table of data counts
  a_counts <- table(!is.na(dt$PPFD_IN), dt$hour, dt$week)[2, , ]
  a_counts <- a_counts > 0  # are there more than zero?
  a_counts <- a_counts * 1  # convert to numeric count
  a_counts <- colSums(a_counts)

  if (any(a_counts < 10)) { # there are weeks with less than 10 hours with data
    # put the week term inside the smoother so not all independent
    dt[, PPFD_pred := predict(mgcv::gam(PPFD_IN ~ s(hour, bs = "cc", by = week), data = .SD), newdata = .SD)]
  } else {
    # put the week term outside the smoother so all independent
    dt[, PPFD_pred := predict(mgcv::gam(PPFD_IN ~ s(hour, bs = "cc"), data = .SD), newdata = .SD), by = week]
  }
  dt[PPFD_pred < 0 , PPFD_pred := 0] # cannot be negative
  # replace missing values with predictions
  dt[is.na(PPFD_IN), PPFD_IN := PPFD_pred]

  # predict dTA on basis of PPFD and hour, separately for each week
  if (any(a_counts < 10)) { # there are weeks with less than 10 hours with data
    # put the week term inside the smoother so not all independent
    dt[, dTA_pred := predict(mgcv::gam(dTA ~ PPFD_IN + s(hour, bs = "cc", by = week), data = .SD), newdata = .SD)]
  } else {
    # put the week term outside the smoother so all independent
    dt[, dTA_pred := predict(mgcv::gam(dTA ~ PPFD_IN + s(hour, bs = "cc"), data = .SD), newdata = .SD), by = week]
  }

  # predict VWC on basis of time
  dt[, VWC_pred := predict(mgcv::gam(VWC ~ s(datets), data = .SD), newdata = .SD)]
  # replace missing values with predictions
  dt[is.na(dTA), dTA := dTA_pred]
  dt[is.na(VWC), VWC := VWC_pred]
  # remove unneeded columns
  dt[, PPFD_pred := NULL]
  dt[, dTA_pred := NULL]
  dt[, VWC_pred := NULL]
  return(dt)
}

# dtc <- get_cum_f_co2(dtts)
get_cum_f_co2 <- function(dt) {
  setorder(dt, chamber_id, datect)
  interval_length <- difftime(dt$datect[2], dt$datect[1], units = "secs")
  if (interval_length != 3600) stop("I haven't checked this works time_interval
    other than an hour, although it should")
  secs_per_interval <- set_units(as.numeric(interval_length), s)

  # calculate flux terms
  dt[, f_co2_pred := lue * sqrt(PPFD_IN) + (R_10 + k_T * dTA)]
  dt[, P := lue * sqrt(PPFD_IN)]
  dt[, R := R_10 + k_T * dTA]

  # accumulate these
  dt[, P_cum := cumsum(P), by = chamber_id]
  dt[, R_cum := cumsum(R), by = chamber_id]
  dt[, f_co2_cum := cumsum(f_co2_pred), by = chamber_id]
  dt[, f_co2_obs_cum := cumsum(f_co2), by = chamber_id]

  # convert units to g / m^2
  dt[, P_cum := set_units(P_cum, umol_c_co2_/m^2/s)]
  dt[, P_cum := set_units(P_cum * secs_per_interval, g / m^2)]
  dt[, R_cum := set_units(R_cum, umol_c_co2_/m^2/s)]
  dt[, R_cum := set_units(R_cum * secs_per_interval, g / m^2)]
  dt[, f_co2_cum := set_units(f_co2_cum, umol_c_co2_/m^2/s)]
  dt[, f_co2_cum := set_units(f_co2_cum * secs_per_interval, g / m^2)]
  dt[, f_co2_obs_cum := set_units(f_co2_obs_cum, umol_c_co2_/m^2/s)]
  dt[, f_co2_obs_cum := set_units(f_co2_obs_cum * secs_per_interval, g / m^2)]
  return(dt)
}

plot_cum_f_co2 <- function(dt) {
  p <- ggplot(dt, aes(datect, f_co2_cum))
  p <- p + geom_line(aes(y = P_cum), colour = "green")
  p <- p + geom_line(aes(y = R_cum), colour = "red")
  p <- p + geom_line(aes(y = f_co2_cum), colour = "blue")
  p <- p + facet_wrap(~ chamber_id)
  return(p)
}

plot_vwc_response <- function(dt, alpha1 = 4,
    alpha2 = 6, save_plot = FALSE) {

  # testing beta parameters
  # VWC <- seq(0, 1, 0.01)
  # dt_vwc <- data.table(
  #   VWC,
  #   f_vwc = dbeta(VWC, shape1 = alpha1, shape2 = alpha2)
  # )
  # p <- ggplot(dt_vwc, aes(VWC, f_vwc))
  # p <- p + geom_line()
  # p
  dt[, f_n2o_rel := f_n2o/max(f_n2o), by = expt_id]
  dt[, f_vwc := dbeta(VWC, shape1 = alpha1, shape2 = alpha2)]
  dt[, f_vwc := f_vwc / max(f_vwc, na.rm = TRUE)]

  p <- ggplot(drop_units(dt), aes(VWC, f_n2o_rel))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_point(colour = "black")
  p <- p + geom_line(aes(y = f_vwc), colour = "orange")
  p <- p + facet_wrap(~ expt_id)

  if (save_plot) {
    fname <- here("output", "vwc_response.png")
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

plot_T_response <- function(dt,
    x_varname = "TSoil",
    logy = TRUE,
    save_plot = FALSE) {

  # should update filter_fluxes
  dt <- dt[log(f_n2o) > -20 & f_n2o < 0.1]

  p <- ggplot(drop_units(dt), aes(get(x_varname), f_n2o))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_point(aes(colour = expt_id))
  p <- p + stat_smooth(method = "lm")
  p <- p + xlab(x_varname)
  if (logy) p <- p + scale_y_continuous(trans = scales::log_trans())

  if (save_plot) {
    fname <- here("output", "T_response.png")
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}


plot_nema_response <- function(dt, save_plot = FALSE) {

  # should update filter_fluxes
  dt <- dt[log(f_n2o) > -20 & f_n2o < 0.1]

  p <- ggplot(drop_units(dt), aes(N_ema_010, f_n2o))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_point(colour = "black")
  p <- p + geom_point(aes(x = N_ema_025), colour = "blue")
  p <- p + geom_point(aes(x = N_ema_050_4), colour = "green")
  p <- p + stat_smooth(method = "lm")
  if (logy) p <- p + scale_y_continuous(trans = scales::log_trans())
  # p <- p + facet_wrap(~ chamber_id)
p
  if (save_plot) {
    fname <- here("output", this_site_id, this_expt_id, "png",
      paste0(flux_name, "_timeseries_with_treatment.png"))
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}


plot_hist_fn2o <- function(dt, logy = FALSE, save_plot = FALSE) {

  # should update filter_fluxes
  dt <- dt[log(f_n2o) > -20 & f_n2o < 0.1]

  p <- ggplot(drop_units(dt), aes(x = f_n2o))
  if (logy) p <- ggplot(drop_units(dt), aes(x = log(f_n2o)))
  p <- p + geom_histogram()
  p <- p + facet_wrap(~ expt_id)
  p
  if (save_plot) {
    fname <- here("output", "fn2o_histogram_by_expt.png")
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}

add_unique_chamber_id <- function(dt) {
  dt[,  chamber_uid := paste0(expt_id, "_", chamber_id)]
  return(dt)
}

add_unique_chamber_id <- function(dt) {
  dt[,  chamber_uid := paste0(expt_id, "_", chamber_id)]
  return(dt)
}

fit_model <- function(dt) {
  m <- lm(log(f_n2o) ~ f_vwc + TA + TSoil +
    N_ema_010_1 + N_ema_025_1 + N_ema_050_1 +
    N_ema_010_4 + N_ema_025_4 + N_ema_050_4 +
    chamber_uid,
    data = dt)
  summary(m)
  return(m)
}

add_vwc_response <- function(dt, alpha1 = 4,
    alpha2 = 6, save_plot = FALSE) {

  dt[, f_n2o_rel := f_n2o/max(f_n2o), by = expt_id]
  dt[, f_vwc := dbeta(VWC, shape1 = alpha1, shape2 = alpha2)]
  dt[, f_vwc := f_vwc / max(f_vwc, na.rm = TRUE)]

  p <- ggplot(drop_units(dt), aes(VWC, f_n2o_rel))
  p <- p + geom_hline(yintercept = 0)
  p <- p + geom_point(colour = "black")
  p <- p + geom_line(aes(y = f_vwc), colour = "orange")
  p <- p + facet_wrap(~ expt_id)

  if (save_plot) {
    fname <- here("output", "vwc_response.png")
    ggsave(p, file = fname, type = "cairo")
  }
  return(dt)
}

skyline_diagnostic_plot <- function(dt, this_date, pname_diagnostic_plots){ # useful for EA but default to FALSE unless specified
  dt_subset <- dt %>% filter(row_number() %% 15 == 1)

  a <- ggplot(dt_subset, aes(datect, chamber_id)) + geom_point() +
    ggtitle("Chamber Position") + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M")
  b <- ggplot(dt_subset, aes(datect, PPFD_IN_ch)) + geom_point() +
    ggtitle("Chamber PAR") + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M")
  c <- ggplot(dt_subset, aes(datect, TA)) + geom_point() +
    ggtitle("Chamber Temperature") + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M")
  d <- ggplot(dt_subset, aes(datect, chi_co2)) + geom_point() +
    ggtitle("CO2") + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M")
  e <- ggplot(dt_subset, aes(datect, chi_n2o)) + geom_point() +
    ggtitle("N2O") + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M")
  f <- ggplot(dt_subset, aes(datect, chi_h2o)) + geom_point() +
    ggtitle("H2O") + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M")
  g <- ggplot(dt_subset, aes(datect, P_cavity)) + geom_point() +
    ggtitle("CavityPressure") + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M")
  h <- ggplot(dt_subset, aes(datect, T_cavity)) + geom_point() +
    ggtitle("CavityTemp") + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M")

  plot <- ggpubr::ggarrange(a,b,c,d,e,f,g,h)
  plot
  ggsave(paste(pname_diagnostic_plots, "/", lubridate::date(this_date), ".jpg", sep = ""), width = 30, height = 20, units = "cm")

}
