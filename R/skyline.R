#' @import ggplot2

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
      "CavityPressure", "CavityTemp"), v_names)
    ## TODO: set instrument-specific units here too
  }

  dt_ghg[, datect := as.POSIXct(round(datect, "secs"))]
  # aggregate to 1 Hz i.e. do 1-sec averaging
  dt_ghg <- dt_ghg[, lapply(.SD, mean), .SDcols = v_names, by = datect]

  # subset to this_date before returning
  dt_ghg <- dt_ghg[this_date == as.POSIXct(lubridate::date(datect))]
  return(dt_ghg)
}

get_ch_position_data <- function(v_fnames) {

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
  dt[, datect := as.POSIXct(round(datect, "secs"))]
  dt <- dt[, lapply(.SD, mean), .SDcols = c("C_Voltage"),  by = datect]
  # convert chamber position voltage to chamber ID
  dt[, chamber_id := as.factor(round(C_Voltage * 0.01, 0))]
  return(dt)
}

get_soilmet_data <- function(v_fnames) {
  if (fs::path_ext(v_fnames[1]) == "csv") l_dt <- lapply(v_fnames, fread,
    na.strings = c("NAN"))
  if (fs::path_ext(v_fnames[1]) == "dat") l_dt <- lapply(v_fnames, read_cs_data)
  dt <- rbindlist(l_dt)

  # standardise time names
  if ("DateTime"  %in% names(dt)) dt[, datect := as.POSIXct(DateTime)]
  if ("TIMESTAMP" %in% names(dt)) dt[, datect := as.POSIXct(TIMESTAMP)]

  dt[, datect := as.POSIXct(round(datect, "mins"))]
  dt[, RECORD := NULL]
  setnames(dt, c("C_Temp_C_Avg", "QR_Avg", "QR_C_Avg"),
    c("TA",        "PPFD_IN", "PPFD_IN_ch"))

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
  dt <- melt(dt,
    id.vars = c("datect", "TA", "PPFD_IN", "PPFD_IN_ch"),
    measure.vars = patterns("VWC", "TSoil", "SoilPerm", "SoilEC"),
    variable.name = "chamber_id",
    value.name = c("SWC", "TS", "SoilPerm", "SoilEC"))
  return(dt)
}

get_data <- function(v_dates, this_site_id = "HRG",
                     this_expt_id = "diurnal1", data_location, l_meta,
                     initial_deadband_width = 150, final_deadband_width = 150,
                     method = "time fit", dryrun = FALSE,
                     save_plots = TRUE, write_all = FALSE) {
  # create directories for output
  pname_csv        <- here("output", this_site_id, this_expt_id, "csv")
  pname_png        <- here("output", this_site_id, this_expt_id, "png")
  # subdirectory for unfiltered data, including deadbands
  pname_csv_unfilt <- here("output", this_site_id, this_expt_id, "csv", "unfilt")
  pname_png_unfilt <- here("output", this_site_id, this_expt_id, "png", "unfilt")
  fs::dir_create(pname_csv)
  fs::dir_create(pname_png)
  fs::dir_create(pname_csv_unfilt)
  fs::dir_create(pname_png_unfilt)


  n_days <- length(v_dates)
  l_dt_chi  <- list()
  l_dt_flux <- list()
  for (i in seq_along(v_dates)) {
    this_date <- v_dates[i]
    print(paste("Processing ", this_date))
    l_files <- check_data_available(this_date, this_site_id, this_expt_id, data_location, l_meta)
    # if no data today, move on to next day
    if (length(l_files$v_fnames_ghg) == 0 || length(l_files$v_fnames_pos) == 0) next

    dt_ghg <- get_ghg_data(l_files$v_fnames_ghg, this_date,
      this_site_id, this_expt_id, l_meta)
    dt_pos <- get_ch_position_data(l_files$v_fnames_pos)
    dt_met <- get_soilmet_data(l_files$v_fnames_met)

    dt <- dt_pos[dt_ghg, on = .(datect = datect), roll = TRUE]
    # remove where chamber_id data is missing
    dt <- dt[!is.na(chamber_id)]
    dt <- dt_met[dt, on = .(chamber_id = chamber_id, datect = datect), roll = TRUE]

    # find unique mmnt_id from sequence of chamber_id
    dt[, seq_id  := rleid(chamber_id)] # enumerate the sequence
    # then enumerate the sequence for a given chamber
    dt[, seq_id := rleid(seq_id), by = chamber_id]
    dt[, mmnt_id := paste(lubridate::date(dt$datect), chamber_id, seq_id, sep = "_")]

    # enumerate the records within a mmnt sequence
    dt[, t := seq_len(.N), by = mmnt_id]
    dt[, n := .N, by = mmnt_id]
    dt <- dt[!is.na(chi_h2o)] # subset to valid ghg data only

    # join with chamber data
    dt_cham <- l_meta$dt_cham[this_site_id == site_id & this_expt_id == expt_id]
    dt_cham <- dt_cham[this_date >= start_date & this_date < end_date]
    dt <- dt[dt_cham, on = .(chamber_id = chamber_id)]
    # remove where ghg data is missing
    dt <- dt[!is.na(datect)]
    # skip if chpos data is invalid - stuck on single value all day
    if (length(unique(dt$chamber_id)) < 2) next
    # remove deadband and plot
    dt <- remove_deadband(dt,
      initial_deadband_width = initial_deadband_width,
      final_deadband_width = final_deadband_width,
      method = method, dryrun = dryrun)
    # re-check how many data are left
    dt[, n_filt := .N, by = mmnt_id]
    # if too few data left (100?), or too many (ch position sensor stuck)
    # remove the whole measurement sequence
    dt <- dt[n_filt > 100 & n_filt < 1800]
    # skip if no data left after filtering
    if (nrow(dt) == 0) next
    
    # save to file and list
    if (dryrun) {
      fname <- paste0(pname_csv_unfilt, "/dt_chi_", lubridate::date(this_date), ".csv")
      p <- plot_data_unfiltered(dt, 
        initial_deadband_width = initial_deadband_width, 
        final_deadband_width = final_deadband_width, this_seq_id = 4)
    } else {
      fname <- paste0(pname_csv, "/dt_chi_", lubridate::date(this_date), ".csv")
    }
    fwrite(dt, file = fname)
    l_dt_chi[[i]] <- dt

    if (save_plots) {
      p <- plot_chi(dt, gas_name = "chi_h2o")
      fname <- paste0(pname_png, "/h2o_", lubridate::date(this_date), ".png")
      ggsave(p, file = fname, type="cairo")
      p <- plot_chi(dt, gas_name = "chi_co2")
      fname <- paste0(pname_png, "/co2_", lubridate::date(this_date), ".png")
      ggsave(p, file = fname, type="cairo")
      p <- plot_chi(dt, gas_name = "chi_ch4")
      fname <- paste0(pname_png, "/ch4_", lubridate::date(this_date), ".png")
      ggsave(p, file = fname, type="cairo")
      p <- plot_chi(dt, gas_name = "chi_n2o")
      fname <- paste0(pname_png, "/n2o_", lubridate::date(this_date), ".png")
      ggsave(p, file = fname, type="cairo")
    }
    # calculate fluxes each day
    dt <- calc_flux(dt, gas_name = "chi_h2o")
    dt <- calc_flux(dt, gas_name = "chi_co2")
    dt <- calc_flux(dt, gas_name = "chi_ch4")
    dt <- calc_flux(dt, gas_name = "chi_n2o")
    # subset to just the first record
    dt_flux <- dt[, .SD[1], by = mmnt_id]

    # save to file and list
    if (dryrun) {
      fname <- paste0(pname_csv_unfilt, "/dt_flux_", lubridate::date(this_date), ".csv")
    } else {
      fname <- paste0(pname_csv, "/dt_flux_", lubridate::date(this_date), ".csv")
    }
    fwrite(dt_flux, file = fname)
    l_dt_flux[[i]] <- dt_flux
  }
  dt_chi  <- rbindlist(l_dt_chi)
  dt_flux <- rbindlist(l_dt_flux)

  if (write_all) {
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

remove_deadband <- function(dt, initial_deadband_width = 150, final_deadband_width = 150,
                            method = c("time fit", "specified deadband only"), dryrun = FALSE) {
  dt[, exclude := FALSE]
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
      form <- formula(t ~ chi_co2 + chi_ch4 + chi_n2o + chi_h2o)
      # very slow on JASMIN:
      # m <- lm(t ~ chi_co2 + chi_ch4 + chi_n2o + chi_h2o + mmnt_id, w = w, data = dt) # nolint
      dt[, t_pred := predict(lm(form, w = w, data = .SD)), by = mmnt_id]
      dt[, t_resid := abs(scale(resid(lm(form, w = w, data = .SD)))), by = mmnt_id]
    } else { ## WIP would the above fail with only one mmnt_id? just in case:
      dt[, t_pred := predict(lm(form, w = w, data = .SD))]
      dt[, t_resid := abs(scale(resid(lm(form, w = w, data = .SD))))]
    }
    dt[t / n > 0.25 & t / n < 0.75, t_resid := 0]
    # set the exclusion criteria
    dt[t < initial_deadband_width | t > start_final_deadband |
      t_resid > 1, exclude := TRUE]
  }
  # unless it is a dryrun, subset the data
  if (!dryrun) dt <- dt[exclude == FALSE]
  return(dt)
}

plot_data_unfiltered <- function(dt_unfilt, initial_deadband_width = 150,
                                 final_deadband_width = 150, this_seq_id = 1) {
  # if the requested seq_id is not available, set to first value in list
  if (this_seq_id %!in% dt_unfilt$seq_id) this_seq_id <- 
    unique(dt_unfilt$seq_id)[1]
  dt1 <- dt_unfilt[this_seq_id == seq_id]
  dt_sfdband <- dt1[, .(start_final_deadband = .SD[1, start_final_deadband]), 
    by = mmnt_id]

  p <- ggplot(dt1, aes(t, chi_co2, colour = exclude))
  p <- p + geom_point(aes(size = t_resid))
  p <- p + facet_wrap(~mmnt_id) + xlim(0, NA)
  p <- p + geom_vline(xintercept = initial_deadband_width)
  p <- p + geom_vline(data = dt_sfdband, aes(xintercept = start_final_deadband))
  
  fname <- here("output", dt1$site_id[1], dt1$expt_id[1], "png", "unfilt",
    paste0("chi_co2_", as.character(lubridate::date(dt1$datect[1])), 
    "_", this_seq_id, ".png"))
  ggsave(p, file = fname, type="cairo")
  
  return(p)
}

plot_chi <- function(dt, gas_name = "chi_n2o",
                     initial_deadband_width = 150, final_deadband_width = 150) {
  p <- ggplot(dt, aes(t, get(gas_name), colour = as.factor(seq_id), group = mmnt_id))
  p <- p + geom_point(alpha = 0.1) ## WIP setting alpha adds computation time - try without
  p <- p + xlim(0, NA) + ylab(gas_name)
  p <- p + facet_wrap(~chamber_id)
  return(p)
}

calc_flux <- function(dt, gas_name = "chi_co2", use_STP = TRUE, PA = 1000, TA = 15) {
  form <- formula(paste(gas_name, "~ t"))
  # use substr to get rid of "chi" in gas name
  flux_var_name  <- paste0("f_",     substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
  sigma_var_name <- paste0("sigma_f_", substr(gas_name, nchar(gas_name) - 2, nchar(gas_name)))
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
  p <- p + facet_wrap(~trmt_id)
  p <- p + ylab(flux_name)
  p <- p + ylim(y_min, y_max)
  p

  fname <- here("output", site_id, expt_id,
    paste0(flux_name, ".png"))
  ggsave(p, file = fname, type="cairo")

  return(p)
}

plot_n2o_flux <- function(dt_flux, flux_name = "f_N2O_dry",
                          sigma_name = "sigma_N2O_dry", this_site_id, this_expt_id,
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
  p <- p + facet_wrap(~trmt_id)
  p <- p + ylab(flux_name)
  p <- p + ylim(y_min, y_max)
  p


  fname <- here("output", site_id, expt_id,
    paste0(flux_name, "_with_Nappl.png"))
  ggsave(p, file = fname, type="cairo")

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

  p <- ggplot(dt_flux, aes(h, f))
  p <- p + geom_point(colour = "yellow")
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
    colour = "dark orange")
  p <- p + geom_hline(yintercept = 0)
  p <- p + stat_smooth(method = "gam")
  p <- p + ylab(flux_name)
  p <- p + ylim(y_min, y_max)
  p


  fname <- here("output", site_id, expt_id,
    paste0(flux_name, "_diurnal.png"))
  ggsave(p, file = fname, type="cairo")

  return(p)
}

plot_flux_vs_xvar <- function(dt_flux, flux_name = "f_co2",
                              sigma_name = "sigma_f_co2", xvar_name = "SWC",
                              colour_name = "trmt_id", facet_name = "trmt_id",
                              site_id, expt_id,
                              y_min = NA, y_max = NA) {

  dt_flux[, f     := get(flux_name)]
  dt_flux[, x     := get(xvar_name)]
  dt_flux[, sigma := get(sigma_name)]
  dt_flux[, ci_lo := f - (sigma * 1.96)]
  dt_flux[, ci_hi := f + (sigma * 1.96)]

  p <- ggplot(dt_flux, aes(x, f, colour = as.factor(get(colour_name))))
  p <- p + geom_point()
  p <- p + geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi))
  p <- p + geom_hline(yintercept = 0)
  p <- p + facet_wrap(~ get(facet_name))
  p <- p + ylab(flux_name) + xlab(xvar_name) + labs(colour = NULL)
  p <- p + ylim(y_min, y_max)
  p

  fname <- here("output", site_id, expt_id,
    paste0(flux_name, "_vs_", xvar_name, ".png"))
  ggsave(p, file = fname, type="cairo")

  return(p)
}

'%!in%' <- function(x,y)!('%in%'(x,y))
