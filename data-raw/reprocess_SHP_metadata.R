here::i_am("data-raw/reprocess_SHP_metadata.R")
library(here)
library(fs)
library(data.table)
library(readxl)
library(zoo)
# Always use GMT, never BST
Sys.setenv(TZ = "GMT")

dir_out <- "/gws/nopw/j04/ceh_generic/plevy/skyline_data/SHP/chamber_position/"

# dt <- read_excel("data-raw/SHIPTON_2023_metadata V2.1.xlsx")
dt <- fread("data-raw/SHIPTON_2023_chpos.csv")
head(dt)
tail(dt)
summary(dt)
dt[, datect_licor := strptime(CHAMTIME, "%d%b%Y:%H:%M:%S")]
dt[, datect_lgr := strptime(paste(DATE, TIME), "%d%b%Y %H:%M:%S")]

# create a daily sequence covering all experiments in LGR data
first_date <- lubridate::ceiling_date(min(as.POSIXct(dt$datect_lgr)), "day")
last_date <- lubridate::floor_date(max(as.POSIXct(dt$datect_lgr)), "day")
v_date <- seq(from = first_date, to = last_date, by = "days")
n_date <- length(v_date)

for (i in seq_along(v_date)) {
  # subset to one day
  dts <- dt[lubridate::floor_date(dt$datect_lgr, "day") == v_date[i]]
  # create a 1-sec sequence for each day
  first_time <- lubridate::floor_date(v_date[i], "day")
  v_time <- seq(from = first_time, by = "secs", length.out = 60 * 60 * 24)
  dt_time <- data.table(v_time)

  dtt <- dts[dt_time, on = .(datect_lgr = v_time)]
  # need an initial value if missing
  if (is.na(dtt$CHAMBER[1])) {
    dtt$CHAMBER[1] <- 99
  }
  if (is.na(dtt$TEMPK[1])) {
    dtt$TEMPK[1] <- 99
  }
  # carry chamber data over to subsequent seconds
  dtt[, CHAMBER := na.locf(CHAMBER)]
  dtt[, TEMPK := na.locf(TEMPK)]
  #   summary(dtt)
  dt_out <- dtt[
    CHAMBER != 99,
    .(TIMESTAMP = datect_lgr, C_Voltage = CHAMBER, TA = TEMPK - 273.15)
  ]
  fname <- paste0(dir_out, v_date[i], ".csv")
  fwrite(dt_out, fname, dateTimeAs = "write.csv")
}
