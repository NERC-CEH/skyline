here::i_am("./run.R")
library(targets)
tar_source()
lapply(v_pkgs, require, character.only = TRUE)

Sys.setenv(TAR_PROJECT = "biochar1")
tar_outdated()
tar_make()
tar_read(test)
dt_flux <- tar_read(dt_flux)
names(dt_flux)
summary(dt_flux$f_n2o)
hist(dt_flux$f_n2o)
plot_flux_vs_xvar(dt_flux, flux_name = "chi_n2o",
                              sigma_name = "sigma_f_n2o", xvar_name = "datect",
                              colour_name = "trmt_id", facet_name = "trmt_id",
                              colour_is_factor = TRUE, rows_only = FALSE,
                              mult = 1, y_min = NA, y_max = NA,
                              save_plot = FALSE)

get_biological_flux <- function(P_max = 20, lue = 0.05, R_10 = 5, k_T = 0.06931472,
  PPFD_IN, dTA = 0, Q_asymptotic = FALSE, T_exponential = FALSE, convention = c("biological", "micrometerological")) {
  convention <- match.arg(convention)

  if (Q_asymptotic == TRUE) {
    P <- (P_max * lue * PPFD_IN) /
      (P_max + lue * PPFD_IN)
  } else {
    P <- lue * sqrt(PPFD_IN)
  }

  if (T_exponential == TRUE) {
    R <- R_10 * exp(k_T * dTA)
  } else {
    R <- R_10 + k_T * dTA
  }

  if (convention == "micrometerological") {
    Fc <- R - P # predicted flux, sign convention to micrometerological
  } else {
    Fc <- P - R # predicted flux, sign convention to biological
  }

  return(Fc)
}


PPFD_IN <- seq(0, 2000, 200)
get_photosynthesis(lue = 0.2, R_10 = 5, PPFD_IN)

get_photosynthesis <- function(lue, R_10,
  PPFD_IN, Q_asymptotic = FALSE) {

  if (Q_asymptotic == TRUE) {
    P <- (P_max * lue * PPFD_IN) /
      (P_max + lue * PPFD_IN)
  } else {
    P <- lue * sqrt(PPFD_IN) - R_10
  }
  return(P)
}

get_respiration <- function(R_10 = 5, k_T = 0.06931472,
  dTA, T_exponential = FALSE) {

  if (T_exponential == TRUE) {
    R <- R_10 * exp(k_T * dTA)
  } else {
    R <- R_10 + k_T * dTA
  }
  return(R)
}


get_biological_flux(P_max = 20, lue = 0.5, R_10 = 5, k_T = 0.06931472,
  PPFD_IN = seq(0, 2000, 200), TA = 10)
get_biological_flux(P_max = 20, lue = 0.5, R_10 = 5, k_T = 0.06931472,
  PPFD_IN = 1000, dTA = seq(-20, 20, 2), convention = "biological")
get_biological_flux(P_max = 20, lue = 0.5, R_10 = 5, k_T = 0.06931472,
  PPFD_IN = seq(0, 2000, 200), TA = 10, convention = "micrometerological")

get_rmse <- function(v_theta) {
  # Other variables cannot be passed as arguments, so must exist in the parent
  # environment from which this is called. The following are referred to:
  # dt: a data table containing the raw EC time series data with u, v, w, T, chi_co2, chi_h2o
  # PPFD_IN: photosynthetic photon flux density umol/m2/s, single numeric value
  # TA: air temperature oC, single numeric value

  # dt <- copy(dt) # need to copy data table to avoid altering original by reference

  # unpack vector into named parameters, avoiding referencing by position
  P_max     <- v_theta[1]
  lue       <- v_theta[2]
  R_10      <- v_theta[3]
  k_T       <- v_theta[4]

  # biological process
  dt[, v_resid := f_co2 - get_biological_flux(
    P_max = 0, lue = 0.5, R_10, k_T,
    PPFD_IN = PPFD_IN, dTA = dTA, convention = "biological")]

  rmse <- sqrt(mean(dt$v_resid^2, na.rm = TRUE))
  return(rmse)
}

get_rmse_P <- function(v_theta) {
  lue       <- v_theta[1]
  R_10      <- v_theta[2]
  # biological process
  dt[, v_resid := f_co2 - get_photosynthesis(
    lue = lue, R_10 = R_10,
    PPFD_IN = PPFD_IN)]

  rmse <- sqrt(mean(dt$v_resid^2, na.rm = TRUE))
  return(rmse)
}

m <- lm(f_co2 ~ sqrt(PPFD_IN) + dTA, data = dt)
m <- lm(f_co2 ~ sqrt(PPFD_IN), data = dt[light == TRUE])
coef(m)

v_theta_ini <- c(P_max = 20, lue = 0.5, R_10 = 5, k_T = 0.06931472)
v_theta_ini <- c(P_max = 0, lue = coef(m)[2], R_10 = -1*coef(m)[1], k_T = 0.06931472)
get_rmse(v_theta_ini)
get_rmse(fit$par)


dt[, f_co2_pred := get_photosynthesis(
    v_theta_ini[2], v_theta_ini[3],
    PPFD_IN = PPFD_IN)]

dt[, f_co2_pred := get_photosynthesis(
    fit$estimate[1], fit$estimate[2],
    PPFD_IN = PPFD_IN)]

dt[, f_co2_pred := get_biological_flux(
    v_theta_ini[1], v_theta_ini[2], v_theta_ini[3], v_theta_ini[4],
    PPFD_IN = PPFD_IN, dTA = dTA)]

dt[, f_co2_pred := get_biological_flux(
    fit$par[1], fit$par[2], fit$par[3], fit$par[4],
    PPFD_IN = PPFD_IN, dTA = dTA)]

dt[, week := lubridate::week(datect)]
dt[, week := as.factor(week)]
dt[, month := as.factor(month)]
dt[VWC > 100, VWC := NA]

p <- ggplot(dt[light == FALSE], aes(dTA, f_co2, colour = week))
p <- ggplot(dt[chamber_id == 4], aes(PPFD_IN, f_co2, colour = VWC))
p <- p + geom_hline(yintercept = 0)
p <- p + geom_point()
p <- p + geom_line(aes(y = f_co2_pred))
p <- p + geom_line(aes(y = P), colour = "green")
p <- p + geom_line(aes(y = R), colour = "red")
p <- p + facet_wrap(~ month)
p <- p + scale_colour_viridis(option="viridis")
p

# minimise the RMSE
m <- lm(f_co2 ~ sqrt(PPFD_IN), data = dt)
coef(m)
v_theta_ini <- c(P_max = 0, lue = coef(m)[2], R_10 = -1*coef(m)[1], k_T = 0.06931472)

get_rmse_P(v_theta_ini[2:3])

fit <- optim(v_theta_ini[2:3], get_rmse_P,
    method = "L-BFGS-B",
    lower = 0, control = list(trace = 3))

  method = "Nelder-Mead",

fit <- optim(v_theta_ini, get_rmse,
    method = "L-BFGS-B",
    lower = 0, control = list(factr = 1e-15, trace = 3))
    control = list(factr = 1e-10))
fit <- optim(v_theta_ini, get_rmse,
  method = "Nelder-Mead",
  control = list(maxit = 10000, trace = 3))

# just a check
switch_sign_co2(dt, convention_in = "meterological")

dt <- dt[month == 5]
dt <- dt[month < 8 & !is.na(f_co2) & chamber_id == 1]
dt <- dt[light == FALSE]
dt <- dt[f_co2 > 0]

lapply(dt[, unique(chamber_id)], function(.chamber_id) {
    get_biological_flux(dt[chamber_id == .chamber_id,
    c(nm1, nm2), with=FALSE])})

install.packages("lbfgsb3c")
library(lbfgsb3c)

fit <- lbfgsb3(v_theta_ini, get_rmse, control = list(factr = 1e-10))
fit <- lbfgsb3(v_theta_ini[2:3], get_rmse_P) # , control = list(factr = 1e-10))
fit

fit <- nlm(get_rmse_P, v_theta_ini[2:3])
fit$par <- fit$estimate

fit <- nls(f_co2 ~ lue * sqrt(PPFD_IN) - (R_10 + k_T * dTA),
                 data = dt,
                 start = list(lue = 0.5, R_10 = 15, k_T = 0.069))
                #  algorithm = "default")
str(fit)
summary(fit)

### start here

form <- formula(f_co2 ~ sqrt(PPFD_IN) + dTA)
m <- lm(form, data = dt)
m
sum(is.na(dt$PPFD_IN))
sum(is.na(dt$dTA))
dt <- dt[!is.na(PPFD_IN) & !is.na(dTA)]
dt[, f_co2_pred := predict(lm(form, data = .SD)), by = .(chamber_id, month)]
dt[, R_10 := coef(lm(form, data = .SD))[1], by = .(chamber_id, month)]
dt[, lue  := coef(lm(form, data = .SD))[2], by = .(chamber_id, month)]
dt[, k_T  := coef(lm(form, data = .SD))[3], by = .(chamber_id, month)]
dt[, f_co2_pred := lue * sqrt(PPFD_IN) + (R_10 + k_T * dTA)]
dt[, P := lue * sqrt(PPFD_IN)]
dt[, R := R_10 + k_T * dTA]

expand_to_complete_ts <- function(dt,
  cols = c("date_byhour", "chamber_id", "PPFD_IN", "dTA", "VWC", "f_co2", "lue", "R_10", "k_T")
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
  # cols <- c("date_byhour", "chamber_id", "PPFD_IN", "dTA", "VWC", "f_co2", "lue", "R_10", "k_T")
  dt_pred <- dt[, ..cols][dt_time, on = .(date_byhour = date_byhour, chamber_id = chamber_id)]

  # get numeric parts of date-time stamp
  dt_pred[, datect := date_byhour]
  dt_pred[, date_byhour := NULL]
  dt_pred[, datets := as.numeric(datect)]
  dt_pred[, month := as.numeric(month(datect))]
  dt_pred[, week := as.numeric(week(datect))]
  dt_pred[, hour := as.numeric(hour(datect))]

  # remove erroneous values
  dt_pred[PPFD_IN < 0 , PPFD_IN := 0]
  dt_pred[VWC > 70 , VWC := NA]

  # get hourly means across all chambers for PPFD_IN and dTA
  dt_pred[, PPFD_IN := mean(PPFD_IN, na.rm = TRUE), by = datect]
  dt_pred[,  dTA := mean(dTA, na.rm = TRUE), by = datect]
  dt_pred[,  VWC := mean(VWC, na.rm = TRUE), by = datect]
  # get means by chambers, weekly for CO2 flux, monthly for fitted parameters
  dt_pred[, f_co2 := mean(f_co2, na.rm = TRUE), by = .(chamber_id, week)]
  dt_pred[,  lue  := mean(lue, na.rm = TRUE), by = .(chamber_id, month)]
  dt_pred[,  R_10 := mean(R_10, na.rm = TRUE), by = .(chamber_id, month)]
  dt_pred[,  k_T  := mean(k_T, na.rm = TRUE), by = .(chamber_id, month)]

  sum(is.na(dt_pred$PPFD_IN)); sum(is.na(dt_pred$dTA)); sum(is.na(dt_pred$VWC))
  sum(is.na(dt_pred$f_co2)); sum(is.na(dt_pred$lue)); sum(is.na(dt_pred$R_10)); sum(is.na(dt_pred$k_T))
  return(dt)
}

fill_gaps_PPFD_dTA_VWC <- function(dt) {

dt_pred[, PPFD_pred := predict(mgcv::gam(PPFD_IN ~ s(hour, bs = "cc"), data = .SD), newdata = .SD), by = week]
dt_pred[PPFD_pred < 0 , PPFD_pred := 0]
dt_pred[is.na(PPFD_IN), PPFD_IN := PPFD_pred]
dt_pred[, dTA_pred := predict(mgcv::gam(dTA ~ PPFD_IN + s(hour, bs = "cc"), data = .SD), newdata = .SD), by = week]
dt_pred[, VWC_pred := predict(mgcv::gam(VWC ~ s(datets), data = .SD), newdata = .SD)]
dt_pred[is.na(dTA), dTA := dTA_pred]
dt_pred[is.na(VWC), VWC := VWC_pred]
return(dt_pred)
}

p <- ggplot(dtts[chamber_id == 1],
  aes(datect, VWC, colour = chamber_id))
p <- p + geom_point()
p <- p + geom_line(aes(y = VWC_pred))
p

### done to here
dt_pred[, f_co2_pred := lue * sqrt(PPFD_IN) + (R_10 + k_T * dTA)]
dt_pred[, P := lue * sqrt(PPFD_IN)]
dt_pred[, R := R_10 + k_T * dTA]

p <- ggplot(dt_pred[month == 6 & PPFD_IN > 10],
  aes(PPFD_IN, f_co2, colour = chamber_id))
p <- p + geom_point()
p <- p + geom_line(aes(y = P), colour = "green")
p <- p + geom_line(aes(y = R), colour = "red")
p <- p + geom_line(aes(y = f_co2_pred), colour = "blue")
p <- p + facet_wrap(~ chamber_id)
p

dt_pred[, P_cum := cumsum(P), by = chamber_id]
dt_pred[, R_cum := cumsum(R), by = chamber_id]
dt_pred[, f_co2_cum := cumsum(f_co2_pred), by = chamber_id]
dt_pred[, f_co2_obs_cum := cumsum(f_co2), by = chamber_id]

p <- ggplot(dt_pred,
  aes(datect, f_co2_obs_cum, colour = chamber_id))
p <- p + geom_line()
p <- p + geom_line(aes(y = P_cum), colour = "green")
p <- p + geom_line(aes(y = R_cum), colour = "red")
p <- p + geom_line(aes(y = f_co2_cum), colour = "blue")
p <- p + facet_wrap(~ chamber_id)
p

secs_per_hour <- set_units(60*60, s)
dt_pred[, P_cum := set_units(P_cum, umol_c_co2_/m^2/s)]
dt_pred[, P_cum := set_units(P_cum * secs_per_hour, g / m^2)]
dt_pred[, R_cum := set_units(R_cum, umol_c_co2_/m^2/s)]
dt_pred[, R_cum := set_units(R_cum * secs_per_hour, g / m^2)]
dt_pred[, f_co2_cum := set_units(f_co2_cum, umol_c_co2_/m^2/s)]
dt_pred[, f_co2_cum := set_units(f_co2_cum * secs_per_hour, g / m^2)]
dt_pred[, f_co2_obs_cum := set_units(f_co2_obs_cum, umol_c_co2_/m^2/s)]
dt_pred[, f_co2_obs_cum := set_units(f_co2_obs_cum * secs_per_hour, g / m^2)]

library(openair)
start_ts <- as.POSIXct("2018-06-01 00:00:00", tz = "UTC")
dt <- dt[datect >= start_ts]
df <- timeAverage(dt, avg.time = "hour", start.date = start_ts, type = c("chamber_id"))
df <- timeAverage(dt_pred, avg.time = "hour", start.date = start_ts, type = c("chamber_id"))
dim(dt_pred)
dim(df)
setDT(df)
df
