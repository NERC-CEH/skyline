library(data.table)
library(ggplot2)
library(units)
library(mgcv)
install.packages("ggpmisc") # for adding nonlin as text to plot
library(ggpmisc) # for adding nonlin as text to plot

# constants
# define the conversion unit between g N and moles of N2O
units::install_unit("mol_n2o", "28 g", "mol wt of N in N2O")
rho <- units::set_units(41.742, mol / m^3) # air density at STP, mol m-3
h <- set_units(0.3, m)
chi_ambient <- set_units(350, nmol_n2o / mol)
n_t <- 60 * 5
v_t <- set_units(1:n_t, s)
sigma_seal <- 0.0


get_ts_chi <- function(
  sigma_anal = set_units(0.5, nmol_n2o / mol),
  flux = set_units(10, nmol_n2o / m^2 / s)
) {
  seal <- 1 - abs(rnorm(1, 0, sigma_seal))
  epsilon_anal <- set_units(rnorm(n_t, 0, sigma_anal), nmol_n2o / mol)

  dchi_dt <- flux / (rho * h) # check correct
  chi_t_flux <- chi_ambient + dchi_dt * v_t
  chi_t <- (chi_t_flux + epsilon_anal) * seal + chi_ambient * (1 - seal)
  dt <- data.table(
    flux = flux,
    sigma_anal = sigma_anal,
    t = v_t,
    chi_t_flux = chi_t_flux,
    chi_t = chi_t
  )
  return(dt)
}


get_ts_chi_wdbands <- function(
  chi_ambient = chi_ambient,
  g_ch_open = set_units(0.5, 1 / s),
  g_ch_closed = set_units(0, 1 / s),
  g_ch_leaky = set_units(0.25, 1 / s),
  g_sf_open = 0,
  g_sf_closed = 1,
  g_sf_leaky = 1,
  t_closed = set_units(100, s),
  t_leaky = set_units(200, s),
  t_open = set_units(250, s),
  sigma_anal = set_units(0.5, nmol_n2o / mol),
  flux = set_units(10, nmol_n2o / m^2 / s)
) {
  epsilon_anal <- set_units(rnorm(n_t, 0, sigma_anal), nmol_n2o / mol)

  dt_g <- data.table(
    t = c(0, t_closed, t_leaky, t_open),
    g_ch = c(g_ch_open, g_ch_closed, g_ch_leaky, g_ch_open),
    g_sf = c(g_sf_open, g_sf_closed, g_sf_leaky, g_sf_open)
  )

  v_g_ch <- dt_g$g_ch[findInterval(v_t, dt_g$t)]
  v_g_sf <- dt_g$g_sf[findInterval(v_t, dt_g$t)]

  chi_ch <- rep(chi_ambient, times = n_t)

  dchi_flux <- dchi_dt_flux * set_units(1, s)

  for (t in 2:n_t) {
    # check correct
    dchi_flux <- flux / (rho * h) * v_g_sf[t] * set_units(1, s)
    dchi_leak <- (chi_ambient - chi_ch[t - 1]) * v_g_ch[t] * set_units(1, s)

    chi_ch[t] <- chi_ch[t - 1] + dchi_flux + dchi_leak
  }
  dt <- data.table(flux = flux, t = v_t, chi_ch = chi_ch)
  return(dt)
}


dt <- get_ts_chi_wdbands(
  chi_ambient = chi_ambient,
  g_ch_open = set_units(0.5, 1 / s),
  g_ch_closed = set_units(0, 1 / s),
  g_ch_leaky = set_units(0.005, 1 / s),
  g_sf_open = 0,
  g_sf_closed = 1,
  g_sf_leaky = 1,
  t_closed = set_units(100, s),
  t_leaky = set_units(200, s),
  t_open = set_units(250, s),
  sigma_anal = set_units(0.5, nmol_n2o / mol),
  flux = set_units(10, nmol_n2o / m^2 / s)
)

p <- ggplot(dt, aes(t, chi_ch))
p <- p + geom_line()
p <- p + geom_point()
p


get_dt_flux <- function(
  sigma_anal = set_units(0.5, nmol_n2o / mol),
  flux = set_units(10, nmol_n2o / m^2 / s),
  n_sim = 50,
  sigma_seal = 0.1
) {
  v_sigma_anal <- seq(from = 0, to = 1, length.out = n_sim)
  v_flux <- set_units(
    seq(from = 0, to = 5, length.out = n_sim),
    nmol_n2o / m^2 / s
  )
  l_dt <- list()
  for (i in 1:n_sim) {
    l_dt[[i]] <- lapply(v_sigma_anal, get_ts_chi, flux = v_flux[i])
    l_dt[[i]] <- rbindlist(l_dt[[i]])
  }
  dt <- rbindlist(l_dt)
  dt <- drop_units(dt)
  dt[, beta := lm(chi_t ~ t, data = .SD)$coef[2], by = .(flux, sigma_anal)]
  dt[,
    r2 := summary(lm(chi_t ~ t, data = .SD))$r.squared,
    by = .(flux, sigma_anal)
  ]
  dt[,
    rmse := summary(lm(chi_t ~ t, data = .SD))$sigma,
    by = .(flux, sigma_anal)
  ]
  # add rmse and r2 here - needs summmary
}

p <- ggplot(dt, aes(sigma_anal, rmse, colour = r2))
p <- ggplot(dt, aes(sigma_anal, r2, colour = rmse))
p <- ggplot(dt, aes(r2, rmse, colour = sigma_anal))
p <- ggplot(dt, aes(flux, r2, colour = sigma_anal))
p <- p + geom_point()
p


# adds analyser noise as rnorm
# make seal random in time, so adds noise as rnorm
# make seal an arima model
# add deadband widths a variable

# non-Bayesian version
# simulate data
# test different metrics for identifying bad seal cf. low flux
# - corr between r2 vs rmse ...

# Bayesian version
# # given prior for true flux, estimate the parameters
# flux
# seal
# sigma_seal
# sigma_anal

dt <- l_out_biochar1$dt_chi
dt[, date_day := lubridate::round_date(datect, "days")]
# dt <- dt[date_day == "2021-05-19" & chamber_id == 1]
v_mmnt_id <- unique(dt$mmnt_id)
v_mmnt_id <- sample(v_mmnt_id, 9)
dt <- dt[mmnt_id %in% v_mmnt_id]


dt <- l_out_biochar1$dt_chi[mmnt_id == "2021-06-01_01_01"]
dt <- l_out_biochar1$dt_chi[mmnt_id == "2021-05-19_01_02"]
plot_chi(dt, gas_name = "chi_ch4")
plot_chi(dt, gas_name = "chi_co2")
plot_chi(dt, gas_name = "chi_co2")
plot_chi(dt, gas_name = "chi_co2")
x <- ts(dt$chi_co2)
plot.ts(x)
auto.arima(x)
auto.arima(x, d = 1)

# difference in fit is measure of linearity
get_nonlinearity <- function(dt) {
  m_gam <- gam(log(chi_co2) ~ s(t), data = dt)
  m_lm <- lm(log(chi_co2) ~ t, data = dt)
  # difference in absolute residual error
  nonlinearity <- sqrt(summary(m_lm)$sigma^2 - m_gam$sig2)
  # or difference in r2
  # nonlinearity <- summary(m_gam)$r.sq - summary(m_lm)$r.squared
  return(nonlinearity)
}

dt <- l_out_digestate1$dt_chi
dt <- l_out_diurnal1$dt_chi
dt <- l_out_yield1$dt_chi
dt <- l_out_biochar1$dt_chi
# dt <- dt[date_day == "2021-05-19" & chamber_id == 1]
v_mmnt_id <- unique(dt$mmnt_id)
v_mmnt_id <- sample(v_mmnt_id, 9)
dt <- dt[mmnt_id %in% v_mmnt_id]
dt[, date_day := lubridate::round_date(datect, "days")]
dt[, nonlin := get_nonlinearity(.SD), by = mmnt_id]

p <- plot_chi(dt, gas_name = "chi_co2")
p <- p + stat_smooth(method = "lm", colour = "red")
p <- p + stat_smooth(method = "gam", colour = "blue")
npc_txt <- geom_text_npc(
  aes(npcx = 0.5, npcy = 0.9, label = round(nonlin * 1e3)),
  size = 4
)
# label <-  textGrob(label = round(nonlin, 2), x = .5, y = 0.9)
# p <- p + annotation_custom(label)
p <- p + npc_txt
p <- p + facet_wrap(~ reorder(mmnt_id, -nonlin, mean))
p

dt[, chi_co2 := scale(chi_co2)]
dt[, nonlin := get_nonlinearity(.SD), by = mmnt_id]

p <- plot_chi(dt, gas_name = "chi_co2")
p <- p + stat_smooth(method = "lm", colour = "red")
p <- p + stat_smooth(method = "gam", colour = "blue")
npc_txt <- geom_text_npc(
  aes(npcx = 0.5, npcy = 0.9, label = round(nonlin, 2)),
  size = 4
)
# label <-  textGrob(label = round(nonlin, 2), x = .5, y = 0.9)
# p <- p + annotation_custom(label)
p <- p + facet_wrap(~ reorder(mmnt_id, -nonlin, mean))
p <- p + npc_txt
p

dt
plot_chi(
  l_out_biochar1$dt_chi[mmnt_id == "2021-05-19_01_02"],
  gas_name = "chi_co2"
)

library(forecast)
str(x)
acf(x)
m_ts <-
  summary(m_ts)
plot(forecast(m_ts, h = 20))

qsave(dt, file = here("output", "dt_filter_test.qs"))
dt <- qread(file = here("output", "dt_filter_test.qs"))


# difference in fit is measure of linearity
get_nonlinearity <- function(dt) {
  m_gam <- gam(log(chi_co2) ~ s(t), data = dt)
  m_lm <- lm(log(chi_co2) ~ t, data = dt)
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

# difference in fit is measure of linearity
get_nonlinearity <- function(dt) {
  m_gam <- gam((chi_co2) ~ s(t), data = dt)
  m_lm <- lm((chi_co2) ~ t, data = dt)
  # difference in residual variance, sqrt to original units of umol/mol
  nonlinearity <- log(sqrt(summary(m_lm)$sigma^2 - m_gam$sig2))
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
  # dt <- qread(file = here("output", "dt_filter_test.qs"))
  v_mmnt_id <- unique(dt$mmnt_id)
  v_mmnt_id <- sample(v_mmnt_id, 20)
  dt <- dt[mmnt_id %in% v_mmnt_id]
  dt[, nonlin := get_nonlinearity(.SD) * 1000, by = mmnt_id]
  dt[, label := round(sigma_f_co2 * 100, 1)]

  p <- ggplot(dt, aes(t, chi_co2, colour = nonlin > 10, group = mmnt_id))
  p <- p + geom_point() ## WIP setting alpha adds computation time - try without
  p <- p + stat_smooth(method = "lm", colour = "red")
  p <- p + stat_smooth(method = "gam", colour = "blue")
  npc_txt <- geom_text_npc(aes(npcx = 0.5, npcy = 0.9, label = label), size = 4)
  p <- p + npc_txt
  p <- p + facet_wrap(~ reorder(mmnt_id, -nonlin, mean), scales = "free_y")
  p <- p + facet_wrap(~ reorder(mmnt_id, -nonlin, mean))
  p <- p + scale_y_continuous(trans = 'log10')
  p

  if (save_plot) {
    # use site & expt in first row for file name
    fname <- here("output", dt[1, .(site_id, expt_id)], "p_nonlinearity.png")
    ggsave(p, file = fname, type = "cairo")
  }
  return(p)
}


plot_data_unfiltered <- function(
  dt_unfilt,
  gas_name = "chi_co2",
  initial_deadband_width = 150,
  final_deadband_width = 150,
  seq_id_to_plot = 1
) {
  dt <- l_out_diurnal1$dt_chi
  dt[, date_day := lubridate::floor_date(datect, "days")]
  unique(dt$date_day)
  dt <- dt[date_day == "2023-05-25"]
  dt[, hour := lubridate::hour(datect)]
  dt[, qday := floor(hour / 6) + 1]
  dt[, hsecs := difftime(datect, hour)]
  dt[, deadband := FALSE]
  dt[t < initial_deadband_width | t > start_final_deadband, deadband := TRUE]

  # if the requested seq_id is not available, set to first value in list
  if (seq_id_to_plot %!in% dt_unfilt$seq_id) {
    seq_id_to_plot <-
      unique(dt_unfilt$seq_id)[1]
  }
  dt1 <- dt_unfilt[seq_id_to_plot == seq_id]
  dt_sfdband <- dt1[,
    .(start_final_deadband = .SD[1, start_final_deadband]),
    by = mmnt_id
  ]

  p <- ggplot(dt[qday == 2], aes(datect, get(gas_name)))
  p <- p + geom_line()
  p <- p + geom_point(aes(colour = deadband))
  p <- p + facet_wrap(~hour, scales = "free_x")
  p
  p <- p + geom_point(aes(size = t_resid))
  p <- p + geom_vline(xintercept = initial_deadband_width)
  p <- p + geom_vline(data = dt_sfdband, aes(xintercept = start_final_deadband))
  fname <- here(
    "output",
    dt1$site_id[1],
    dt1$expt_id[1],
    "png",
    "unfilt",
    paste0(
      gas_name,
      "_",
      as.character(lubridate::date(dt1$datect[1])),
      "_",
      seq_id_to_plot,
      ".png"
    )
  )
  ggsave(p, file = fname, type = "cairo")
  return(p)
}
