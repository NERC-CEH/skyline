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
