library(targets)
library(tarchetypes)
library(here)
library(qs)

# Always use GMT, never BST
Sys.setenv(TZ = "GMT")
set.seed(448)

# Set target options:
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2",
  "lubridate", "dplyr", "future", "viridis", "lme4", "ggeffects", 
  "photobiology", "mgcv", "ggpmisc")
tar_option_set(
  packages = v_pkgs,
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

data.table::setDTthreads(threads = 1)
data.table::getDTthreads()

list(
  tar_target(fname_meta, "data-raw/skyline_meta-data.xlsx", format = "file"),
  tar_target(l_meta, read_metadata(fname_meta)),
  tar_target(fname_dt_flux, "_targets/objects/dt_flux", format = "file"),
  tar_target(dt_flux, qread(fname_dt_flux)[expt_id == "digestate1"]), 
  tar_target(fname_dt_chi, "_targets/objects/dt_chi_digestate1", format = "file"),
  tar_target(dt_chi, qread(fname_dt_chi)), 
  tar_target(test, summary(dt_flux))
)