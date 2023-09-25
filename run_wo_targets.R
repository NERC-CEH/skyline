here::i_am("./run.R")
library(targets)
Sys.setenv(TZ = "GMT")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

tar_outdated()

# for debugging
v_pkgs = c("here", "fs", "data.table", "readxl", "units", "qs", "ggplot2")
lapply(v_pkgs, require, character.only = TRUE)

# site, experiment, and dates to process:
data_location <- "local drive"
site_id <- "EH"
expt_id <- "digestate1"
start_date <- "2023-03-28"
end_date   <- "2023-08-31"
example_date   <- "2023-04-04"
v_dates <- as.POSIXct(seq(from = as.Date(start_date), to = as.Date(end_date), by="day"))
save_plots <- FALSE
this_date <- example_date

fname_meta <- "data-raw/skyline_meta-data.xlsx"
l_meta <- read_metadata(fname_meta)

l_files <- check_data_available(this_date, site_id, expt_id, data_location, l_meta)
length(l_files$v_fnames_ghg)

dt_ghg <- get_data(v_dates[1], site_id, expt_id, data_location, l_meta)
dt_unfilt <- remove_deadband(dt_ghg, method = "time fit", dryrun = TRUE)
dt <- remove_deadband(dt_ghg, method = "time fit")
plot_data(dt)
