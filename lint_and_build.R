# Can install skyline as a package https://www.youtube.com/watch?v=hxit78_ZSco
# need to export all functions called by _targets
# refer to data in inst/extdata
# changes to source code of functions only registers when package re-installed
# make a project for each experiment
# run each one with unfiltered data first, show all without removing deadband
# add shiny app to package

here::i_am("./lint_and_build.R")
library(devtools)
library(usethis)
library(lintr)
library(styler)
library(diffr)
library(sinew)
library(attachment)

desc_file <- attachment::att_amend_desc(path = ".", inside_rmd = TRUE, 
  update.config = TRUE)
# att_from_rmds(path = file.path(here::here("vignettes/use_powerFlux.Rmd")))

pkgload::load_all(".")

# add to .RBuildIgnore
usethis::use_build_ignore(c("data-raw", "output", ".lintr"))

# # add the packages requiring importing to package.r
use_tidy_eval()
use_data_table()
use_package("data.table", "Imports")
use_package("here", "Imports")
use_package("fs", "Imports", min_version = "1.5.0")
use_package("ggplot2", "Imports")
use_package("readxl", "Imports")
# use_package("lubridate", "Imports")
use_import_from("lubridate", "date")
use_import_from("lubridate", "hour")
use_import_from("lubridate", "minute")
# use_package("ggforce", "Imports")
# use_package("units", "Imports")
# use_package("rmarkdown", "Suggests")
# use_package("viridis", "Suggests")
# use_package("pkgload", "Suggests")
# usethis::use_package("readxl", "Imports")
# usethis::use_package("mgcv", "Imports")
# use_import_from("units", "install_unit")
# use_import_from("units", "set_units")

roxygen2::roxygenise()
devtools::document()
# usethis::use_package_doc()

# name of file to style
fname <- "./R/skyline.R"
# make a back-up copy
fname_backup <- paste0(tools::file_path_sans_ext(fname),  ".backup_", tools::file_ext(fname))
file.copy(fname, fname_backup, overwrite = TRUE)

# style the file - it gets re-written
style_file(fname, dry = "off", strict = FALSE)
# examine the differences
diffr(fname_backup, fname)
# check how the result conforms to rules with lint
lint(fname)
# source(fname)

# best to have a .lintr file in the project root with:
# linters: linters_with_defaults(
    # line_length_linter(80), 
    # object_name_linter = NULL,
    # object_usage_linter = NULL
  # )
# exclusions: list(
    # "data-raw/Basic_Figures.R"
  # )
# encoding: "UTF-8"


makeOxyFile(fname)

sinew::makeOxygen(skyline::read_cs_data)
sinew::makeOxygen(skyline::read_metadata)
sinew::makeOxygen(skyline::check_data_available)
sinew::makeOxygen(skyline::get_ghg_data)
sinew::makeOxygen(skyline::get_ch_position_data)
sinew::makeOxygen(skyline::get_soilmet_data)
sinew::makeOxygen(skyline::get_data)
sinew::makeOxygen(skyline::remove_deadband)
sinew::makeOxygen(skyline::plot_data_unfiltered)
sinew::makeOxygen(skyline::plot_chi)
sinew::makeOxygen(skyline::calc_flux)
sinew::makeOxygen(skyline::combine_fluxes)
sinew::makeOxygen(skyline::plot_flux)
sinew::makeOxygen(skyline::plot_n2o_flux)
sinew::makeOxygen(skyline::plot_n2o_flux_diurnal)
sinew::makeOxygen(skyline::plot_flux_vs_xvar)

# to create/overwrite .lintr and edit 
# use_lintr() # if you want to change defaults

getwd()
setwd("..")
#setwd("C:/Users/plevy/Documents/skyline")
#devtools::use_data_raw()
system.time(check(manual = FALSE, vignettes = TRUE))

#create("skyline") # if it doesn't already exist
# Add functions in files to R/ directory 

#devtools::use_data(projlonlat, projOSGB, projUTM, overwrite = TRUE)
#devtools::use_data(ch4BySector, co2BySector, n2oBySector, internal = TRUE)
#C:/0Peter/curr/ECsystem/Footprint
document()
#devtools::document()
check_man()
#use_vignette("use_CBED")
clean_vignettes()
build_vignettes()

# build the manual
#Sys.getenv(c("R_TEXI2DVICMD", "R_PAPERSIZE", "RD2PDF_INPUTENC"))
#Sys.setenv(RD2PDF_INPUTENC = "inputenx ")
pack <- "skyline"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

system.time(build(manual = TRUE, vignettes = FALSE))
system.time(check(vignettes = FALSE))
#build()
#build(binary = TRUE)
#check()

getwd()
setwd("..")
install("skyline", build_vignettes = FALSE)
#source("skyline/R/skyline.R")
#install.packages("./rCBED_0.6.tar.g", repos = NULL, type="source")
library(skyline)
?skyline
?get_ci_flux
?get_ci_omega
browseVignettes("skyline")

detach("package:skyline", unload=TRUE)
.libPaths()
remove.packages("skyline")