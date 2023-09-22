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
att_from_rmds(path = file.path(here::here("vignettes/use_powerFlux.Rmd")))

pkgload::load_all(".")

# # add the packages requiring importing to package.r
use_tidy_eval()
use_data_table()
use_package("here", "Imports")
use_package("data.table", "Imports")
use_package("ggplot2", "Imports")
use_package("ggforce", "Imports")
use_package("units", "Imports")
use_package("rmarkdown", "Suggests")
use_package("viridis", "Suggests")
use_package("pkgload", "Suggests")
# usethis::use_package("readxl", "Imports")
# usethis::use_package("mgcv", "Imports")
use_import_from("units", "install_unit")
use_import_from("units", "set_units")

roxygen2::roxygenise()
devtools::document()
usethis::use_package_doc()

# name of file to style
fname <- "./R/dsdz.R"
# make a back-up copy
fname_backup <- paste0(tools::file_path_sans_ext(fname),  ".backup_", tools::file_ext(fname))
file.copy(fname, fname_backup, overwrite = TRUE)

# style the file - it gets re-written
style_file(fname, dry = "off", strict = FALSE)
# examine the differences
diffr(fname_backup, fname)
# check how the result conforms to rules with lint
lint(fname)
source(fname)

sinew::makeOxygen(powerFlux::rmse)
sinew::makeOxygen(powerFlux::get_v_t)
sinew::makeOxygen(powerFlux::get_sigma_t)
sinew::makeOxygen(powerFlux::get_ci_flux)
sinew::makeOxygen(powerFlux::get_ci_omega)
sinew::makeOxygen(powerFlux::get_dt_ci)
sinew::makeOxygen(powerFlux::get_percent_detectable)

# to create/overwrite .lintr and edit 
# use_lintr() # if you want to change defaults

getwd()
setwd("..")
#setwd("C:/Users/plevy/Documents/powerFlux")
#devtools::use_data_raw()
system.time(check(manual = FALSE, vignettes = TRUE))

#create("powerFlux") # if it doesn't already exist
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
pack <- "powerFlux"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

build(manual = TRUE, vignettes = TRUE)
system.time(check())
#build()
#build(binary = TRUE)
#check()

getwd()
setwd("..")
install("powerFlux", build_vignettes = TRUE)
#source("powerFlux/R/powerFlux.R")
#install.packages("./rCBED_0.6.tar.g", repos = NULL, type="source")
library(powerFlux)
?powerFlux
?get_ci_flux
?get_ci_omega
browseVignettes("powerFlux")

detach("package:powerFlux", unload=TRUE)
.libPaths()
remove.packages("powerFlux")