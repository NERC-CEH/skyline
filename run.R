here::i_am("./run.R")
library(targets)
source("_targets.R")
tar_outdated()
system.time(tar_make())
tar_load_everything()
p_unfilt
p_chi_co2
p_chi_ch4
p_chi_n2o
p_flux_co2
p_flux_ch4
p_flux_n2o

# for debugging
lapply(v_pkgs, require, character.only = TRUE)

l_meta$dt_site
l_meta$dt_expt
l_meta$dt_trmt

length(unique(dt$core_id))
length(unique(dt$site_id))
length(unique(dt[survey == "RAC", core_id]))
dim(dt)
(unique(dt[survey == "RAC", lat]))

tar_manifest(fields = "command") # list the targets
# visualise the network of targets / pipeline workflow
g <- tar_glimpse()
g$height <- 1000; g$width <- "100%"
g
v <- tar_visnetwork(targets_only = TRUE)
v$height <- 1000; v$width <- "100%"
v
tar_visnetwork()

# serial
system.time(tar_make())
# parallel
system.time(tar_make_future(workers = 6L))

use_targets()