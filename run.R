here::i_am("./run.R")
library(targets)
source("_targets.R")
tar_outdated()
tar_outdated(starts_with("dt_"))
system.time(tar_make(starts_with("dt_")))
system.time(tar_make())
tar_load_everything(strict = FALSE)
p_unfilt
ggsave(p_unfilt,  file = "output/HRG/diurnal1/p_unfilt__2023-06-01.png")
ggsave(p_chi_co2, file = "output/HRG/diurnal1/p_chi_co2_2023-06-01.png")
ggsave(p_chi_ch4, file = "output/HRG/diurnal1/p_chi_ch4_2023-06-01.png")
ggsave(p_chi_n2o, file = "output/HRG/diurnal1/p_chi_n2o_2023-06-01.png")

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
system.time(tar_make_future(workers = 5L))
system.time(tar_make_future(workers = 5L))
system.time(tar_make_future(starts_with("l_"), workers = 5L))

use_targets()