here::i_am("./run.R")
library(targets)
# tar_config_set(script = "_targets.R", store = "_targets", project = "main")
# tar_config_set(script = "_targets_biochar1.R", store = "_targets_biochar1", project = "biochar1")
# tar_config_set(script = "_targets_digestate1.R", store = "_targets_digestate1", project = "digestate1")
# tar_config_set(script = "_targets_processing_only.R", store = "_targets_processing_only", project = "processing_only")

source("_targets.R")

Sys.setenv(TAR_PROJECT = "main")
tar_outdated()

Sys.setenv(TAR_PROJECT = "biochar1")
tar_outdated()
tar_make()
tar_read(test)

tar_outdated(starts_with("dt_"))
tar_outdated(starts_with("manuscript_"))
system.time(tar_make(starts_with("p_")))
system.time(tar_make(starts_with("manuscript_")))
system.time(tar_make())
tar_load(starts_with("p_"))
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


# processing only - select experiment in _targets file
Sys.setenv(TAR_PROJECT = "processing_only")
tar_outdated()
tar_make(starts_with("dt_"))



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
