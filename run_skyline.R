# sbatch --account=short4hr run_skyline.job
# sbatch run_skyline.job
# R CMD BATCH --no-restore --no-save run_skyline.R output/run_skyline.Rout &

here::i_am("./run_skyline.R")
library(targets)
source("_targets.R")
Sys.setenv(TAR_PROJECT = "main")
tar_outdated()

# serial
system.time(tar_make())
# parallel
# system.time(tar_make_future(workers = 5L))

quit(save = "no")
