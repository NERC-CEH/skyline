here::i_am("./run.R")
library(targets)
tar_source()
source("_targets.R")
lapply(v_pkgs, require, character.only = TRUE)

Sys.setenv(TAR_PROJECT = "digestate1")
tar_outdated()
tar_make()
