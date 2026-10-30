# Figures from the tables 01_build.R writes, for one state's race.
#
# Usage (from the project root):
#   Rscript R/02_figures.R                                  # ZZ_sen in the latest run
#   Rscript R/02_figures.R output/2026-11-03_21-05-00/GA_sen

source("R/functions.R")
source("R/figures.R")

args <- commandArgs(trailingOnly = TRUE)
make_figures(if (length(args) >= 1) args[1] else fs::path(latest_run_dir(), "ZZ_sen"))
