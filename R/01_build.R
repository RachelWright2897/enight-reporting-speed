# Build precinct reporting tables from one state's folder of enightdata snapshots,
# into a new output/{YYYY-MM-DD_HH-MM-SS}/ folder for this run.
#
# Usage (from the project root):
#   Rscript R/01_build.R                                   # simulated data
#   Rscript R/01_build.R "<dropbox>/26_general/GA/clean" sen
#   Rscript R/01_build.R "<dropbox>/26_general/TX/clean" house 7

source("R/functions.R")

args <- commandArgs(trailingOnly = TRUE)
build_race(
  out_root = new_run_dir(),
  clean_dir = if (length(args) >= 1) args[1] else "data/simulated/ZZ/clean",
  target_contest = if (length(args) >= 2) args[2] else "sen",
  target_district = if (length(args) >= 3) as.integer(args[3]) else NA_integer_
)
