# Compare reporting speed and margin swings across every race in one run.
#
# Usage (from the project root):
#   Rscript R/04_compare_states.R                                # latest run
#   Rscript R/04_compare_states.R output/2026-11-03_21-05-00
#
# 03_run_states.R calls this at the end of every run, so this script is only
# needed to redo the comparison for an earlier run.

source("R/functions.R")
source("R/compare.R")

args <- commandArgs(trailingOnly = TRUE)
compare_states(if (length(args) >= 1) args[1] else latest_run_dir())
