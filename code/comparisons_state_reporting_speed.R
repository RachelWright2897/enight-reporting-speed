# Cross-state comparison of every statewide race in an election folder (compare_states() in
# R/compare.R). The per-race figures are the fig*.R scripts beside this one.
#
# Usage (from the repo root; arguments as in R/races.R):
#   Rscript code/comparisons_state_reporting_speed.R                                # CBS (else MEDSL) folder from config.R
#   Rscript code/comparisons_state_reporting_speed.R "<dropbox>/26_primaries"
#   Rscript code/comparisons_state_reporting_speed.R "<dropbox>/26_primaries" gov   # one contest
#   Rscript code/comparisons_state_reporting_speed.R "<dropbox>/26_primaries" all GA,PA
#   Rscript code/comparisons_state_reporting_speed.R "<dropbox>/26_general" house   # every U.S. House district
#
# Writes outputs/compare_states/ for statewide races ("all", the default), and
# outputs/compare_states/{contest}/ for anything else (e.g. compare_states/house/ for every
# U.S. House district), replacing that folder's last figures.
source("setup.R")

run <- race_run()
races <- for_each_race(run)
out_dir <- file.path(OUTPUT_DIR, "compare_states")
if (run$contest != "all") out_dir <- file.path(out_dir, run$contest)
tryCatch(compare_states(races, out_dir, run$folder, CONFIG_DIR),
         error = \(e) cli::cli_alert_danger("Comparison skipped: {conditionMessage(e)}"))
