# Cross-state comparison of every statewide race in an election folder (compare_states() in
# R/compare.R). The per-race figures are the fig*.R scripts beside this one.
#
# Usage (from the repo root; arguments as in R/races.R):
#   Rscript code/compare_states.R                                  # CBS (else MEDSL) folder from config.R
#   Rscript code/compare_states.R "<dropbox>/26_primaries"
#   Rscript code/compare_states.R "<dropbox>/26_primaries" gov     # one contest
#   Rscript code/compare_states.R "<dropbox>/26_primaries" all GA,PA
#
# Writes outputs/compare_states/, replacing the last run's figures.
source("setup.R")

run <- race_run()
races <- for_each_race(run)
tryCatch(compare_states(races, file.path(OUTPUT_DIR, "compare_states"), run$folder, CONFIG_DIR),
         error = \(e) cli::cli_alert_danger("Comparison skipped: {conditionMessage(e)}"))
