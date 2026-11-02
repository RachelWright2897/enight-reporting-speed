# Dress rehearsal: simulate one state's election night with a two-party (D vs R)
# Senate race, then run it through every step of the pipeline (tables, all nine
# state figures, cross-state comparison). Everything goes under mock_run/:
#   mock_run/data/simulated/ZM/clean/   simulated snapshots, as cbs-enr names them
#   mock_run/{YYYY-MM-DD_HH-MM-SS}/     this run's output, as 03_run_states.R writes it
#
# Usage (from the project root):
#   Rscript R/05_mock_run.R

source("R/functions.R")
source("R/figures.R")
source("R/compare.R")
source("R/simulate.R")

mock_root <- "mock_run"
data_root <- fs::path(mock_root, "data", "simulated")   # "data/simulated" marks output as simulated

# Six counties, mixed reporting styles; early and mail votes lean D, election day
# leans R, so the count starts with a D lead and ends R (a red shift)
simulate_state(
  "ZM",
  tibble(
    county = c("adams", "burke", "carver", "dane", "elko", "ford"),
    n_prec = c(60, 25, 40, 15, 35, 50),
    style  = c("batch", "steady", "batch", "steady", "steady", "batch"),
    lean   = c(0.55, 0.40, 0.48, 0.35, 0.60, 0.45),
    size   = c(900, 500, 700, 400, 600, 800)
  ),
  seed = 20261102,
  root = data_root
)

run_dir <- new_run_dir(mock_root)
message("Writing this run to ", run_dir)

out_dir <- build_race(fs::path(data_root, "ZM", "clean"), "sen", out_root = run_dir,
                      election_date = "2026-11-03")
make_figures(out_dir)

readr::write_csv(
  tibble(state = "ZM", contest = "sen", status = "done", output = as.character(out_dir),
         source = as.character(data_root), started = fs::path_file(run_dir)),
  fs::path(run_dir, "run_summary.csv")
)
compare_states(run_dir)
