# Build tables and figures for every state in an election folder, one state at a time.
#
# Usage (from the project root):
#   Rscript R/03_run_states.R                                  # data/simulated
#   Rscript R/03_run_states.R "<dropbox>/26_general" sen
#   Rscript R/03_run_states.R "<dropbox>/26_general" sen GA,PA  # only these states
#   Rscript R/03_run_states.R "<dropbox>/26_primaries" all      # every statewide contest
#
# Each state with a {STATE}/clean/ folder gets output/{STATE}_{contest}/. States
# without the contest are skipped, not treated as errors.

source("R/functions.R")
source("R/figures.R")
source("R/compare.R")

args <- commandArgs(trailingOnly = TRUE)
election_dir <- if (length(args) >= 1) args[1] else "data/simulated"
target_contest <- if (length(args) >= 2) args[2] else "sen"
only_states <- if (length(args) >= 3) str_split_1(args[3], ",") else NULL

state_dirs <- fs::dir_ls(election_dir, type = "directory", regexp = "/[A-Z]{2}$")
state_dirs <- state_dirs[fs::dir_exists(fs::path(state_dirs, "clean"))]
if (!is.null(only_states)) state_dirs <- state_dirs[basename(state_dirs) %in% only_states]
if (length(state_dirs) == 0) stop("No {STATE}/clean/ folders found in ", election_dir)

# Election dates, when the election column can't be trusted: config/{folder}_dates.csv
dates_file <- fs::path("config", paste0(basename(election_dir), "_dates.csv"))
election_dates <- if (fs::file_exists(dates_file)) {
  readr::read_csv(dates_file, col_types = "cc") |> tibble::deframe()
} else {
  character()
}

# "all" runs every statewide office each state has (district races are left out)
jobs <- map(state_dirs, \(d) {
  contests <- if (target_contest == "all") {
    list_contests(fs::path(d, "clean")) |> filter(is_statewide(contest)) |> distinct(contest) |> pull(contest)
  } else {
    target_contest
  }
  tibble(state = basename(d), dir = d, contest = contests)
}) |>
  list_rbind()

# Every run writes to its own output/{YYYY-MM-DD_HH-MM-SS}/, so runs over
# election night never overwrite each other
run_dir <- new_run_dir()
message("Writing this run to ", run_dir)

results <- pmap(jobs, \(state, dir, contest) {
  tryCatch(
    {
      out_dir <- build_race(fs::path(dir, "clean"), contest, out_root = run_dir, election_date = if (state %in% names(election_dates)) election_dates[[state]] else NULL)
      make_figures(out_dir)
      tibble(state, contest, status = "done", output = as.character(out_dir))
    },
    error = \(e) tibble(state, contest, status = "skipped", output = conditionMessage(e))
  )
}) |>
  list_rbind()

readr::write_csv(
  mutate(results, source = election_dir, started = fs::path_file(run_dir)),
  fs::path(run_dir, "run_summary.csv")
)
print(results, n = Inf)

# Cross-state comparison of reporting speed and margin swings, into {run}/comparison/
tryCatch(compare_states(run_dir), error = \(e) message("Comparison skipped: ", conditionMessage(e)))
