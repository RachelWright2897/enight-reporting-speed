# Which races a post-election script runs on, from its command-line arguments, and a loop that
# builds each one. Shared by the margin scripts in code/ (fig4-fig7, comparisons_state_reporting_speed.R).
#
#   Rscript code/<script>.R                                  # CBS (else MEDSL) folder from config.R, every statewide office
#   Rscript code/<script>.R "<dropbox>/26_primaries"
#   Rscript code/<script>.R "<dropbox>/26_primaries" gov     # one contest
#   Rscript code/<script>.R "<dropbox>/26_primaries" all GA,PA
#   Rscript code/<script>.R "<dropbox>/26_general" house TX   # every U.S. House district in TX
#   Rscript code/<script>.R "<dropbox>/26_general" house_17 NY  # one district
#
# The contest is "all" (every statewide office), a statewide office (sen, gov, ...), a district
# office (house), which runs every district the state's files hold, or a district office with
# its number (house_17). States without the contest or without snapshots, and uncontested races,
# are skipped with a warning.

#' The election folder, contest and states to run, and each state's election day
race_run <- function(args = commandArgs(trailingOnly = TRUE)) {
  election_dir <- if (length(args) >= 1) {
    args[1]
  } else {
    roots <- c(CBS_DATA_PATH, MEDSL_DATA_PATH)
    dirs <- file.path(roots, ELECTION_FOLDER)
    dirs <- dirs[nzchar(roots) & fs::dir_exists(dirs)]
    if (length(dirs) == 0) stop("Set CBS_DATA_PATH or MEDSL_DATA_PATH in ~/.Renviron, or pass the election folder")
    dirs[1]
  }
  target_contest <- if (length(args) >= 2) args[2] else "all"
  only_states <- if (length(args) >= 3) str_split_1(args[3], ",") else NULL
  folder <- basename(election_dir)

  state_dirs <- fs::dir_ls(election_dir, type = "directory", regexp = "/[A-Z]{2}$")
  state_dirs <- state_dirs[fs::dir_exists(fs::path(state_dirs, "clean"))]
  if (!is.null(only_states)) state_dirs <- state_dirs[basename(state_dirs) %in% only_states]
  if (length(state_dirs) == 0) stop("No {STATE}/clean/ folders found in ", election_dir)

  # Election day per state, since the election column can be stale: helper_files/{folder}_dates.csv.
  # Without one, every state uses ELECTION_DATE.
  dates_file <- fs::path(CONFIG_DIR, paste0(folder, "_dates.csv"))
  election_dates <- if (fs::file_exists(dates_file)) {
    readr::read_csv(dates_file, col_types = "cc") |> tibble::deframe()
  } else {
    character()
  }

  list(state_dirs = unname(state_dirs), contest = target_contest, folder = folder, election_dates = election_dates)
}

state_election_date <- function(run, state) {
  if (state %in% names(run$election_dates)) run$election_dates[[state]] else ELECTION_DATE
}

#' Builds every race in `run` (build_race()) and calls draw(x) on each; returns the races built.
#' "all" runs every statewide office each state has (district races are left out); a district
#' office such as "house" runs each of its districts, and "house_17" just that one.
for_each_race <- function(run, draw = \(x) NULL) {
  one_district <- str_match(run$contest, "^(.+)_(\\d+)$")
  jobs <- map(run$state_dirs, \(d) {
    state <- basename(d)
    contests <- tryCatch(list_contests(fs::path(d, "clean")), error = \(e) {
      cli::cli_alert_warning("{state} skipped: {conditionMessage(e)}")
      NULL
    })
    if (is.null(contests)) return(NULL)
    races <- if (run$contest == "all") {
      contests |> filter(is_statewide(contest)) |> distinct(contest) |> mutate(district = NA_integer_)
    } else if (!is.na(one_district[, 1])) {
      tibble::tibble(contest = one_district[, 2], district = as.integer(one_district[, 3]))
    } else if (is_statewide(run$contest)) {
      tibble::tibble(contest = run$contest, district = NA_integer_)
    } else {
      # a district office: each district the files hold. A row without a district can't be
      # placed, so it is skipped.
      held <- filter(contests, contest == run$contest)
      if (any(is.na(held$district))) {
        cli::cli_alert_warning("{state} {run$contest}: rows without a district number skipped")
      }
      held |> filter(!is.na(district)) |> distinct(contest, district)
    }
    if (nrow(races) == 0) return(NULL)
    tibble::tibble(state = state, dir = d, contest = races$contest, district = races$district)
  }) |>
    list_rbind()
  if (nrow(jobs) == 0) return(list())

  pmap(jobs, \(state, dir, contest, district) {
    tryCatch(
      {
        x <- build_race(fs::path(dir, "clean"), contest, district, election_date = state_election_date(run, state))
        draw(x)
        x
      },
      error = \(e) {
        what <- if (is.na(district)) contest else paste(contest, district)
        cli::cli_alert_warning("{state} {what} skipped: {conditionMessage(e)}")
        NULL
      }
    )
  }) |>
    compact()
}

#' Calls draw(state, clean_dir, election_date) for each state in `run`
for_each_state <- function(run, draw) {
  walk(run$state_dirs, \(d) {
    state <- basename(d)
    tryCatch(draw(state, fs::path(d, "clean"), state_election_date(run, state)),
             error = \(e) cli::cli_alert_warning("{state} skipped: {conditionMessage(e)}"))
  })
}
