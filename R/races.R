# Which races a post-election script runs on, from its command-line arguments, and a loop that
# builds each one. Shared by the margin scripts in code/ (fig4-fig7, comparisons_state_reporting_speed.R).
#
#   Rscript code/<script>.R                                   # CBS (else MEDSL) folder from config.R, every statewide office
#   Rscript code/<script>.R --state GA,PA                     # only these states
#   Rscript code/<script>.R --race gov                        # one contest
#   Rscript code/<script>.R --race sen,gov --state GA         # several
#   Rscript code/<script>.R --race house --state TX           # every U.S. House district in TX
#   Rscript code/<script>.R --race house_17,house_16 --state NY   # some districts
#   Rscript code/<script>.R --folder "<dropbox>/26_primaries" --race gov
#
# --race is "all" (every statewide office, the default), statewide offices (sen, gov, ...), a
# district office (house), which runs every district the state's files hold, or a district office
# with its number (house_17); several are separated by commas. --state takes postal codes, and
# --folder an election folder other than config.R's. States without the contest or without
# snapshots, and uncontested races, are skipped with a warning.
#
# Two options shape the time figures (4-6) and captions (4-7) of every race in the run:
#   --ap off       leave AP out: no AP line in captions, shares and the final margin from the
#                  files alone (default on; see race_context() in R/figure_helpers.R)
#   --end last     run the time axis through the race's last update, instead of leaving off
#                  late updates under 5% of votes (vote_window()); or an ET time to stop at,
#                  "2026-11-14" or "2026-11-06 21:00"
#
# The older positional form still works: folder, then contest, then states:
#   Rscript code/<script>.R "<dropbox>/26_general" sen GA,PA

#' The election folder, contest and states to run, and each state's election day
race_run <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (any(startsWith(args, "--"))) {
    opts <- parse_flags(args, c("folder", "state", "race", "ap", "end"))
  } else {
    opts <- list(folder = args[1], race = args[2], state = args[3])
    opts[is.na(opts)] <- list(NULL)
  }
  election_dir <- if (!is.null(opts$folder)) {
    opts$folder
  } else {
    roots <- c(CBS_DATA_PATH, MEDSL_DATA_PATH)
    dirs <- file.path(roots, ELECTION_FOLDER)
    dirs <- dirs[nzchar(roots) & fs::dir_exists(dirs)]
    if (length(dirs) == 0) stop("Set CBS_DATA_PATH or MEDSL_DATA_PATH in ~/.Renviron, or pass --folder")
    dirs[1]
  }
  set_figure_options(ap = opts$ap, end = opts$end)
  target_contest <- flag_list(opts$race) %||% "all"
  only_states <- flag_list(opts$state, upper = TRUE)
  folder <- basename(election_dir)

  state_dirs <- fs::dir_ls(election_dir, type = "directory", regexp = "/[A-Z]{2}$")
  state_dirs <- state_dirs[fs::dir_exists(fs::path(state_dirs, "clean"))]
  if (!is.null(only_states)) {
    state_dirs <- state_dirs[basename(state_dirs) %in% only_states]
    missing <- setdiff(only_states, basename(state_dirs))
    if (length(missing)) cli::cli_alert_warning("No {{STATE}}/clean/ folder in {election_dir} for {missing}.")
  }
  if (length(state_dirs) == 0) stop("No {STATE}/clean/ folders found in ", election_dir)

  # Election day per state, since the election column can be stale: helper_files/{folder}_dates.csv.
  # Without one, every state uses ELECTION_DATE.
  dates_file <- fs::path(CONFIG_DIR, paste0(folder, "_dates.csv"))
  election_dates <- if (fs::file_exists(dates_file)) {
    readr::read_csv(dates_file, col_types = "cc") |> tibble::deframe()
  } else {
    character()
  }

  list(state_dirs = unname(state_dirs), contest = target_contest, folder = folder, election_dates = election_dates,
       only_states = only_states)
}

#' Stores --ap and --end for the figures (figure_ap(), figure_window() in R/figure_helpers.R)
set_figure_options <- function(ap = NULL, end = NULL) {
  ap <- tolower(ap %||% "on")
  if (!ap %in% c("on", "off")) cli::cli_abort("--ap is on or off, not {.val {ap}}.")
  if (!is.null(end) && !identical(tolower(end), "last")) {
    t <- lubridate::ymd_hms(end, tz = TZ, truncated = 3, quiet = TRUE)
    if (is.na(t)) cli::cli_abort("Can't read --end {.val {end}}; use last, \"2026-11-14\" or \"2026-11-06 21:00\".")
    end <- t
  } else if (!is.null(end)) {
    end <- "last"
  }
  options(enr.use_ap = ap == "on", enr.end = end)
}

state_election_date <- function(run, state) {
  if (state %in% names(run$election_dates)) run$election_dates[[state]] else ELECTION_DATE
}

#' Builds every race in `run` (build_race()) and calls draw(x) on each; returns the races built.
#' For each contest in run$contest: "all" runs every statewide office each state has (district
#' races are left out); a district office such as "house" runs each of its districts, and
#' "house_17" just that one.
for_each_race <- function(run, draw = \(x) NULL) {
  jobs <- map(run$state_dirs, \(d) {
    state <- basename(d)
    contests <- tryCatch(list_contests(fs::path(d, "clean")), error = \(e) {
      cli::cli_alert_warning("{state} skipped: {conditionMessage(e)}")
      NULL
    })
    if (is.null(contests)) return(NULL)
    races <- map(run$contest, \(target) {
      one_district <- str_match(target, "^(.+)_(\\d+)$")
      if (target == "all") {
        contests |> filter(is_statewide(contest)) |> distinct(contest) |> mutate(district = NA_integer_)
      } else if (!is.na(one_district[, 1])) {
        tibble::tibble(contest = one_district[, 2], district = as.integer(one_district[, 3]))
      } else if (is_statewide(target)) {
        tibble::tibble(contest = target, district = NA_integer_)
      } else {
        # a district office: each district the files hold. A row without a district can't be
        # placed, so it is skipped.
        held <- filter(contests, contest == target)
        if (any(is.na(held$district))) {
          cli::cli_alert_warning("{state} {target}: rows without a district number skipped")
        }
        held |> filter(!is.na(district)) |> distinct(contest, district)
      }
    }) |>
      list_rbind() |>
      distinct(contest, district)
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
