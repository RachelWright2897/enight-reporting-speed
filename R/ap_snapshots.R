# Functions for reading the AP feed: the live race timeline (code/race_timeline.R), reporting
# speed (code/reporting_speed.R), and the AP percent reported in the margin figures' captions.
#
# Input: the timestamped files cbs-enr writes after every AP pull,
#   {election folder}/networks/ap/clean/ap_candidates_{timestamp}.parquet  (one row per candidate x race)
#   {election folder}/networks/ap/clean/ap_contests_{timestamp}.parquet    (one row per race)
# (plus _latest copies of each). AP reports statewide totals only: no counties, no vote methods.

ap_ts_regex <- "\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}"
ap_keys <- c("network", "source_race_id", "fetched_at")

#' The AP clean/ folder: AP_CLEAN_PATH (config.R) if it has any snapshots, else the CBS copy, else the MEDSL backup
ap_dir <- function() {
  dirs <- c(if (exists("AP_CLEAN_PATH")) AP_CLEAN_PATH,
            file.path(c(CBS_DATA_PATH, MEDSL_DATA_PATH)[nzchar(c(CBS_DATA_PATH, MEDSL_DATA_PATH))],
                      ELECTION_FOLDER, "networks", "ap", "clean"))
  for (dir in dirs) {
    if (length(list.files(dir, paste0("^ap_contests_", ap_ts_regex, "\\.parquet$"))) > 0) return(dir)
  }
  cli::cli_abort("No AP snapshots in AP_CLEAN_PATH or the CBS or MEDSL folders (networks/ap/clean).")
}

ap_missing_warned <- new.env()

#' AP's count for one race at a given time
#'
#' Takes the newest AP contests snapshot fetched at or before `at` (the newest of all when
#' `at` is NULL), so a figure whose own feed stopped early can say how much of the count it
#' covers. `district` NA means the statewide race. Returns list(pct, total, expected, time):
#' pct on 0-100 as AP reports it, total the votes AP has counted, expected AP's implied total
#' vote (total / pct, NA at 0%), time the snapshot's fetch time, and margin AP's D-R margin
#' (ap_race_margin()). NULL when there's no AP
#' folder, no snapshot by then, or AP doesn't carry the race.
ap_race_at <- function(state, contest, district = NA, at = NULL, election_date = ELECTION_DATE) {
  dir <- tryCatch(ap_dir(), error = \(e) NULL)
  if (is.null(dir)) {
    # say so once per run: without AP the figures quietly fall back to the feed's own
    # last snapshot, which looks like nothing changed
    if (!isTRUE(ap_missing_warned$done)) {
      cli::cli_alert_warning(paste(
        "No AP snapshots found (AP_CLEAN_PATH is {.path {if (exists('AP_CLEAN_PATH')) AP_CLEAN_PATH else '(unset)'}}),",
        "so figures leave out AP's percent reported. Set AP_CLEAN_PATH in ~/.Renviron."
      ))
      ap_missing_warned$done <- TRUE
    }
    return(NULL)
  }
  files <- list.files(dir, paste0("^ap_contests_", ap_ts_regex, "\\.parquet$"), full.names = TRUE)
  if (is.null(at)) {
    at <- as.POSIXct(Inf)
  } else {
    # file names carry the pull time in ET, so skip anything named after `at`; the fetch
    # time inside is checked too, since it can trail the name by a few seconds
    stamps <- stringr::str_extract(basename(files), ap_ts_regex)
    files <- files[stamps <= format(at, "%Y_%m_%d_%H_%M_%S", tz = "America/New_York")]
  }
  for (f in rev(sort(files))) {
    race <- nanoparquet::read_parquet(f) |>
      dplyr::filter(.data$state == .env$state, office == contest,
                    if (is.na(.env$district)) is.na(.data$district) else .data$district %in% .env$district,
                    startsWith(source_race_id, format(as.Date(election_date), "%Y%m%d")))
    # none: AP doesn't carry the race; several: ambiguous (a special alongside the regular race)
    if (nrow(race) != 1) return(NULL)
    if (race$fetched_at <= at) {
      return(list(
        pct = race$pct_reporting, total = race$total_votes,
        expected = if (race$pct_reporting > 0) race$total_votes / (race$pct_reporting / 100) else NA_real_,
        time = race$fetched_at,
        margin = ap_race_margin(sub("ap_contests_", "ap_candidates_", f), race$source_race_id)
      ))
    }
  }
  NULL
}

ap_history_cache <- new.env()

#' AP's D minus R margin (percentage points of all votes) for one race in one candidates file,
#' NA when the file is missing or the race lacks either a Democrat or a Republican
ap_race_margin <- function(cand_file, race_id) {
  if (!file.exists(cand_file)) return(NA_real_)
  cands <- nanoparquet::read_parquet(cand_file, col_select = c("source_race_id", "party", "votes")) |>
    dplyr::filter(source_race_id == race_id)
  if (!all(c("dem", "rep") %in% cands$party) || sum(cands$votes, na.rm = TRUE) == 0) return(NA_real_)
  with(cands, 100 * (sum(votes[party == "dem"], na.rm = TRUE) - sum(votes[party == "rep"], na.rm = TRUE)) /
                sum(votes, na.rm = TRUE))
}

#' When AP first reported a race at `pct` percent or more, as a fetch time (NA if it never did)
#'
#' Reads every AP contests snapshot once per run (cached), so captions can date a percent
#' reported to the pull where AP first reached it rather than to its newest pull.
ap_first_reached <- function(state, contest, district = NA, pct = 99, election_date = ELECTION_DATE) {
  dir <- tryCatch(ap_dir(), error = \(e) NULL)
  if (is.null(dir)) return(as.POSIXct(NA))
  if (!identical(ap_history_cache$dir, dir)) {
    files <- list.files(dir, paste0("^ap_contests_", ap_ts_regex, "\\.parquet$"), full.names = TRUE)
    ap_history_cache$rows <- purrr::map(files, \(f) {
      nanoparquet::read_parquet(f, col_select = c("state", "office", "district", "source_race_id",
                                                  "pct_reporting", "fetched_at"))
    }) |>
      purrr::list_rbind()
    ap_history_cache$dir <- dir
  }
  hits <- ap_history_cache$rows |>
    dplyr::filter(.data$state == .env$state, office == contest,
                  if (is.na(.env$district)) is.na(.data$district) else .data$district %in% .env$district,
                  startsWith(source_race_id, format(as.Date(election_date), "%Y%m%d")),
                  pct_reporting >= .env$pct)
  if (nrow(hits) == 0) as.POSIXct(NA) else min(hits$fetched_at)
}

#' Contests rows for the given AP office codes on election day, with a race ID that starts with
#' the election date (AP's feed can carry other dates): the statewide races (no district), or
#' with `districts = TRUE` the district races (U.S. House, at-large seats as district 1)
ap_races <- function(contests, offices, election_date = ELECTION_DATE, districts = FALSE) {
  dplyr::filter(contests, office %in% offices, is.na(district) != districts,
                startsWith(source_race_id, format(as.Date(election_date), "%Y%m%d")))
}

#' Every AP snapshot since election day for the races of `offices` (statewide, or with
#' `districts = TRUE` district races such as "house"), candidates merged with contests
#'
#' Each timestamp's candidates file is joined to the same timestamp's contests file on
#' network, source_race_id and fetched_at, keeping every column of both. A timestamp
#' missing either file (one still being written) is skipped.
read_ap_snapshots <- function(dir, offices, election_date = ELECTION_DATE, districts = FALSE) {
  stamp <- \(kind) {
    f <- list.files(dir, paste0("^ap_", kind, "_", ap_ts_regex, "\\.parquet$"), full.names = TRUE)
    rlang::set_names(f, stringr::str_extract(basename(f), ap_ts_regex))
  }
  cand_files <- stamp("candidates")
  cont_files <- stamp("contests")
  ts <- intersect(names(cand_files), names(cont_files))
  # Test pulls from before election day carry no votes
  ts <- ts[as.Date(substr(ts, 1, 10), "%Y_%m_%d") >= as.Date(election_date)]
  if (length(ts) == 0) cli::cli_abort("No AP snapshots from {election_date} on.")

  purrr::map(ts, \(t) {
    contests <- ap_races(nanoparquet::read_parquet(cont_files[[t]]), offices, election_date, districts)
    nanoparquet::read_parquet(cand_files[[t]]) |>
      dplyr::semi_join(contests, by = ap_keys) |>
      dplyr::full_join(contests, by = ap_keys, relationship = "many-to-one")
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(time = lubridate::with_tz(fetched_at, TZ))
}

#' Race x snapshot vote arrivals, for the race timeline
#'
#' A race updates at a snapshot when any candidate's count differs from its previous
#' snapshot. The margin is D minus R, so it is NA for a race without both parties.
ap_race_updates <- function(merged) {
  merged |>
    dplyr::arrange(source_candidate_id) |>
    dplyr::summarise(
      total = sum(votes, na.rm = TRUE),
      a = sum(votes[party %in% "dem"]),
      b = sum(votes[party %in% "rep"]),
      two_party = all(c("dem", "rep") %in% party),
      sig = paste(votes, collapse = ","),
      .by = c(source_race_id, state, office, district, race_name, special, time)
    ) |>
    dplyr::arrange(source_race_id, time) |>
    dplyr::mutate(
      changed = dplyr::if_else(dplyr::row_number() == 1, total > 0, sig != dplyr::lag(sig)),
      delta = total - dplyr::lag(total, default = 0),
      delta_a = a - dplyr::lag(a, default = 0),
      delta_b = b - dplyr::lag(b, default = 0),
      .by = source_race_id
    ) |>
    dplyr::filter(changed) |>
    dplyr::transmute(
      source_race_id, state, office, district, race_name, special, time,
      votes_added = pmax(delta, 0),
      votes_removed = pmax(-delta, 0),
      # Margin among the votes that arrived in this update
      update_margin = dplyr::if_else(two_party & delta > 0, 100 * (delta_a - delta_b) / delta, NA_real_)
    )
}

# ---- For code/reporting_speed.R ------------------------------------------------

CONTESTS <- c("sen", "gov", "sos")

# When each state's polls closed (ET), one row per state; the plots count hours from here
read_poll_close <- function(path = file.path(CONFIG_DIR, paste0(ELECTION_FOLDER, "_poll_close.csv"))) {
  d <- readr::read_csv(path, col_types = readr::cols_only(state = readr::col_character(), poll_close_et = readr::col_character()))
  d$time <- lubridate::ymd_hm(d$poll_close_et, tz = "America/New_York")
  d
}

# One row per candidate x race x AP snapshot from election day on: time (ET), state, office,
# district (NA statewide), candidate, party, votes, pct_reporting. Statewide races, or with
# `districts = TRUE` district races (offices = "house")
read_ap_votes <- function(dir = ap_dir(), offices = CONTESTS, election_date = ELECTION_DATE, districts = FALSE) {
  read_ap_snapshots(dir, offices, election_date, districts) |>
    dplyr::transmute(time, state, office, district, candidate, party, votes, pct_reporting) |>
    dplyr::distinct()
}
