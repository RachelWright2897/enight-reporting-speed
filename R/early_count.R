# Checks on whether a race's state files can show the early count, shared by
# code/margin_direction.R, code/margin_direction_figure.R and code/early_leads_figures.R.

late_start_share <- 0.5    # files whose first snapshot already holds this share can't show the early count

#' Why a race's files can't stand for the statewide count (NA when they can): the race is in the
#' exclude file, its files start with most of the vote in, they cover one county, or their final
#' margin differs from AP's statewide result (the files cover only part of the state; see
#' files_vs_ap()). A race AP doesn't carry, or a run with --ap off, can't be checked and isn't
#' flagged for coverage.
early_count_flags <- function(x, excluded = character()) {
  first_share <- first(x$race$share_counted[x$race$total > 0])
  n_counties <- if (isTRUE(x$has_counties)) nrow(x$county_votes) else NA_integer_
  ap <- files_vs_ap(x)
  flag <- c(
    if (x$race_id %in% excluded) "in exclude file",
    if (!is.na(first_share) && first_share >= late_start_share)
      paste0("files start with ", scales::percent(first_share, 1), " counted"),
    if (!is.na(n_counties) && n_counties == 1) "files cover one county",
    if (isFALSE(ap$matches_ap))
      sprintf("files cover part of the state (final %+.1f vs AP %+.1f)", ap$files_final, ap$ap_final)
  )
  if (length(flag)) paste(flag, collapse = "; ") else NA_character_
}

#' The files' final margin against AP's newest statewide (or districtwide) margin. matches_ap is
#' FALSE when they differ by more than ap_margin_tolerance, which means the files cover only part
#' of the race (Kansas, Pennsylvania, Michigan, Texas): their shifts describe those counties, and
#' their final isn't the statewide result. NA when AP doesn't carry the race or --ap is off.
files_vs_ap <- function(x) {
  id <- str_match(x$race_id, "^([A-Z]{2})_([a-z]+)(?:_(\\d+))?$")
  files_margin <- last(x$race$margin[x$race$total > 0])
  ap <- if (figure_ap()) ap_race_at(id[, 2], id[, 3], as.integer(id[, 4])) else NULL
  ap_margin <- if (!is.null(ap) && !is.null(ap$margin)) ap$margin else NA_real_
  tibble::tibble(files_final = files_margin, ap_final = ap_margin,
                 matches_ap = if (is.na(ap_margin)) NA else abs(ap_margin - files_margin) <= ap_margin_tolerance)
}

#' The running margin to use for each general-election race in `built` (build_race() results)
#' plus every race AP carries that the state files don't: the state files where they can stand
#' for the statewide count (early_count_flags() is NA), else AP's statewide running count
#' (ap_running_margins()). Races in the exclude file are dropped either way.
#'
#' Returns a list of list(race_id, race, source, files_flag), where race has the columns of
#' build_race()$race that the early-count scripts use (time, total, margin, share_counted), and
#' source is "State files" or "AP". `ap` is ap_running_margins() output, read once by the caller;
#' `run` (race_run()) limits the AP-only races to its --state and --race.
race_runs <- function(built, ap, excluded = character(), run = NULL) {
  built <- purrr::keep(built, \(x) identical(x$margin_label, "D–R"))
  from_files <- purrr::map(built, \(x) {
    flag <- early_count_flags(x, excluded)
    if (x$race_id %in% excluded) return(NULL)
    if (is.na(flag)) return(list(race_id = x$race_id, race = x$race, source = "State files", files_flag = NA_character_))
    if (!is.null(ap[[x$race_id]])) return(list(race_id = x$race_id, race = ap[[x$race_id]], source = "AP", files_flag = flag))
    cli::cli_alert_warning("{x$race_id} left out: {flag}, and AP doesn't carry it")
    NULL
  }) |> purrr::compact()

  # races with no state folder at all
  have <- purrr::map_chr(built, "race_id")
  ap_only <- setdiff(names(ap), c(have, excluded))
  # ... within the run's --state and --race, when given
  if (!is.null(run$only_states)) ap_only <- ap_only[str_sub(ap_only, 1, 2) %in% run$only_states]
  if (!is.null(run) && !identical(run$contest, "all")) ap_only <- ap_only[str_sub(ap_only, 4) %in% run$contest]
  c(from_files, purrr::map(ap_only, \(id) list(race_id = id, race = ap[[id]], source = "AP", files_flag = "no state files")))
}
