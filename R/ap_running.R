# AP's statewide running margin for each race over election night, in the same shape as
# build_race()$race (time, total, a, b, margin, share_counted), so the early-count scripts
# (code/margin_direction.R, code/early_leads_figures.R, code/early_swing_figure.R) can use AP
# for races whose state files don't cover the whole state. AP reports race-wide totals only, so
# there is no vote-method breakdown.
#
# The margin is defined as in the state files: 100 * (Democrat - Republican) / all votes.

#' AP running margins by race through `through` plus AP's latest pull (the final count), as a
#' named list of tibbles keyed "{ST}_{office}" (GA_gov). Where AP carries a special election
#' beside the regular one, the regular race is kept.
ap_running_margins <- function(offices = c("sen", "gov"), dir = ap_dir(), election_date = ELECTION_DATE,
                               through = lubridate::ymd_hm(paste(as.Date(election_date) + 1, "12:00"), tz = TZ)) {
  # read only election night and the last pull: link those files into a scratch folder.
  # Pull timestamps in AP file names are ET.
  files <- list.files(dir, paste0("^ap_(candidates|contests)_", ap_ts_regex, "\\.parquet$"), full.names = TRUE)
  stamps <- stringr::str_extract(basename(files), ap_ts_regex)
  when <- lubridate::ymd_hms(stamps, tz = TZ)
  keep <- (when <= through & when >= as.POSIXct(election_date, tz = TZ)) | stamps == max(stamps)
  subset_dir <- file.path(tempdir(), "ap_night")
  unlink(subset_dir, recursive = TRUE)
  dir.create(subset_dir)
  file.symlink(files[keep], file.path(subset_dir, basename(files[keep])))

  merged <- read_ap_snapshots(subset_dir, offices, election_date) |> filter(!is.na(votes))
  if (nrow(merged) == 0) return(list())

  # the regular race where a special shares the state and office
  races <- merged |> distinct(state, office, source_race_id, special) |>
    arrange(state, office, special) |> slice_head(n = 1, by = c(state, office))
  merged <- semi_join(merged, races, by = "source_race_id")

  running <- merged |>
    summarise(total = sum(votes), a = sum(votes[party %in% "dem"]), b = sum(votes[party %in% "rep"]),
              two_party = all(c("dem", "rep") %in% party),
              .by = c(state, office, time)) |>
    filter(two_party) |>
    arrange(time) |>
    mutate(margin = 100 * (a - b) / total, share_counted = total / last(total), .by = c(state, office)) |>
    filter(total > 0) |>
    mutate(race_id = paste0(state, "_", office))

  running |>
    select(race_id, time, total, a, b, margin, share_counted) |>
    split(~race_id) |>
    purrr::map(\(d) select(d, -race_id))
}

#' The AP offices to read for a race_run(): Senate and governor, or those of them the run's
#' --race asks for
ap_offices <- function(run) {
  if (identical(run$contest, "all")) c("sen", "gov") else intersect(c("sen", "gov"), run$contest)
}
