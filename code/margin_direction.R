# Which way did each race's margin move over the count? For every general-election race (D–R
# margin) from the state files, the running margin at a few early points against the final
# margin, and a tally of how many races moved toward Democrats or toward Republicans.
#
# Early points, each compared with the final margin:
#   5% counted   the first snapshot with 5% of the final vote in (as compare.R starts)
#   1 hour       the last snapshot by 1 hour after the state's polls closed
#   4 hours      the last snapshot by 4 hours after
# shift = final margin - early margin, in points: positive means the count moved toward Democrats,
# negative toward Republicans. A shift under `direction_min_pp` either way counts as little change.
#
# Each race's running margin comes from its state files where they can stand for the statewide
# count, and from AP's statewide running count where they can't: files that start after most of
# the vote was in, cover one county, or cover only part of the state (final differs from AP's),
# and states with no state files (race_runs() in R/early_count.R). The source column says which;
# files_flag says why the files weren't used. Races in helper_files/{folder}_exclude.csv are left
# out.
#
# Writes outputs/margin_direction/races.csv (one row per race) and states.csv (one row per
# state and early point), and prints the tally.
#   Rscript code/margin_direction.R                       # every statewide race
#   Rscript code/margin_direction.R --race sen,gov --state GA,PA
#   Rscript code/margin_direction.R --race house          # every U.S. House district
source("setup.R")

direction_min_pp <- 1      # shifts smaller than this, either way, are "little change"
# Speed: the early points only need election night, and the final margin only each feed's last
# file (use_night_snapshots(), R/night_snapshots.R)
use_night_snapshots()

#' The running margin at each early point and its shift to the final, one row per point
race_direction <- function(x, close_time) {
  race <- x$race |> filter(total > 0) |> arrange(time)
  final <- last(race$margin)
  at_time <- \(h) {
    if (is.na(close_time)) return(NULL)
    slice_tail(filter(race, time <= close_time + h * 3600), n = 1)
  }
  points <- list(`5% counted` = slice_head(filter(race, share_counted >= 0.05), n = 1),
                 `1 hour` = at_time(1), `4 hours` = at_time(4))
  purrr::imap(points, \(at, point) {
    has <- !is.null(at) && nrow(at) == 1
    tibble::tibble(
      point = point,
      time = if (has) at$time else as.POSIXct(NA, tz = TZ),
      share_counted = if (has) at$share_counted else NA_real_,
      early_margin = if (has) at$margin else NA_real_
    )
  }) |>
    purrr::list_rbind() |>
    mutate(final_margin = final, shift = final - early_margin)
}

run <- race_run()
closes <- read_poll_close()
exclude_file <- fs::path(CONFIG_DIR, paste0(run$folder, "_exclude.csv"))
excluded <- if (fs::file_exists(exclude_file)) readr::read_csv(exclude_file, show_col_types = FALSE)$race_id else character()

runs <- race_runs(for_each_race(run), ap_running_margins(ap_offices(run)), excluded, run)
races <- purrr::map(runs, \(r) {
  state <- str_sub(r$race_id, 1, 2)
  race_direction(r, closes$time[match(state, closes$state)]) |>
    mutate(race_id = r$race_id, state = state, source = r$source, files_flag = r$files_flag, .before = 1)
}) |>
  purrr::list_rbind()
if (nrow(races) == 0) stop("No general-election races to measure")

races <- races |>
  mutate(direction = case_when(
    is.na(shift) ~ NA_character_,
    shift >= direction_min_pp ~ "Toward Democrats",
    shift <= -direction_min_pp ~ "Toward Republicans",
    .default = "Little change"
  ))

usable <- filter(races, !is.na(direction))
by_state <- usable |>
  summarise(races = n(),
            toward_dem = sum(direction == "Toward Democrats"),
            toward_rep = sum(direction == "Toward Republicans"),
            little_change = sum(direction == "Little change"),
            mean_shift = mean(shift),
            .by = c(state, point)) |>
  arrange(point, desc(mean_shift))

out_dir <- file.path(OUTPUT_DIR, "margin_direction")
fs::dir_create(out_dir)
readr::write_csv(mutate(races, across(where(is.numeric), \(v) round(v, 2))), file.path(out_dir, "races.csv"))
readr::write_csv(mutate(by_state, mean_shift = round(mean_shift, 2)), file.path(out_dir, "states.csv"))

cli::cli_h1("Which way margins moved, early point to final (shift under {direction_min_pp} pt = little change)")
tally <- usable |>
  count(point, direction) |>
  tidyr::pivot_wider(names_from = direction, values_from = n, values_fill = 0)
print(as.data.frame(tally), row.names = FALSE)
from_ap <- distinct(filter(races, source == "AP"), race_id, files_flag)
if (nrow(from_ap)) {
  cli::cli_h2("From AP's running count (state files fall short)")
  print(as.data.frame(from_ap), row.names = FALSE)
}
cli::cli_alert_success("Wrote {file.path(out_dir, 'races.csv')} and states.csv")
