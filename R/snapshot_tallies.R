# Functions for turning enightdata snapshots into the running tallies the
# post-election margin figures (code/fig4-fig7, code/comparisons_state_reporting_speed.R) plot.
#
# Input: the timestamped files cbs-enr writes after every scrape,
#   {election folder}/{STATE}/clean/{STATE}_{county}_{timestamp}.parquet
# (plus a .csv copy of the latest one). Each file is a full snapshot in the
# enightmodels schema (one row per candidate x method x geo), so the night is the
# sequence of those files.

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)

TZ <- "America/New_York"
# first/last time a condition held, NA (not -Inf/Inf with a warning) if it never did
first_time <- \(t) if (length(t)) min(t) else as.POSIXct(NA, tz = TZ)
last_time <- \(t) if (length(t)) max(t) else as.POSIXct(NA, tz = TZ)
ts_regex <- "\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}"
broken_drop <- 0.02
snapshot_cols <- c("state", "election", "contest", "district", "candidate", "party", "method", "geo", "votes")
# Offices elected statewide. Their district is ignored, since some scrapers fill it in
# anyway (NC writes district 1 for U.S. Senate). Primary contests add _dem / _rep.
statewide_offices <- c("sen", "gov", "ltgov", "ag", "sos", "pres", "supcrt")
is_statewide <- \(contest) str_remove(contest, "_(dem|rep)$") %in% statewide_offices

#' Timestamped snapshot files in a clean/ folder
#'
#' cbs-enr writes every snapshot as parquet and only the latest as csv, so use
#' parquet when there is any (the simulated data is csv only).
snapshot_files <- function(clean_dir) {
  files <- fs::dir_ls(clean_dir, regexp = paste0("_", ts_regex, "\\.parquet$"))
  if (length(files) == 0) files <- fs::dir_ls(clean_dir, regexp = paste0("_", ts_regex, "\\.csv$"))
  if (length(files) == 0) stop("No timestamped snapshots found in ", clean_dir)
  files
}

read_snapshot_file <- function(f) {
  d <- if (str_ends(f, "parquet")) {
    nanoparquet::read_parquet(f, col_select = snapshot_cols)
  } else {
    data.table::fread(f, select = snapshot_cols,
                      colClasses = list(character = c("geo", "candidate", "party", "method", "contest")))
  }
  as_tibble(d) |>
    mutate(across(c(geo, candidate, party, method, contest), as.character), district = as.integer(district))
}

#' Read every timestamped snapshot in a clean/ folder for one race
#'
#' `time` is taken from the file name (the string returned by `xx_timestamp()`),
#' not from the `time` column, so it survives the csv round trip unchanged.
#' Snapshots from before election day (test pulls) are dropped.
read_snapshots <- function(clean_dir, target_contest, target_district = NA, election_date = NULL) {
  snaps <- map(snapshot_files(clean_dir), \(f) {
    read_snapshot_file(f) |>
      filter(
        .data$contest %in% target_contest,
        is_statewide(.data$contest) |
          (if (is.na(target_district)) is.na(.data$district) else .data$district %in% target_district)
      ) |>
      mutate(
        time = ymd_hms(str_extract(basename(f), ts_regex), tz = TZ),
        feed = str_remove(basename(f), paste0("_", ts_regex, "\\..*$")),
        file_county = basename(f) |>
          str_remove(paste0("_", ts_regex, "\\.(csv|parquet)$")) |>
          str_remove("^[A-Za-z]{2}_*") |>
          # per-party feeds (e.g. TX_dallas_dem) carry the party after the county
          str_remove("_?(dem|rep)$")
      )
  }) |>
    list_rbind()

  if (nrow(snaps) == 0) stop("No rows for contest '", target_contest, "' in ", clean_dir)

  # The election column records the configured date at scrape time, which can be
  # stale (several 2026 primaries carry the previous primary's date), so an
  # explicit date wins when one is given
  election_day <- ymd(election_date %||% as.character(min(snaps$election)), tz = TZ)
  snaps <- filter(snaps, time >= election_day)

  # Some feeds write NA for precincts that haven't reported (GA) or for an empty
  # cell (PA Philadelphia provisional); for a running tally that is zero votes
  n_na <- sum(is.na(snaps$votes))
  if (n_na > 0) {
    message("Counting ", format(n_na, big.mark = ","), " missing vote values as 0")
    snaps <- mutate(snaps, votes = coalesce(votes, 0))
  }

  # Rows repeated within one pull (CA Orange listed 110,250 rows twice on 06/08)
  # would double that pull's total and make every later pull look broken
  snaps <- distinct(snaps, feed, geo, candidate, party, method, time, .keep_all = TRUE)

  feed_totals <- snaps |>
    summarise(votes = sum(votes), sig = paste(sort(votes), collapse = ","), .by = c(feed, time)) |>
    arrange(time)

  # A pull identical to a later pull, with lower totals in between, carries the
  # wrong timestamp. TX Harris has a file stamped 05/26 01:04 that matches the
  # 05/27 01:04 pull exactly, ahead of election night's real, lower counts.
  mislabeled <- feed_totals |>
    filter(votes > 0) |>
    mutate(
      later_copy = purrr::map_lgl(seq_along(time), \(i) {
        later <- which(sig[-seq_len(i)] == sig[i]) + i
        length(later) > 0 && any(votes[(i + 1):later[1]] < (1 - broken_drop) * votes[i])
      }),
      .by = feed
    ) |>
    filter(later_copy)
  if (nrow(mislabeled) > 0) {
    message("Dropping ", nrow(mislabeled), " mislabeled pull(s) that repeat a later pull: ",
            paste(mislabeled$feed, format(mislabeled$time, "%m/%d %H:%M"), collapse = "; "))
    snaps <- anti_join(snaps, mislabeled, by = c("feed", "time"))
    feed_totals <- anti_join(feed_totals, mislabeled, by = c("feed", "time"))
  }
  feed_totals <- feed_totals |> select(-sig) |> mutate(peak = cummax(votes), .by = feed)

  # A feed that drops to zero after showing votes was reset, so what it showed
  # before was test or preliminary data. Keep only what came after the last reset.
  resets <- feed_totals |> filter(peak > 0, votes == 0)
  if (nrow(resets) > 0) {
    resets <- summarise(resets, reset = max(time), .by = feed)
    message("Feed reset to zero, dropping earlier pulls: ",
            paste(resets$feed, format(resets$reset, "%m/%d %H:%M"), collapse = "; "))
    snaps <- snaps |>
      left_join(resets, by = "feed") |>
      filter(is.na(reset) | time >= reset) |>
      select(-reset)
    feed_totals <- feed_totals |>
      left_join(resets, by = "feed") |>
      filter(is.na(reset) | time >= reset) |>
      mutate(peak = cummax(votes), .by = feed)
  }

  # A pull more than 2% below the feed's earlier peak, but not zero, is a broken
  # scrape, not a real correction: totals only grow during a count apart from small
  # fixes. That covers partial pages (CA Orange, 06/12) and pulls that moved votes
  # between precincts (CA Orange, 06/15-16). Drop just those pulls.
  broken <- feed_totals |> filter(peak > 0, votes > 0, votes < (1 - broken_drop) * peak)
  if (nrow(broken) > 0) {
    message("Dropping ", nrow(broken), " broken pull(s): ",
            paste(broken$feed, format(broken$time, "%m/%d %H:%M"), collapse = "; "))
    snaps <- anti_join(snaps, broken, by = c("feed", "time"))
  }

  snaps |>
    select(-feed) |>
    # Same geo scraped twice at one timestamp (e.g. overlapping feeds): keep one
    distinct(geo, candidate, party, method, time, .keep_all = TRUE) |>
    mutate(county_cbs = str_split_i(geo, "_", 2)) |>
    add_county_names()
}

#' Contests (and districts) in a clean/ folder, from each feed's latest snapshot
list_contests <- function(clean_dir) {
  files <- snapshot_files(clean_dir)
  feed <- str_remove(basename(files), paste0("_", ts_regex, "\\..*$"))
  map(tapply(files, feed, max), \(f) distinct(read_snapshot_file(f), contest, district)) |>
    list_rbind() |>
    filter(!is.na(contest)) |>
    distinct() |>
    arrange(contest, district)
}

# Lane labels, in order of preference: the county in the file name (per-county
# feeds), the county the CBS code stands for (statewide feeds), the code itself.
# The CBS code is the county's alphabetical index within its state, so it can be
# matched against usmap's county list (Harris TX = 101).
add_county_names <- function(snaps) {
  from_files <- snaps |>
    filter(file_county != "", file_county != "NA") |>
    count(county_cbs, file_county) |>
    slice_max(n, n = 1, by = county_cbs, with_ties = FALSE) |>
    transmute(county_cbs, county_file = str_to_title(file_county))

  from_codes <- usmap::countypop |>
    filter(abbr == snaps$state[1]) |>
    arrange(county) |>
    transmute(
      county_cbs = str_pad(row_number(), 3, pad = "0"),
      county_code = str_remove(county, " (County|Parish|Borough|Census Area|Municipality)$")
    )

  snaps |>
    left_join(from_files, by = "county_cbs") |>
    left_join(from_codes, by = "county_cbs") |>
    mutate(county = coalesce(county_file, county_code, paste("County", county_cbs))) |>
    select(-file_county, -county_file, -county_code)
}

#' Which candidates make up the two sides of the margin
#'
#' D minus R when the race has both parties (generals). Otherwise the final
#' first-place minus second-place candidate (primaries).
margin_sides <- function(snaps) {
  final <- snaps |>
    filter(time == max(time), .by = geo) |>
    summarise(votes = sum(votes), .by = c(candidate, party))

  if (all(c("dem", "rep") %in% final$party)) {
    sides <- final |> mutate(side = case_when(party == "dem" ~ "a", party == "rep" ~ "b"))
    attr(sides, "label") <- "D–R"
  } else {
    top <- final |> slice_max(votes, n = 2, with_ties = FALSE)
    # No margin to measure when the runner-up is a stray write-in
    if (nrow(top) < 2 || top$votes[2] < 0.01 * sum(final$votes)) {
      stop("Uncontested: no second candidate with at least 1% of the vote")
    }
    sides <- final |> mutate(side = case_when(
      candidate == top$candidate[1] ~ "a",
      candidate == top$candidate[2] ~ "b"
    ))
    attr(sides, "label") <- paste(str_to_title(word(top$candidate, 1, sep = ",")), collapse = "–")
  }
  sides
}

#' One row per precinct per snapshot, with change flags and final-result comparisons
#'
#' A precinct "reports" at a snapshot when any candidate x method count differs
#' from its previous snapshot (or, at its first snapshot, when it has any votes).
#' Changes that happen between two scrapes collapse into one report.
precinct_snapshots <- function(snaps, sides) {
  snaps |>
    left_join(select(sides, candidate, party, side), by = c("candidate", "party")) |>
    arrange(candidate, party, method) |>
    summarise(
      total = sum(votes),
      a = sum(votes[side %in% "a"]),
      b = sum(votes[side %in% "b"]),
      sig = paste(votes, collapse = ","),
      .by = c(geo, county_cbs, county, time)
    ) |>
    arrange(geo, time) |>
    mutate(
      changed = if_else(row_number() == 1, total > 0, sig != lag(sig)),
      delta = total - lag(total, default = 0),
      delta_a = a - lag(a, default = 0),
      delta_b = b - lag(b, default = 0),
      report_n = cumsum(changed),
      # The precinct's own final result is its last snapshot
      final_total = last(total),
      final_a = last(a),
      final_b = last(b),
      .by = geo
    ) |>
    mutate(
      margin = 100 * (a - b) / total,
      final_margin = 100 * (final_a - final_b) / final_total,
      dev_from_final = margin - final_margin,
      share_counted = total / final_total,
      increment_margin = if_else(delta > 0, 100 * (delta_a - delta_b) / delta, NA_real_)
    ) |>
    select(-sig)
}

#' Race-wide running tally at every snapshot
#'
#' Precincts missing from a snapshot carry their last known count forward.
race_running <- function(pt) {
  pt |>
    select(geo, time, total, a, b) |>
    complete(geo, time = sort(unique(pt$time))) |>
    arrange(geo, time) |>
    fill(total, a, b, .by = geo) |>
    replace_na(list(total = 0, a = 0, b = 0)) |>
    summarise(across(c(total, a, b), sum), .by = time) |>
    arrange(time) |>
    mutate(
      margin = 100 * (a - b) / total,
      final_margin = last(margin),
      share_counted = total / last(total)
    )
}

#' County x snapshot vote arrivals, for the count timeline
county_updates <- function(reports) {
  reports |>
    summarise(
      votes_added = sum(pmax(delta, 0)),
      votes_removed = -sum(pmin(delta, 0)),
      precincts_changed = n(),
      # Margin among the votes that arrived in this update
      update_margin = 100 * sum((delta_a - delta_b)[delta > 0]) / sum(delta[delta > 0]),
      .by = c(county, county_cbs, time)
    ) |>
    arrange(county, time)
}

# --- Vote method -------------------------------------------------------------

#' One row per precinct x method x snapshot
#'
#' Same change rule as precinct_snapshots(), applied within each method.
method_snapshots <- function(snaps, sides) {
  snaps |>
    left_join(select(sides, candidate, party, side), by = c("candidate", "party")) |>
    arrange(candidate, party) |>
    summarise(
      total = sum(votes),
      a = sum(votes[side %in% "a"]),
      b = sum(votes[side %in% "b"]),
      sig = paste(votes, collapse = ","),
      .by = c(geo, county_cbs, county, method, time)
    ) |>
    arrange(geo, method, time) |>
    mutate(
      changed = if_else(row_number() == 1, total > 0, sig != lag(sig)),
      .by = c(geo, method)
    ) |>
    select(-sig)
}

#' Running tally for each method, across all precincts or within each county
#'
#' Precincts missing from a snapshot carry their last known count forward.
method_running <- function(mt, by = NULL) {
  mt |>
    select(geo, county, method, time, total, a, b) |>
    complete(nesting(geo, county, method), time = sort(unique(mt$time))) |>
    arrange(geo, method, time) |>
    fill(total, a, b, .by = c(geo, method)) |>
    replace_na(list(total = 0, a = 0, b = 0)) |>
    summarise(across(c(total, a, b), sum), .by = all_of(c(by, "method", "time"))) |>
    arrange(time) |>
    mutate(
      margin = 100 * (a - b) / total,
      share_counted = total / last(total),
      .by = all_of(c(by, "method"))
    )
}

#' Per-method summary: size, final margin, and how fast each method was counted
method_summary <- function(mt, running, by = NULL) {
  timing <- running |>
    summarise(
      final_votes = last(total),
      final_margin = last(margin),
      first_votes = first_time(time[total > 0]),
      half_counted = first_time(time[share_counted >= 0.5]),
      ninety_counted = first_time(time[share_counted >= 0.9]),
      .by = all_of(c(by, "method"))
    )
  reports <- mt |>
    summarise(n = sum(changed), .by = all_of(c(by, "geo", "method"))) |>
    summarise(
      precinct_reports = sum(n),
      median_reports_per_precinct = median(n[n > 0]),
      .by = all_of(c(by, "method"))
    )

  timing |>
    left_join(reports, by = c(by, "method")) |>
    mutate(share_of_votes = final_votes / sum(final_votes), .by = all_of(by)) |>
    filter(final_votes > 0)
}

#' Ballots counted by vote method over time, across every contest in a clean/ folder
#'
#' The feeds report votes per contest, not ballots cast, so ballots are estimated per precinct,
#' method and snapshot as the most votes in any one contest there. That undercounts only by the
#' undervote in the most-voted contest (typically 1-3% at the top of the ticket). Every contest
#' the feeds carry is vote-for-one (Senate, governor, House, AG, SOS), so no contest counts a
#' ballot twice. Each contest is read through read_snapshots(), so mislabeled, broken and reset
#' pulls are dropped as they are for the race figures, and a precinct missing from a snapshot
#' carries its last count forward.
#'
#' Returns one row per method x snapshot: ballots, and share_counted (ballots over the method's
#' ballots at the last snapshot).
ballots_by_method <- function(clean_dir, election_date = NULL) {
  contests <- list_contests(clean_dir) |>
    mutate(district = if_else(is_statewide(contest), NA_integer_, district)) |>
    distinct()
  per_contest <- pmap(contests, \(contest, district) {
    read_snapshots(clean_dir, contest, district, election_date) |>
      summarise(votes = sum(votes), .by = c(geo, method, time)) |>
      mutate(race = paste(contest, district))
  }) |>
    list_rbind()

  per_contest |>
    complete(nesting(race, geo, method), time = sort(unique(per_contest$time))) |>
    arrange(race, geo, method, time) |>
    fill(votes, .by = c(race, geo, method)) |>
    replace_na(list(votes = 0)) |>
    summarise(ballots = max(votes), .by = c(geo, method, time)) |>
    summarise(ballots = sum(ballots), .by = c(method, time)) |>
    arrange(time) |>
    mutate(share_counted = ballots / last(ballots), .by = method)
}

#' Every table the figures need for one state's race, kept in memory
#'
#' race_id is {STATE}_{contest}[_{district}], e.g. GA_gov_rep, TX_house_7.
build_race <- function(clean_dir, target_contest, target_district = NA_integer_, election_date = NULL) {
  race_id <- paste0(
    basename(dirname(clean_dir)), "_", target_contest,
    if (!is.na(target_district)) paste0("_", target_district) else ""
  )

  snaps <- read_snapshots(clean_dir, target_contest, target_district, election_date)
  sides <- margin_sides(snaps)

  pt <- precinct_snapshots(snaps, sides)
  race <- race_running(pt)
  reports <- filter(pt, changed)

  mt <- method_snapshots(snaps, sides)
  method_run <- method_running(mt)

  message(race_id, ": ", n_distinct(snaps$time), " snapshots, ", n_distinct(pt$geo), " precincts. Margin is ",
          attr(sides, "label"), ".")
  list(
    race = race,
    updates = county_updates(reports),
    # final votes per county, to pick the largest counties for the county figures
    county_votes = pt |> slice_max(time, n = 1, by = geo, with_ties = FALSE) |>
      summarise(votes = sum(total), .by = county) |> arrange(desc(votes)),
    method_running = method_run,
    methods = method_summary(mt, method_run),
    county_methods = method_summary(mt, method_running(mt, by = "county"), by = "county"),
    margin_label = attr(sides, "label"),
    race_id = race_id,
    # A feed with only statewide totals (geo with no county code, or county 000)
    # has no counties to show, so figure 7 is skipped
    has_counties = any(str_detect(pt$county_cbs, "^\\d{3}$") & pt$county_cbs != "000", na.rm = TRUE)
  )
}
