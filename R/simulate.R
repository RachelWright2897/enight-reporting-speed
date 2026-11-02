# Simulated election nights in the enightmodels schema, for testing the pipeline
# before real snapshots exist. simulate_state() writes one file per county per
# 5-minute scrape to {root}/{STATE}/clean/, named the way cbs-enr names them.
# Keep "data/simulated" in root: build_race() uses it to mark output as simulated.
#
# Built in: early and mail votes arrive first and lean D, election day votes
# arrive later and lean R. "batch" counties release groups of precincts at a few
# times, "steady" counties release precincts one by one. A few precincts get a
# downward correction late in the night.

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)

TZ <- "America/New_York"
at <- \(x) ymd_hm(x, tz = TZ)
times <- seq(at("2026-11-03 19:00"), at("2026-11-04 01:30"), by = "5 min")
rand_time <- \(n, from, to) at(from) + runif(n, 0, as.numeric(difftime(at(to), at(from), units = "secs")))

simulate_state <- function(st, counties, seed, root = "data/simulated") {
  set.seed(seed)
  out_dir <- fs::path(root, st, "clean")
  fs::dir_create(out_dir)
  fs::file_delete(fs::dir_ls(out_dir, glob = "*.csv"))

  precincts <- counties |>
    mutate(county_cbs = row_number()) |>
    uncount(n_prec, .id = "i") |>
    mutate(
      precinct = sprintf("%04d", i),
      geo = paste(tolower(st), str_pad(county_cbs, 3, pad = "0"), precinct, sep = "_"),
      lean = plogis(qlogis(lean) + rnorm(n(), 0, 0.5)),
      size = round(size * exp(rnorm(n(), 0, 0.4)))
    )

  # Final votes by precinct x method x candidate
  method_share <- c(early = 0.35, mail = 0.25, eday = 0.40)
  method_shift <- c(early = 0, mail = 0.35, eday = -0.30)

  finals <- precincts |>
    cross_join(tibble(method = names(method_share))) |>
    mutate(
      n = round(size * method_share[method] * exp(rnorm(n(), 0, 0.15))),
      dem = round(n * plogis(qlogis(lean) + method_shift[method])),
      rep = n - dem
    ) |>
    pivot_longer(c(dem, rep), names_to = "party", values_to = "final") |>
    mutate(candidate = if_else(party == "dem", "alvarez, maria", "whitfield, james"))

  # Release schedule: (geo, method, time, cumulative fraction)
  county_times <- counties |>
    mutate(
      early_t = rand_time(n(), "2026-11-03 19:30", "2026-11-03 19:55"),
      mail_t = early_t + minutes(10),
      mail_late_t = rand_time(n(), "2026-11-03 22:30", "2026-11-04 00:30")
    )

  pre_eday <- precincts |>
    left_join(select(county_times, county, early_t, mail_t, mail_late_t), by = "county") |>
    reframe(
      method = c("early", "mail", "mail"),
      time = c(early_t, mail_t, mail_late_t),
      # steady counties count mail in two passes; batch counties in one
      frac = if (style == "steady") c(1, 0.6, 1) else c(1, 1, 1),
      .by = c(geo, county, style)
    )

  eday_batch <- precincts |>
    filter(style == "batch") |>
    mutate(group = sample(1:3, n(), replace = TRUE)) |>
    left_join(
      counties |>
        filter(style == "batch") |>
        reframe(group = 1:3, time = sort(rand_time(3, "2026-11-03 20:45", "2026-11-04 00:45")), .by = county),
      by = c("county", "group")
    ) |>
    transmute(geo, county, style, method = "eday", time, frac = 1)

  eday_steady <- precincts |>
    filter(style == "steady") |>
    mutate(
      full_t = rand_time(n(), "2026-11-03 20:15", "2026-11-03 23:45"),
      partial = runif(n()) < 0.3
    ) |>
    reframe(
      method = "eday",
      time = if (partial) c(full_t - minutes(sample(30:60, 1)), full_t) else full_t,
      frac = if (partial) c(0.5, 1) else 1,
      .by = c(geo, county, style)
    )

  events <- bind_rows(pre_eday, eday_batch, eday_steady)

  corrections <- events |>
    filter(method == "eday") |>
    slice_max(time, n = 1, by = geo) |>
    slice_sample(prop = 0.03) |>
    mutate(time = time + minutes(sample(30:90, n(), replace = TRUE)), frac = 0.97) |>
    filter(time <= max(times))

  events <- bind_rows(events, corrections) |> arrange(geo, method, time)

  # Cumulative fraction reported at each scrape = fraction of the last event at or before it
  reported <- events |>
    reframe(
      frac = c(0, frac)[findInterval(as.numeric(times), as.numeric(time)) + 1],
      time = times,
      .by = c(geo, county, method)
    )

  snapshots <- finals |>
    select(geo, county, method, candidate, party, final) |>
    inner_join(reported, by = c("geo", "county", "method"), relationship = "many-to-many") |>
    mutate(
      state = st,
      election = as.Date("2026-11-03"),
      jurisdiction = tolower(st),
      contest = "sen",
      district = NA_integer_,
      votes = round(final * frac)
    ) |>
    mutate(stage = if_else(sum(votes) > 0, 2L, 0L), .by = c(geo, time)) |>
    select(county, state, election, jurisdiction, contest, district, candidate, party, time, stage, method, geo, votes)

  snapshots |>
    group_by(county, time) |>
    group_walk(\(d, k) {
      ts <- format(k$time, "%Y_%m_%d_%H_%M_%S", tz = TZ)
      data.table::fwrite(select(d, -county), fs::path(out_dir, glue::glue("{st}_{k$county}_{ts}.csv")))
    }, .keep = TRUE)

  message("Wrote ", length(fs::dir_ls(out_dir)), " snapshot files to ", out_dir)
}
