# Cross-state comparison: do states that report faster have fewer swings in the
# statewide margin? compare_states() takes every race build_race() returned and
# writes the comparison figures 3, 4 and 6 to out_dir (run by code/compare_states.R).
#
# Swing here is frequency, measured on the statewide running margin:
#   big moves     snapshots where the margin moved more than 1 point from the
#                 previous snapshot
#   lead changes  times the leader in the running count switched
# Both are counted once 5% of the final vote is in, since every count swings
# wildly over its first few batches. Speed is hours from the race's first results
# (1% of the final vote) to 90% of the final vote counted.
#
# Benchmarks follow Shaikh and Stewart, "Why the Wait?" (2026): at 1 hour, 4 hours,
# 12 hours, 2 days and 1 week after polls close, the share of the final vote
# reported and the shift in two-party share from the final result. In a general
# the shift is the Republican two-party share minus its final value (positive =
# blue shift still to come); in a primary it is the eventual winner's share. Poll
# close times come from helper_files/{election folder}_poll_close.csv.

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggmedsl)

medsl_fonts(dpi = 300)

big_move_pp <- 1
start_share <- 0.05
benchmark_hours <- c(`1 hour` = 1, `4 hours` = 4, `12 hours` = 12, `2 days` = 48, `1 week` = 168)

office_short <- c(
  sen = "Senate", gov = "Governor", ltgov = "Lt. Governor", ag = "Attorney General",
  sos = "Secretary of State", supcrt = "Supreme Court", pres = "President", house = "House"
)

#' One row per race: reporting speed and how often the statewide margin swung
race_swing_summary <- function(x) {
  race <- x$race |> filter(total > 0) |> arrange(time)
  t0 <- min(race$time[race$share_counted >= 0.01])
  hours_to <- \(p) as.numeric(difftime(min(race$time[race$share_counted >= p]), t0, units = "hours"))

  counted <- race |> filter(share_counted >= start_share)
  moves <- abs(diff(counted$margin))
  leader <- sign(counted$margin)
  leader <- leader[leader != 0]

  id <- str_match(x$race_id, "^([A-Z]{2})_([a-z]+)(?:_(dem|rep))?(?:_(\\d+))?$")
  tibble(
    race_id = x$race_id,
    state = id[, 2],
    label = paste0(id[, 2], " ", coalesce(office_short[id[, 3]], id[, 3]),
                   if_else(is.na(id[, 4]), "", paste0(" (", str_to_upper(str_sub(id[, 4], 1, 1)), ")"))),
    margin_label = x$margin_label,
    final_margin = last(race$margin),
    hours_to_50 = hours_to(0.5),
    hours_to_90 = hours_to(0.9),
    hours_to_99 = hours_to(0.99),
    snapshots = nrow(counted),
    big_moves = sum(moves > big_move_pp),
    big_moves_any_share = sum(abs(diff(race$margin)) > big_move_pp),
    lead_changes = sum(leader[-1] != leader[-length(leader)]),
    largest_move = if (length(moves)) max(moves) else NA_real_
  )
}

#' Share reported and two-party shift at each benchmark after polls close
#'
#' `close_time` falls back to the race's first results (1% of the final vote) when
#' no poll close time is configured for the state.
race_benchmarks <- function(x, close_time = NULL) {
  race <- x$race |> arrange(time)
  close_source <- if (is.null(close_time) || is.na(close_time)) "first results" else "poll close"
  if (close_source == "first results") close_time <- min(race$time[race$share_counted >= 0.01])

  # a and b are the margin's two sides; in a general b is the Republican
  partisan <- identical(x$margin_label, "D\u2013R")
  two_party <- \(a, b) if (partisan) b / (a + b) else a / (a + b)
  final <- last(race)
  final_share <- two_party(final$a, final$b)

  purrr::imap(benchmark_hours, \(h, name) {
    at <- race |> filter(time <= close_time + h * 3600) |> slice_tail(n = 1)
    counted <- nrow(at) == 1 && at$total > 0
    tibble(
      race_id = x$race_id,
      benchmark = name,
      hours = h,
      share_reported = if (counted) at$share_counted else 0,
      shift = if (counted) 100 * (two_party(at$a, at$b) - final_share) else NA_real_
    )
  }) |>
    purrr::list_rbind() |>
    mutate(abs_shift = abs(shift), shift_measure = if (partisan) "Republican share" else "Winner's share",
           close_time = close_time, close_source = close_source)
}

#' Figures 3, 4 and 6 for every race in `races`, written to `out_dir`
#'
#' `folder` is the election folder (26_primaries), which names the config files:
#' helper_files/{folder}_exclude.csv lists races whose margin isn't meaningful, and
#' helper_files/{folder}_poll_close.csv has each state's poll close time (state, poll_close_et).
compare_states <- function(races, out_dir, folder, config_dir = "helper_files") {
  if (length(races) < 2) {
    message("Fewer than two races to compare; no comparison figures")
    return(invisible(out_dir))
  }

  exclude_file <- fs::path(config_dir, paste0(folder, "_exclude.csv"))
  excluded <- if (fs::file_exists(exclude_file)) {
    readr::read_csv(exclude_file, show_col_types = FALSE)
  } else {
    tibble(race_id = character(), reason = character())
  }

  summary <- races |>
    purrr::map(race_swing_summary) |>
    purrr::list_rbind() |>
    mutate(excluded = race_id %in% excluded$race_id)

  close_file <- fs::path(config_dir, paste0(folder, "_poll_close.csv"))
  closes <- if (fs::file_exists(close_file)) {
    readr::read_csv(close_file, col_types = readr::cols(.default = "c")) |>
      transmute(state, close_time = lubridate::ymd_hm(poll_close_et, tz = "America/New_York"))
  } else {
    tibble(state = character(), close_time = as.POSIXct(character(), tz = "America/New_York"))
  }
  benchmarks <- races |>
    purrr::map(\(x) race_benchmarks(x, closes$close_time[match(str_sub(x$race_id, 1, 2), closes$state)])) |>
    purrr::list_rbind() |>
    left_join(select(summary, race_id, state, label, excluded), by = "race_id")

  races <- filter(summary, !excluded)
  if (nrow(races) < 2) {
    message("Fewer than two races to compare; no comparison figures")
    return(invisible(out_dir))
  }
  # overwrite: clear the last run's figures only once there are new ones to write
  fs::dir_create(out_dir)
  fs::file_delete(fs::dir_ls(out_dir, glob = "*.png"))

  # States are unordered, non-partisan categories: brand categorical palette (palette.md)
  states <- sort(unique(races$state))
  state_colors <- setNames(
    rep_len(c("#948DE5", "#C0BA79", "#37C256", "#FF8318", "#59CBF5", "#4E4A81", "#989044", "#0B2E4F"),
            length(states)),
    states
  )

  caption <- medsl_caption(source = "State and county reporting sites, via enightdata")
  scope <- paste0(nrow(races), " statewide races in ", length(states), " states, ", folder)
  excluded_note <- if (any(summary$excluded)) {
    paste0("Excluded: ", paste(summary$label[summary$excluded], collapse = ", "), ".")
  } else {
    NULL
  }
  counting_note <- paste0("Counted after ", percent(start_share), " of the vote was in.")
  notes <- \(...) paste(c(...), collapse = " ")
  note_theme <- theme(
    plot.tag.position = c(0.99, 0.02),
    plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0)
  )
  # 3. Every race, fastest at top, with all three measures side by side
  panels <- c(hours_to_90 = "Hours to 90% reported",
              big_moves = paste0("Moves of more than ", big_move_pp, " point"),
              lead_changes = "Lead changes")
  long <- races |>
    tidyr::pivot_longer(names(panels), names_to = "measure", values_to = "value") |>
    mutate(
      measure = factor(panels[measure], levels = panels),
      label = factor(label, levels = races$label[order(-races$hours_to_90)])
    )

  p3 <- ggplot(long, aes(value, label, color = state)) +
    geom_segment(aes(x = 0, xend = value, yend = label), linewidth = 0.6, color = "#C4C4C4") +
    geom_point(size = 2.6) +
    # leading spaces keep each value clear of its dot whatever the panel's scale
    geom_text(aes(label = paste0("   ", ifelse(measure == panels[["hours_to_90"]], sprintf("%.1f", value), value))),
              hjust = 0, family = "StyreneB-Regular", size = 2.8, color = "#444444", show.legend = FALSE) +
    facet_wrap(~measure, scales = "free_x") +
    scale_color_manual(values = state_colors, name = "State") +
    # whole-number ticks: moves and lead changes are counts
    scale_x_continuous(expand = expansion(mult = c(0, 0.25)), limits = c(0, NA),
                       breaks = \(l) unique(floor(pretty(l)))) +
    labs(
      title = "Reporting Speed, Big Margin Moves, and Lead Changes by Race",
      subtitle = paste0(scope, ", fastest-reporting races at top"),
      x = NULL, y = NULL, caption = caption, tag = notes(counting_note, excluded_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right", panel.spacing.x = unit(2, "lines"))
  ggsave_medsl(fs::path(out_dir, "3_speed_moves_leads_by_race.png"), p3, width = 10, height = 6.5)

  # --- 4 and 6. Benchmarks after poll close (Shaikh and Stewart) -------------------
  bench <- benchmarks |>
    filter(!excluded) |>
    mutate(benchmark = factor(benchmark, levels = names(benchmark_hours)))
  fallback <- unique(bench$label[bench$close_source == "first results"])
  close_note <- paste0(
    "Hours after poll close",
    if (length(fallback)) paste0(" (first results for ", paste(fallback, collapse = ", "), ")") else "",
    "."
  )
  # benchmarks are ordered in time, so they take a sequential ramp, pale (1 hour) to dark (1 week)
  bench_colors <- setNames(c("#9FDDF3", "#59CBF5", "#3791FF", "#156DD0", "#0B2E4F"), names(benchmark_hours))

  # 4. Share of the final vote reported at each benchmark, slowest races at bottom
  order_4h <- bench |> filter(benchmark == "4 hours") |> arrange(share_reported) |> pull(label)
  p4 <- ggplot(mutate(bench, label = factor(label, levels = order_4h)), aes(share_reported, label)) +
    geom_line(aes(group = label), color = "#C4C4C4", linewidth = 0.8) +
    geom_point(aes(color = benchmark), size = 2.6) +
    scale_color_manual(values = bench_colors, name = "After poll close") +
    scale_x_continuous(labels = percent, limits = c(0, 1)) +
    labs(
      title = "Share of Final Vote Reported at Each Benchmark After Poll Close",
      subtitle = paste0(scope, ", ordered by 4-hour share"),
      x = "Share of the final vote reported", y = NULL,
      caption = caption, tag = notes(close_note, excluded_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right")
  ggsave_medsl(fs::path(out_dir, "4_share_reported_by_benchmark.png"), p4, width = 9, height = 6.5)

  # 6. Speed against convergence at the election-night benchmarks
  night <- bench |> filter(benchmark %in% c("1 hour", "4 hours", "12 hours"), !is.na(abs_shift))
  p6 <- ggplot(night, aes(share_reported, abs_shift, color = state)) +
    geom_point(size = 2.6) +
    ggrepel::geom_text_repel(aes(label = label), family = "StyreneB-Regular", size = 2.4,
                             show.legend = FALSE, seed = 1, min.segment.length = 0.2, max.overlaps = 8) +
    facet_wrap(~benchmark) +
    scale_color_manual(values = state_colors, name = "State") +
    scale_x_continuous(labels = percent, limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Share Reported and Distance From Final Result After Poll Close",
      subtitle = paste0(scope, ", one dot per race"),
      x = "Share of the final vote reported", y = "Distance from final two-party share (pp)",
      caption = caption, tag = notes(close_note, excluded_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right", panel.spacing.x = unit(1.5, "lines"))
  ggsave_medsl(fs::path(out_dir, "6_share_vs_distance_by_benchmark.png"), p6, width = 11, height = 5.5)

  message("Wrote cross-state comparison to ", out_dir)
  invisible(out_dir)
}
