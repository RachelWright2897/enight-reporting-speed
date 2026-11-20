# Figure 5: the share of each vote method's ballots counted over the night. Without --race, one
# figure per state across every contest (ballots_by_method() in R/snapshot_tallies.R), written to
# outputs/fig5_method_counted/{STATE}_method_counted.png. With --race, one figure per race from
# that race's own votes by method, written to {race_id}_method_counted.png (arguments: see R/races.R).
#   Rscript code/fig5_method_counted.R --state GA,PA
#   Rscript code/fig5_method_counted.R --state GA --race sen
#   Rscript code/fig5_method_counted.R --state GA --race sen --start 0 --end 8
#
# --start and --end set the period drawn (default polls close to 3 days after): a number is hours
# after polls close, "21:00" a time on election day, "2026-11-06" or "2026-11-06 21:00" a date
# and time (ET). --end last runs through the last snapshot.
#
# When AP carries a top-of-ticket race (president, Senate or governor) and has under 99% reporting
# as of the feed's last snapshot, the caption gives AP's percent and each method is measured
# against its ballots at that snapshot.
source("setup.R")

# The axis is log2(hours + 0.5), so the first hour gets room while day 3 still fits. By default
# it runs from polls close to 3 days after, with ticks at polls close, 30 minutes, then 1, 2, 4,
# 8, 12 and 24 hours and 3 days; a narrower --start/--end gets finer ticks.
close_axis_max <- 72
close_axis_ticks <- c(0, 0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24, 36, 48, 72, 96, 120, 168, 240, 336, 504, 720)
close_axis_default <- c(0, 0.5, 1, 2, 4, 8, 12, 24, 72, 168, 336, 720)

hours_label <- \(h) case_when(
  h == 0 ~ "Polls\nclose",
  h < 1 ~ paste(round(h * 60), "min"),
  h < 48 ~ paste(h, "hr"),
  .default = paste(h / 24, "days")
)

close_hours_axis <- function(from = 0, to = close_axis_max) {
  breaks <- close_axis_default[close_axis_default >= from & close_axis_default <= to]
  if (length(breaks) < 4) breaks <- close_axis_ticks[close_axis_ticks >= from & close_axis_ticks <= to]
  if (length(breaks) < 3) breaks <- scales::breaks_pretty(4)(c(from, to))
  breaks <- breaks[breaks >= from & breaks <= to]
  # a tick near the end, when the last one is far short of it (53 hours ends at 48)
  near_end <- max(close_axis_ticks[close_axis_ticks <= to])
  if (to > 1.5 * max(breaks) && near_end > max(breaks)) breaks <- c(breaks, near_end)
  scale_x_continuous(
    transform = scales::new_transform("close_log", \(x) log2(x + 0.5), \(x) 2^x - 0.5),
    breaks = breaks, labels = hours_label(breaks),
    limits = c(from, to), expand = expansion(mult = c(0.01, 0.02)),
    minor_breaks = NULL
  )
}

# --start and --end for this script; race_run() takes the rest
fig5_window_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list(start = NULL, end = NULL)
  rest <- character()
  args <- unlist(lapply(args, \(a) if (grepl("^--(start|end)=", a)) c(sub("=.*", "", a), sub("^[^=]*=", "", a)) else a))
  i <- 1
  while (i <= length(args)) {
    if (args[i] %in% c("--start", "--end") && i < length(args)) {
      opts[[sub("^--", "", args[i])]] <- args[i + 1]
      i <- i + 2
    } else {
      rest <- c(rest, args[i])
      i <- i + 1
    }
  }
  # check the forms now, before the files are read
  probe <- as.POSIXct(ELECTION_DATE, tz = TZ)
  from <- timeline_bound(opts$start, probe)
  to <- if (!identical(tolower(opts$end %||% ""), "last")) timeline_bound(opts$end, probe)
  numeric <- \(x) !is.null(x) && grepl("^-?[0-9.]+$", x)
  if (!is.null(from) && !is.null(to) && numeric(opts$start) == numeric(opts$end) && from >= to) {
    cli::cli_abort("--start ({.val {opts$start}}) must come before --end ({.val {opts$end}}).")
  }
  c(opts, list(rest = rest))
}

#' One state's figure, across every contest: b from ballots_by_method()
plot_method_counted <- function(b, state, window = list()) {
  cutoff <- max(b$time)
  ap <- NULL
  for (contest in if (figure_ap()) c("pres", "sen", "gov")) {
    ap <- ap_race_at(state, contest, NA, cutoff)
    if (!is.null(ap)) break
  }
  caption <- medsl_caption(source = "state and county election websites")
  if (!is.null(ap)) caption <- paste0(caption, "\n", ap_caption_line(ap, state, contest, NA, cutoff))
  state_name <- coalesce(state.name[match(state, state.abb)], state)
  draw_method_counted(
    b, state, out_name = paste0(state, "_method_counted.png"),
    subtitle = paste(state_name, "ballots, all contests"), caption = caption, ap = ap,
    method_note = "Ballots estimated as the most votes in any contest, by precinct and method.",
    window = window
  )
}

#' One race's figure: x from build_race(), each method's votes in that race alone
plot_race_method_counted <- function(x, window = list()) {
  ctx <- race_context(x)
  b <- transmute(x$method_running, method, time, ballots = total, share_counted)
  draw_method_counted(b, ctx$state, out_name = paste0(x$race_id, "_method_counted.png"),
                      subtitle = ctx$scope, caption = ctx$caption, ap = ctx$ap, unit = "votes",
                      window = window)
}

#' b: one row per method x snapshot with ballots and share_counted. ap: AP's count at the
#' feed's last snapshot (ap_race_at()) or NULL, for the y-axis title when the feed stopped short.
#' window: --start and --end as given (fig5_window_args()); either can be NULL.
draw_method_counted <- function(b, state, out_name, subtitle, caption, ap = NULL, method_note = NULL, unit = "ballots",
                                window = list(), fig_dir = file.path(OUTPUT_DIR, "fig5_method_counted")) {
  method_levels <- intersect(names(method_labels), unique(b$method[b$ballots > 0]))
  if (length(method_levels) < 2) {
    message(sub("\\.png$", "", out_name), ": one vote method only, so no figure 5")
    return(invisible(NULL))
  }
  cutoff <- max(b$time)
  partial <- !is.null(ap) && ap$pct < 99

  # The x-axis is hours after polls close, out to 3 days. Without a poll close time for the
  # state (helper_files/{folder}_poll_close.csv), hours count from the first results.
  close_time <- poll_close_time(state)
  close_label <- "polls close"
  if (is.na(close_time)) {
    close_time <- min(b$time[b$ballots > 0])
    close_label <- "first results"
    message(state, ": no poll close time, so hours count from the first results")
  }
  # The period drawn: --start (default polls close; nothing before it, since the axis starts
  # there) to --end (default 3 days after; "last" the last snapshot), never past the last snapshot
  window_start <- max(timeline_bound(window$start, close_time) %||% close_time, close_time)
  user_end <- if (identical(tolower(window$end %||% ""), "last")) cutoff else timeline_bound(window$end, close_time)
  window_end <- min(user_end %||% (close_time + close_axis_max * 3600), cutoff)
  if (is.null(window$end) && inherits(getOption("enr.end"), "POSIXct")) window_end <- min(window_end, getOption("enr.end"))
  if (window_start >= window_end) {
    message(sub("\\.png$", "", out_name), ": no snapshots between --start and --end, so no figure 5")
    return(invisible(NULL))
  }
  to_hours <- \(t) as.numeric(difftime(t, close_time, units = "hours"))
  from_h <- to_hours(window_start)
  to_h <- to_hours(window_end)

  windowed <- !is.null(window$start) || !is.null(window$end)
  if (windowed) {
    stamp <- \(t) format(t, "%Y_%m_%d_%H_%M", tz = TZ)
    out_name <- sub("\\.png$", paste0("_", stamp(window_start), "_to_", stamp(window_end), ".png"), out_name)
    subtitle <- paste0(subtitle, ", ", timeline_clock(window_start), "\u2013", timeline_clock(window_end), " ET")
  }
  out_file <- fresh_figure(fig_dir, out_name)

  all_ballots <- summarise(b, ballots = sum(ballots), .by = time) |> arrange(time)
  shown <- all_ballots$ballots[all_ballots$time <= window_end]
  final <- last(all_ballots$ballots)
  hidden <- if (length(shown) && final > 0) (final - last(shown)) / final else 0
  late_note <- if (hidden > 0.0005) {
    end_hours <- round(to_h)
    paste0("Lines end ", if (end_hours %% 24 == 0 && end_hours >= 48) paste(end_hours / 24, "days") else paste(end_hours, "hours"),
           " after ", close_label, "; ", percent(hidden, accuracy = 0.1), " of ", unit, " were counted later.")
  }

  # Each line starts at what was counted by the period's start: the last snapshot before it is
  # drawn at the start (snapshots before polls close sit at 0 hours)
  d <- b |>
    filter(ballots > 0, time <= window_end, method %in% method_levels) |>
    mutate(method = factor(method, levels = method_levels),
           hours = pmax(to_hours(time), from_h)) |>
    arrange(method, time) |>
    filter(hours > from_h | lead(hours, default = Inf) > from_h, .by = method)
  # with a set period, each line runs on to its end at the last count
  if (windowed) d <- bind_rows(d, slice_max(d, time, n = 1, by = method) |> mutate(hours = to_h))

  p <- ggplot(d, aes(hours, share_counted, color = method)) +
    geom_step(linewidth = 0.5) +
    scale_color_manual(values = method_colors[method_levels], labels = method_labels[method_levels],
                       breaks = method_levels, name = "Vote method") +
    scale_y_continuous(labels = percent) +
    expand_limits(y = c(0, 1)) +
    close_hours_axis(from_h, to_h) +
    labs(
      title = "Share of Each Vote Method Counted",
      subtitle = subtitle,
      x = if (close_label == "polls close") "Time after polls close" else "Time after first results",
      y = if (partial) paste0("Share of method's ", unit, " at ", round(ap$pct), "% reporting")
          else paste0("Share of method's ", unit, " counted"),
      caption = caption,
      tag = join_notes(method_note, late_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(
      legend.title.position = "top",                   # title above the method names
      legend.title = element_text(hjust = 0.5),
      legend.box.spacing = unit(1.5, "lines")          # legend sits lower, toward the bottom
    )

  ggsave_medsl(out_file, p, width = 8, height = 5)
  message("Wrote ", out_file)
  invisible(out_file)
}

if (sys.nframe() == 0) {
  window <- fig5_window_args()
  run <- race_run(window$rest)
  if (identical(run$contest, "all")) {
    for_each_state(run, \(state, clean_dir, election_date) {
      plot_method_counted(ballots_by_method(clean_dir, election_date = election_date), state, window)
    })
  } else {
    invisible(for_each_race(run, \(x) plot_race_method_counted(x, window)))
  }
}
