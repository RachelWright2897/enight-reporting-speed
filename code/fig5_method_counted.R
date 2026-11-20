# Figure 5: the share of each vote method's ballots counted over the night, one figure per
# state across every contest (ballots_by_method() in R/snapshot_tallies.R) rather than per race.
# Writes outputs/fig5_method_counted/{STATE}_method_counted.png (arguments: see R/races.R; the
# --race is ignored).
#   Rscript code/fig5_method_counted.R --state GA,PA
#
# When AP carries a top-of-ticket race (president, Senate or governor) and has under 99% reporting
# as of the feed's last snapshot, the caption gives AP's percent and each method is measured
# against its ballots at that snapshot.
source("setup.R")

# Ticks at polls close, 30 minutes, then 1, 2, 4, 8, 12 and 24 hours and 3 days after.
# The axis is log2(hours + 0.5), so the first hour gets room while day 3 still fits.
close_axis_breaks <- c(0, 0.5, 1, 2, 4, 8, 12, 24, 72)
close_axis_labels <- c("Polls\nclose", "30 min", "1 hr", "2 hr", "4 hr", "8 hr", "12 hr", "24 hr", "3 days")
close_axis_max <- 72

close_hours_axis <- function() {
  scale_x_continuous(
    transform = scales::new_transform("close_log", \(x) log2(x + 0.5), \(x) 2^x - 0.5),
    breaks = close_axis_breaks, labels = close_axis_labels,
    limits = c(0, close_axis_max), expand = expansion(mult = c(0.01, 0.02)),
    minor_breaks = NULL
  )
}

plot_method_counted <- function(b, state, fig_dir = file.path(OUTPUT_DIR, "fig5_method_counted")) {
  out_file <- fresh_figure(fig_dir, paste0(state, "_method_counted.png"))
  state_name <- coalesce(state.name[match(state, state.abb)], state)

  method_levels <- intersect(names(method_labels), unique(b$method[b$ballots > 0]))
  if (length(method_levels) < 2) {
    message(state, ": one vote method only, so no ballots-by-method figure")
    return(invisible(NULL))
  }

  cutoff <- max(b$time)
  ap <- NULL
  for (contest in if (figure_ap()) c("pres", "sen", "gov")) {
    ap <- ap_race_at(state, contest, NA, cutoff)
    if (!is.null(ap)) break
  }
  partial <- !is.null(ap) && ap$pct < 99
  caption <- medsl_caption(source = "state and county election websites")
  if (!is.null(ap)) caption <- paste0(caption, "\n", ap_caption_line(ap, state, contest, NA, cutoff))

  # The x-axis is hours after polls close, out to 3 days. Without a poll close time for the
  # state (helper_files/{folder}_poll_close.csv), hours count from the first results.
  close_time <- poll_close_time(state)
  close_label <- "polls close"
  if (is.na(close_time)) {
    close_time <- min(b$time[b$ballots > 0])
    close_label <- "first results"
    message(state, ": no poll close time, so hours count from the first results")
  }
  all_ballots <- summarise(b, ballots = sum(ballots), .by = time) |> arrange(time)
  window_end <- min(close_time + close_axis_max * 3600, cutoff)
  user_end <- getOption("enr.end")
  if (inherits(user_end, "POSIXct")) window_end <- min(window_end, user_end)
  shown <- all_ballots$ballots[all_ballots$time <= window_end]
  final <- last(all_ballots$ballots)
  hidden <- if (length(shown) && final > 0) (final - last(shown)) / final else 0
  late_note <- if (hidden > 0.0005) {
    end_hours <- round(as.numeric(difftime(window_end, close_time, units = "hours")))
    paste0("Lines end ", if (end_hours == close_axis_max) "3 days" else paste(end_hours, "hours"),
           " after ", close_label, "; ", percent(hidden, accuracy = 0.1), " of ballots were counted later.")
  }

  # Snapshots before polls close sit at 0, so each line starts at what was counted by then
  d <- b |>
    filter(ballots > 0, time <= window_end, method %in% method_levels) |>
    mutate(method = factor(method, levels = method_levels),
           hours = pmax(as.numeric(difftime(time, close_time, units = "hours")), 0)) |>
    arrange(method, hours)

  p <- ggplot(d, aes(hours, share_counted, color = method)) +
    geom_step(linewidth = 0.5) +
    scale_color_manual(values = method_colors[method_levels], labels = method_labels[method_levels],
                       breaks = method_levels, name = "Vote method") +
    scale_y_continuous(labels = percent) +
    expand_limits(y = c(0, 1)) +
    close_hours_axis() +
    labs(
      title = "Share of Each Vote Method Counted",
      subtitle = paste(state_name, "ballots, all contests"),
      x = if (close_label == "polls close") "Time after polls close" else "Time after first results",
      y = if (partial) paste0("Share of method's ballots at ", round(ap$pct), "% reporting")
          else "Share of method's ballots counted",
      caption = caption,
      tag = join_notes("Ballots estimated as the most votes in any contest, by precinct and method.", late_note)
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
  for_each_state(race_run(), \(state, clean_dir, election_date) {
    plot_method_counted(ballots_by_method(clean_dir, election_date = election_date), state)
  })
}
