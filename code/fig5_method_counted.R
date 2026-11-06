# Figure 5: the share of each vote method's ballots counted over the night, one figure per
# state across every contest (ballots_by_method() in R/snapshot_tallies.R) rather than per race.
# Writes outputs/fig5_method_counted/{STATE}_method_counted.png (arguments: see R/races.R; the
# contest argument is ignored).
#   Rscript code/fig5_method_counted.R "<dropbox>/26_general" all GA,PA
#
# When AP carries a top-of-ticket race (president, Senate or governor) and has under 99% reporting
# as of the feed's last snapshot, the caption gives AP's percent and each method is measured
# against its ballots at that snapshot.
source("setup.R")

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
  for (contest in c("pres", "sen", "gov")) {
    ap <- ap_race_at(state, contest, NA, cutoff)
    if (!is.null(ap)) break
  }
  partial <- !is.null(ap) && ap$pct < 99
  caption <- medsl_caption(source = "state and county election websites")
  if (!is.null(ap)) caption <- paste0(caption, "\n", ap_caption_line(ap, state, contest, NA, cutoff))

  # Ends 1 hour after the last update that matters, as the race figures do (vote_window())
  all_ballots <- summarise(b, ballots = sum(ballots), .by = time) |> arrange(time)
  window <- vote_window(all_ballots$time, all_ballots$ballots, cutoff)
  window_end <- window$end
  late_note <- window$note

  d <- b |>
    filter(ballots > 0, time <= window_end, method %in% method_levels) |>
    mutate(method = factor(method, levels = method_levels))

  p <- ggplot(d, aes(time, share_counted, color = method)) +
    geom_step(linewidth = 0.5) +
    scale_color_manual(values = method_colors[method_levels], labels = method_labels[method_levels],
                       breaks = method_levels, name = "Vote method") +
    scale_y_continuous(labels = percent) +
    expand_limits(y = c(0, 1)) +
    time_axis(min(d$time), window_end) +
    labs(
      title = "Share of Each Vote Method Counted Over Election Night",
      subtitle = paste(state_name, "ballots, all contests"),
      x = "Time (ET)",
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
