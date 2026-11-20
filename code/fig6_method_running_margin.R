# Figure 6: the running margin within each vote method over election night. Skipped for a race
# that reports one method only. Writes outputs/fig6_method_running_margin/{race_id}_method_running_margin.png for
# each race (arguments: see R/races.R).
#   Rscript code/fig6_method_running_margin.R --race sen --state GA,PA
source("setup.R")

plot_method_running_margin <- function(x, fig_dir = file.path(OUTPUT_DIR, "fig6_method_running_margin")) {
  out_file <- fresh_figure(fig_dir, paste0(x$race_id, "_method_running_margin.png"))
  m <- method_context(x)
  if (is.null(m)) {
    message(x$race_id, ": one vote method only, so no figure 6")
    return(invisible(NULL))
  }
  ctx <- race_context(x)

  # When the feed stopped short, each method's share (used for the 5% start) is measured
  # against that method's votes at the snapshot the caption's AP percent describes (the feed's
  # last, `cutoff`). AP has no vote methods, so there is no per-method expected vote.
  method_run <- x$method_running

  subtitle <- coverage_subtitle(x, ctx)
  if (ctx$partial) {
    method_run <- mutate(method_run, share_counted = total / last(total[time <= ctx$cutoff]), .by = method)
  }
  method_run <- method_run |>
    ctx$in_window() |>
    mutate(method = factor(method, levels = m$levels))

  # x axis: hours since the state's polls closed (helper_files/{folder}_poll_close.csv), or since
  # the race's first results when the state has no close time
  close <- poll_close_time(ctx$state)
  x_title <- if (is.na(close)) {
    close <- min(method_run$time[method_run$total > 0])
    "Hours since first results"
  } else {
    paste0("Hours since polls closed (", sub("^0", "", format(close, "%I:%M %p ET", tz = TZ)) |> sub(pattern = ":00", replacement = ""), ")")
  }
  method_run <- mutate(method_run, hours = as.numeric(difftime(time, close, units = "hours")))

  # A method's first few votes swing wildly, so each line starts once 5% of that method is in.
  shown <- filter(method_run, share_counted >= 0.05)

  # The x axis ends just after the last report. The reference line is labeled on one line at
  # its right end, on whichever side (above or below) has more room from the method lines there.
  ref_label <- paste0(
    if (ctx$use_ap_margin) paste0(ctx$area, " (AP, ", round(ctx$ap_latest_pct), "% reporting)")
    else if (ctx$partial) paste0("All methods at ", round(ctx$ap$pct), "% reporting")
    else "Final, all methods",
    ": ", ctx$label_margin(round(ctx$final_margin, 1))
  )
  end_margins <- shown |> slice_max(time, n = 1, by = method) |> pull(margin)
  gap_above <- min(c(Inf, end_margins[end_margins >= ctx$final_margin] - ctx$final_margin))
  gap_below <- min(c(Inf, ctx$final_margin - end_margins[end_margins < ctx$final_margin]))
  label_vjust <- if (gap_above >= gap_below) -0.5 else 1.5

  p <- ggplot(shown, aes(hours, margin, color = method)) +
    geom_hline(yintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +   # polls close
    annotate("segment", x = -Inf, xend = max(method_run$hours), y = ctx$final_margin, yend = ctx$final_margin,
             color = "#444444", linewidth = 0.5, linetype = "dashed") +
    geom_step(linewidth = 0.5) +
    annotate("text", x = max(method_run$hours), y = ctx$final_margin, hjust = 1, vjust = label_vjust,
             label = ref_label, family = "StyreneB-Regular", size = 3, color = "#444444") +
    m$scale +
    scale_y_continuous(breaks = margin_breaks, labels = \(v) ctx$label_margin(round(v, 1))) +
    hours_axis(min(shown$hours), max(method_run$hours), right_pad = 0.02) +
    labs(
      title = "Share of Each Vote Method Counted",
      subtitle = subtitle,
      x = x_title, y = paste(x$margin_label, "margin (pp)"),
      caption = ctx$caption,
      tag = join_notes("Lines start once 5% of a method is counted.", ctx$late_note)
    ) +
    theme_medsl() +
    note_theme

  ggsave_medsl(out_file, p, width = 8, height = 5)
  message("Wrote ", out_file)
  invisible(out_file)
}

if (sys.nframe() == 0) invisible(for_each_race(race_run(), plot_method_running_margin))
