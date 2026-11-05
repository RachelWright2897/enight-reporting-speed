# Figure 6: the running margin within each vote method over election night. Skipped for a race
# that reports one method only. Writes outputs/fig6_method_running_margin/{race_id}_method_running_margin.png for
# each race (arguments: see R/races.R).
#   Rscript code/fig6_method_running_margin.R "<dropbox>/26_general" sen GA,PA
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
  if (ctx$partial) {
    method_run <- mutate(method_run, share_counted = total / last(total[time <= ctx$cutoff]), .by = method)
  }
  method_run <- method_run |>
    ctx$in_window() |>
    mutate(method = factor(method, levels = m$levels))

  # A method's first few votes swing wildly, so each line starts once 5% of that method is in.
  p <- ggplot(filter(method_run, share_counted >= 0.05), aes(time, margin, color = method)) +
    geom_hline(yintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_hline(yintercept = ctx$final_margin, color = "#444444", linewidth = 0.5, linetype = "dashed") +
    annotate("text", x = max(method_run$time), y = ctx$final_margin, vjust = 1.6, hjust = 1,
             label = paste0(if (ctx$partial) ctx$final_word else "Final vote margin",
                            " (all methods): ", ctx$label_margin(round(ctx$final_margin, 1))),
             family = "StyreneB-Regular", size = 3.4, color = "#444444") +
    geom_step(linewidth = 0.5) +
    m$scale +
    scale_y_continuous(labels = \(v) ctx$label_margin(round(v, 1))) +
    ctx$time_scale +
    labs(
      title = "Running Margin by Vote Method Over Election Night",
      subtitle = ctx$scope,
      x = "Time (ET)", y = paste(x$margin_label, "margin (pp)"),
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
