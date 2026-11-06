# Figure 4: the race-wide running margin over election night against the final result.
# Writes outputs/fig4_running_margin/{race_id}_running_margin.png for each race (arguments: see R/races.R).
#   Rscript code/fig4_running_margin.R "<dropbox>/26_general" sen GA,PA
source("setup.R")

plot_running_margin <- function(x, fig_dir = file.path(OUTPUT_DIR, "fig4_running_margin")) {
  out_file <- fresh_figure(fig_dir, paste0(x$race_id, "_running_margin.png"))
  ctx <- race_context(x)
  final_margin <- ctx$final_margin

  # the first few batches swing wildly (R +45 on a handful of ballots), so start once
  # 5% of the final vote is in, the same start the comparison uses
  race <- ctx$race |> filter(total > 0, share_counted >= 0.05)
  # put the "Final" label where the running margin is farthest from it, on the
  # opposite side of the dashed line, so the two never overlap
  gap <- race$margin - final_margin
  label_at <- which.max(abs(gap))
  label_below <- gap[label_at] > 0
  headroom <- 0.08 * diff(range(c(race$margin, final_margin)))
  race <- ctx$in_window(race)

  p <- ggplot(race, aes(time, margin)) +
    geom_hline(yintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_hline(yintercept = final_margin, color = medsl_colors[["gold"]], linewidth = 0.6, linetype = "dashed") +
    annotate("text", x = race$time[label_at], y = final_margin,
             vjust = if (label_below) 1.6 else -0.6, hjust = 0,
             label = paste0(if (ctx$use_ap_margin) ctx$final_label else ctx$final_word, ": ", ctx$label_margin(round(final_margin, 1))),
             family = "StyreneB-Regular", size = 3.4, color = "#444444") +
    expand_limits(y = final_margin + if (label_below) -headroom else headroom) +
    geom_step(color = "#444444", linewidth = 0.6) +
    geom_point(aes(color = share_counted), size = 1.4) +
    # share can top 100% briefly before a downward correction; show it as fully counted
    scale_color_medsl_blue_c(labels = percent, limits = c(0, 1), breaks = c(0, 0.5, 1), oob = squish,
                             name = if (ctx$use_expected) "Share of AP expected vote counted"
                                    else if (ctx$partial) "Share of votes in last snapshot"
                                    else "Share of final vote counted") +
    guides(color = guide_colorbar(theme = theme(legend.key.width = unit(8, "lines")))) +
    scale_y_continuous(labels = \(v) ctx$label_margin(round(v, 1))) +
    ctx$time_scale +
    labs(
      title = "Running Race Margin Over Election Night",
      subtitle = ctx$scope,
      x = "Time (ET)", y = paste(x$margin_label, "margin (pp)"),
      caption = ctx$caption, tag = join_notes(ctx$late_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(
      axis.text = element_text(size = rel(0.8)), axis.title = element_text(size = rel(0.85)),
      legend.text = element_text(size = rel(0.8)), legend.title = element_text(size = rel(0.85))
    )

  ggsave_medsl(out_file, p, width = 8, height = 5)
  message("Wrote ", out_file)
  invisible(out_file)
}

if (sys.nframe() == 0) invisible(for_each_race(race_run(), plot_running_margin))
