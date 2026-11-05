# Figure 7: the final margin by vote method, statewide and in each of the 20 largest counties.
# Skipped for a race that reports one method only or statewide totals only. Writes
# outputs/fig7_method_margin_by_county/{race_id}_method_margin_by_county.png for each race (arguments: see R/races.R).
#   Rscript code/fig7_method_margin_by_county.R "<dropbox>/26_general" sen GA,PA
source("setup.R")

max_counties <- 20

plot_method_margin_by_county <- function(x, fig_dir = file.path(OUTPUT_DIR, "fig7_method_margin_by_county")) {
  out_file <- fresh_figure(fig_dir, paste0(x$race_id, "_method_margin_by_county.png"))
  m <- method_context(x)
  if (is.null(m)) {
    message(x$race_id, ": one vote method only, so no figure 7")
    return(invisible(NULL))
  }
  if (!x$has_counties) {
    message(x$race_id, ": statewide totals only, so no figure 7")
    return(invisible(NULL))
  }
  ctx <- race_context(x)

  # Statewide feeds cover every county; show the largest ones
  top_counties <- head(x$county_votes$county, max_counties)
  county_note <- if (nrow(x$county_votes) > max_counties) paste0("Top ", max_counties, " counties by votes.")
  statewide_label <- paste(ctx$state_name, "total")
  gaps <- bind_rows(
    mutate(x$methods, county = statewide_label),
    filter(x$county_methods, county %in% top_counties)
  ) |>
    mutate(
      county = factor(county, levels = c(rev(top_counties), statewide_label)),
      method = factor(method, levels = m$levels)
    )

  p <- ggplot(gaps, aes(final_margin, county)) +
    geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_point(aes(color = method, size = final_votes)) +
    m$scale +
    scale_size_area(max_size = 6, labels = comma, name = "Votes") +
    scale_x_continuous(labels = \(v) ctx$label_margin(round(v, 1))) +
    labs(
      title = "Final Margin by Vote Method and County",
      subtitle = ctx$scope,
      x = paste(x$margin_label, "margin (pp)"), y = NULL,
      caption = ctx$caption, tag = join_notes(county_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right", legend.box.spacing = unit(2.5, "lines"))

  ggsave_medsl(out_file, p, width = 8, height = 5.5)
  message("Wrote ", out_file)
  invisible(out_file)
}

if (sys.nframe() == 0) invisible(for_each_race(race_run(), plot_method_margin_by_county))
