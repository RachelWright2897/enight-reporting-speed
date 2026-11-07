# Figure 7: the final margin by vote method, as two graphs so each gets its own bubble scale:
# race-wide (the state, or a U.S. House district), and in each of the 20 largest counties. Skipped
# for a race that reports one method only or race-wide totals only. Writes, for each race
# (arguments: see R/races.R):
#   outputs/fig7_method_margin_by_county/{race_id}_method_margin_statewide.png   (districtwide for House)
#   outputs/fig7_method_margin_by_county/{race_id}_method_margin_by_county.png
#   Rscript code/fig7_method_margin_by_county.R "<dropbox>/26_general" sen GA,PA
source("setup.R")

max_counties <- 20

plot_method_margin_by_county <- function(x, fig_dir = file.path(OUTPUT_DIR, "fig7_method_margin_by_county")) {
  area <- if (grepl("_\\d+$", x$race_id)) "districtwide" else "statewide"
  statewide_file <- fresh_figure(fig_dir, paste0(x$race_id, "_method_margin_", area, ".png"))
  county_file <- fresh_figure(fig_dir, paste0(x$race_id, "_method_margin_by_county.png"))
  m <- method_context(x)
  if (is.null(m)) {
    message(x$race_id, ": one vote method only, so no figure 7")
    return(invisible(NULL))
  }
  if (!x$has_counties) {
    message(x$race_id, ": ", area, " totals only, so no figure 7")
    return(invisible(NULL))
  }
  ctx <- race_context(x)
  margin_axis <- scale_x_continuous(breaks = margin_breaks, labels = \(v) ctx$label_margin(round(v, 1)))
  x_title <- paste(x$margin_label, "margin (pp)")

  # Race-wide: one row per method, sized by that method's votes
  statewide <- x$methods |>
    mutate(method = factor(method, levels = rev(m$levels)))
  p_state <- ggplot(statewide, aes(final_margin, method)) +
    geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_point(aes(color = method, size = final_votes)) +
    m$scale +
    scale_y_discrete(labels = method_labels) +
    scale_size_area(max_size = 10, labels = comma, name = "Votes") +
    margin_axis +
    guides(color = guide_legend(order = 1), size = guide_legend(order = 2)) +
    labs(
      title = paste(tools::toTitleCase(area), "Final Margin by Vote Method"),
      subtitle = coverage_subtitle(x, ctx),
      x = x_title, y = NULL,
      caption = ctx$caption
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right", legend.box.spacing = unit(2.5, "lines"))
  ggsave_medsl(statewide_file, p_state, width = 8, height = 5)
  message("Wrote ", statewide_file)

  # Counties: feeds can cover many counties; show the largest ones, sized on their own scale
  top_counties <- head(x$county_votes$county, max_counties)
  county_note <- if (nrow(x$county_votes) > max_counties) paste0("Top ", max_counties, " counties by votes.")
  counties <- x$county_methods |>
    filter(county %in% top_counties) |>
    mutate(
      county = factor(county, levels = rev(top_counties)),
      method = factor(method, levels = m$levels)
    )
  p_county <- ggplot(counties, aes(final_margin, county)) +
    geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_point(aes(color = method, size = final_votes), alpha = 0.85) +
    m$scale +
    scale_size_area(max_size = 6, labels = comma, name = "Votes") +
    margin_axis +
    labs(
      title = "Final Margin by Vote Method and County",
      subtitle = ctx$scope,
      x = x_title, y = NULL,
      caption = ctx$caption, tag = join_notes(county_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right", legend.box.spacing = unit(2.5, "lines"))
  ggsave_medsl(county_file, p_county, width = 8, height = 5.5)
  message("Wrote ", county_file)

  invisible(c(statewide_file, county_file))
}

if (sys.nframe() == 0) invisible(for_each_race(race_run(), plot_method_margin_by_county))
