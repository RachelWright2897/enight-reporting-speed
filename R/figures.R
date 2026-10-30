# Figure functions. make_figures() writes the four figures for one state's race
# from the reporting.rds that build_race() saves.

library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(ggmedsl)

medsl_fonts(dpi = 300)

# Vote methods are unordered, non-partisan categories, so they use the brand
# categorical palette (palette.md), pinned per method so a method keeps its color
# in every figure and every state. ggmedsl doesn't export the palette itself.
method_colors <- c(
  early = "#948DE5", mail = "#C0BA79", eday = "#37C256", prov = "#FF8318",
  abs = "#59CBF5", adv = "#4E4A81", all = "#0B2E4F"
)
method_labels <- c(
  early = "Early in-person", mail = "Mail", eday = "Election day", prov = "Provisional",
  abs = "Absentee", adv = "Advance", all = "All methods (not split)"
)

make_figures <- function(in_dir) {
  x <- readRDS(fs::path(in_dir, "reporting.rds"))
  fig_dir <- fs::path(in_dir, "figures")
  fs::dir_create(fig_dir)

  TZ <- "America/New_York"
  simulated <- isTRUE(x$simulated)
  data_source <- if (simulated) "Simulated data" else "State and county reporting sites, via enightdata"
  caption <- medsl_caption(source = data_source)

  # race_id is {STATE}_{contest}[_{dem|rep}][_{district}], e.g. GA_gov_rep, TX_house_7
  id <- str_match(x$race_id, "^([A-Z]{2})_([a-z]+)(?:_(dem|rep))?(?:_(\\d+))?$")
  state <- id[, 2]
  contest <- id[, 3]
  primary <- id[, 4]
  district <- id[, 5]
  race_name <- case_when(
    contest == "sen" ~ "U.S. Senate", contest == "gov" ~ "Governor", contest == "house" ~ "U.S. House",
    contest == "ltgov" ~ "Lieutenant Governor", contest == "ag" ~ "Attorney General",
    contest == "sos" ~ "Secretary of State", contest == "supcrt" ~ "Supreme Court",
    .default = str_to_upper(contest)
  )
  if (!is.na(district)) race_name <- paste(race_name, "District", district)
  if (!is.na(primary)) {
    race_name <- paste0(race_name, ", ", c(dem = "Democratic", rep = "Republican")[[primary]], " primary")
  }
  state_name <- coalesce(state.name[match(state, state.abb)], state)
  scope <- paste0(state_name, " ", race_name, if (simulated) ", simulated election night" else "")

  # Margin labels: "D +10" / "R +10" in a general, candidate surnames in a primary
  sides <- str_split_1(x$margin_label, "–")
  partisan <- identical(sides, c("D", "R"))
  label_margin <- \(v) case_when(
    is.na(v) ~ NA_character_,
    round(v, 1) == 0 ~ "Even",
    v > 0 ~ paste0(sides[1], " +", abs(v)),
    TRUE ~ paste0(sides[2], " +", abs(v))
  )
  # Partisan margins use the diverging party scale; candidate-vs-candidate margins
  # in a primary must not read as D vs R, so they use purple and gold instead.
  margin_color_scale <- \(name, lim = 40) {
    if (partisan) {
      scale_color_medsl_diverging(midpoint = 0, limits = c(-lim, lim), oob = squish,
                                  labels = label_margin, name = name)
    } else {
      scale_color_gradient2(low = medsl_colors[["gold"]], mid = "#F0F0F0", high = medsl_colors[["purple"]],
                            midpoint = 0, limits = c(-lim, lim), oob = squish,
                            labels = label_margin, name = name)
    }
  }
  # Time figures end once 99% of the final vote is in, so one late canvass update
  # doesn't squeeze election night into a sliver. Totals still use the last snapshot.
  counted <- filter(x$race, total > 0)
  window_end <- min(counted$time[counted$share_counted >= 0.99]) + hours(1)
  window_end <- min(window_end, max(x$race$time))
  in_window <- \(d) filter(d, time <= window_end)
  late_note <- if (max(x$updates$time) > window_end) {
    when <- format(window_end, "%b %d, %I:%M %p", tz = TZ) |> str_replace_all(c(" 0" = " "))
    paste0("Updates after 99% counted (", when, ") not shown.")
  } else {
    NULL
  }

  # Hourly ticks for one night; daily ticks when counting ran on through the canvass
  span_hours <- as.numeric(difftime(window_end, min(counted$time), units = "hours"))
  time_scale <- if (span_hours <= 36) {
    scale_x_datetime(
      date_breaks = if (span_hours <= 12) "1 hour" else "3 hours", timezone = TZ,
      labels = \(t) str_remove(format(t, "%I %p", tz = TZ), "^0")
    )
  } else {
    scale_x_datetime(
      date_breaks = case_when(span_hours <= 24 * 10 ~ "1 day", span_hours <= 24 * 30 ~ "3 days", .default = "1 week"),
      timezone = TZ,
      labels = \(t) str_replace(format(t, "%b %d", tz = TZ), " 0", " ")
    )
  }
  simulated_note <- if (simulated) "Simulated data for testing, not real results." else NULL

  # Margins on a handful of votes swing 100 points on one ballot, so the margin
  # figures (3, 8, 9) leave out precincts with fewer final votes than this
  min_precinct_votes <- 20
  small_note <- paste0("Precincts under ", min_precinct_votes, " votes left out.")

  # Statewide feeds cover every county; county-level figures show the largest ones
  max_counties <- 20
  county_votes <- x$precincts |>
    summarise(votes = sum(final_total), .by = county) |>
    arrange(desc(votes))
  top_counties <- head(county_votes$county, max_counties)
  capped <- nrow(county_votes) > max_counties
  county_note <- if (capped) paste0("Top ", max_counties, " counties by votes.") else NULL
  join_notes <- \(...) { notes <- c(...); if (length(notes)) paste(notes, collapse = " ") else NULL }
  note_theme <- theme(
    plot.tag.position = c(0.99, 0.02),
    plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0)
  )

  # --- 1. County count timeline --------------------------------------------------
  lane_order <- x$updates |>
    summarise(first = min(time), .by = county) |>
    arrange(desc(first)) |>
    pull(county)
  updates <- x$updates |>
    in_window() |>
    filter(county %in% top_counties) |>
    mutate(county = factor(county, levels = intersect(lane_order, top_counties)))

  p1 <- ggplot(updates, aes(time, county)) +
    geom_point(
      data = filter(updates, votes_added > 0),
      aes(size = votes_added, color = update_margin),
      alpha = 0.85
    ) +
    geom_point(
      data = filter(updates, votes_removed > 0),
      aes(shape = "Votes removed"),
      color = "#444444", size = 1.6, position = position_nudge(y = -0.28)
    ) +
    scale_size_area(max_size = 11, labels = comma, name = "Votes added") +
    margin_color_scale("Margin of added votes") +
    scale_shape_manual(values = c("Votes removed" = 4), name = NULL) +
    time_scale +
    labs(
      title = "Vote Count Updates by County Over Election Night",
      subtitle = scope,
      x = "Time (ET)", y = NULL,
      caption = caption, tag = join_notes(county_note, late_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right") +
    guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2), shape = guide_legend(order = 3))

  ggsave_medsl(fs::path(fig_dir, "1_county_timeline.png"), p1, width = 9, height = 5)

  # --- 2. How often precincts report --------------------------------------------
  reporting <- x$precincts |>
    filter(n_reports > 0, county %in% top_counties) |>
    count(county, n_reports)

  p2 <- ggplot(reporting, aes(n_reports, n)) +
    geom_col(fill = medsl_colors[["steel"]], width = 0.75) +
    facet_wrap(~county) +
    scale_x_continuous(breaks = pretty_breaks()) +
    labs(
      title = "Number of Precinct Reports by County",
      subtitle = paste0(scope, ", ", comma(nrow(x$precincts)), " precincts"),
      x = "Reports during the night", y = "Precincts",
      caption = caption, tag = join_notes(county_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme

  ggsave_medsl(fs::path(fig_dir, "2_reports_per_precinct.png"), p2, width = 8, height = 5)

  # --- 3. Interim precinct margins vs. each precinct's final ----------------------
  dev <- x$reports |> filter(is.finite(dev_from_final), final_total >= min_precinct_votes)
  # a precinct can briefly top 100% of its final count before a downward correction
  over_full <- sum(dev$share_counted > 1)
  dev <- filter(dev, share_counted <= 1)
  hidden_note <- paste0("Not shown: precincts under ", min_precinct_votes, " votes",
                        if (over_full > 0) paste0(", ", comma(over_full), " reports over 100%") else "", ".")

  p3 <- ggplot(dev, aes(share_counted, dev_from_final)) +
    geom_hline(yintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_point(color = medsl_colors[["steel"]], alpha = 0.25, size = 1.2,
               position = position_jitter(width = 0.005, height = 0, seed = 1)) +
    stat_summary_bin(fun = median, geom = "line", bins = 10,
                     color = medsl_colors[["navy"]], linewidth = 0.9) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = label_margin) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(
      title = "Interim Precinct Margins Relative to Final Precinct Margins",
      subtitle = paste0(scope, ", ", comma(nrow(dev)), " precinct reports"),
      x = "Share of the precinct's final vote counted",
      y = "Interim minus final margin (pp)",
      caption = caption,
      tag = join_notes("Line: median per 10-point bin.", hidden_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme

  ggsave_medsl(fs::path(fig_dir, "3_precinct_deviation.png"), p3, width = 8, height = 5)

  # --- 4. Race-wide running margin vs. the final result ---------------------------
  race <- x$race |> filter(total > 0)
  final_margin <- last(race$final_margin)
  race <- in_window(race)

  p4 <- ggplot(race, aes(time, margin)) +
    geom_hline(yintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_hline(yintercept = final_margin, color = medsl_colors[["gold"]], linewidth = 0.6, linetype = "dashed") +
    # label at the left end of the line, above it, with headroom so it isn't clipped
    annotate("text", x = min(race$time), y = final_margin, vjust = -0.6, hjust = 0,
             label = paste("Final:", label_margin(round(final_margin, 1))),
             family = "StyreneB-Regular", size = 3.4, color = "#444444") +
    expand_limits(y = final_margin + 0.08 * diff(range(c(race$margin, final_margin)))) +
    geom_step(color = "#444444", linewidth = 0.6) +
    geom_point(aes(color = share_counted), size = 2.2) +
    # share can top 100% briefly before a downward correction; show it as fully counted
    scale_color_medsl_blue_c(labels = percent, limits = c(0, 1), breaks = c(0, 0.5, 1), oob = squish,
                             name = "Share of final vote counted") +
    guides(color = guide_colorbar(theme = theme(legend.key.width = unit(8, "lines")))) +
    scale_y_continuous(labels = \(v) label_margin(round(v, 1))) +
    time_scale +
    labs(
      title = "Running Race Margin Over Election Night",
      subtitle = scope,
      x = "Time (ET)", y = paste(x$margin_label, "margin (pp)"),
      caption = caption, tag = join_notes(late_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme

  ggsave_medsl(fs::path(fig_dir, "4_running_margin.png"), p4, width = 8, height = 5)

  # --- 5-7. By vote method -----------------------------------------------------------
  # Sources that don't split by method report only "all", so there is nothing to compare
  if (is.null(x$methods) || nrow(x$methods) < 2) {
    message("Wrote 4 figures to ", fig_dir, " (one vote method only, so no method figures)")
    return(invisible(fig_dir))
  }

  method_levels <- intersect(names(method_labels), x$methods$method)
  method_scale <- scale_color_manual(
    values = method_colors[method_levels], labels = method_labels[method_levels],
    breaks = method_levels, name = "Vote method"
  )
  method_run <- x$method_running |>
    in_window() |>
    mutate(method = factor(method, levels = method_levels))

  # 5. How fast each method was counted
  p5 <- ggplot(filter(method_run, total > 0), aes(time, share_counted, color = method)) +
    geom_step(linewidth = 0.9) +
    method_scale +
    scale_y_continuous(labels = percent) +
    expand_limits(y = c(0, 1)) +
    time_scale +
    labs(
      title = "Share of Each Vote Method Counted Over Election Night",
      subtitle = scope,
      x = "Time (ET)", y = "Share of the method's final votes counted",
      caption = caption, tag = join_notes(late_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme

  ggsave_medsl(fs::path(fig_dir, "5_method_counted.png"), p5, width = 8, height = 5)

  # 6. Running margin within each method. A method's first few votes swing wildly,
  # so each line starts once 5% of that method is in.
  p6 <- ggplot(filter(method_run, share_counted >= 0.05), aes(time, margin, color = method)) +
    geom_hline(yintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_hline(yintercept = final_margin, color = "#444444", linewidth = 0.5, linetype = "dashed") +
    annotate("text", x = max(method_run$time), y = final_margin, vjust = 1.6, hjust = 1,
             label = paste("All methods, final:", label_margin(round(final_margin, 1))),
             family = "StyreneB-Regular", size = 3.4, color = "#444444") +
    geom_step(linewidth = 0.9) +
    method_scale +
    scale_y_continuous(labels = \(v) label_margin(round(v, 1))) +
    time_scale +
    labs(
      title = "Running Margin by Vote Method Over Election Night",
      subtitle = scope,
      x = "Time (ET)", y = paste(x$margin_label, "margin (pp)"),
      caption = caption,
      tag = join_notes("Lines start once 5% of a method is counted.", late_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme

  ggsave_medsl(fs::path(fig_dir, "6_method_running_margin.png"), p6, width = 8, height = 5)

  # 7. Final margin by method, statewide and in each county
  statewide_label <- paste(state_name, "total")
  county_order <- rev(top_counties)
  gaps <- bind_rows(
    mutate(x$methods, county = statewide_label),
    filter(x$county_methods, county %in% top_counties)
  ) |>
    mutate(
      county = factor(county, levels = c(county_order, statewide_label)),
      method = factor(method, levels = method_levels)
    )

  p7 <- ggplot(gaps, aes(final_margin, county)) +
    geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_line(aes(group = county), color = "#C4C4C4", linewidth = 0.8) +
    geom_point(aes(color = method, size = final_votes)) +
    method_scale +
    scale_size_area(max_size = 6, labels = comma, name = "Votes") +
    scale_x_continuous(labels = \(v) label_margin(round(v, 1))) +
    labs(
      title = "Final Margin by Vote Method and County",
      subtitle = scope,
      x = paste(x$margin_label, "margin (pp)"), y = NULL,
      caption = caption, tag = join_notes(county_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right")

  ggsave_medsl(fs::path(fig_dir, "7_method_margin_by_county.png"), p7, width = 8, height = 5.5)

  # --- 8-9. Swing: vote method mix vs. reporting speed ------------------------------
  # Only precincts that reported more than once can swing at all, so a precinct that
  # reported everything at once would look stable by construction; both figures
  # leave those out.
  if (is.null(x$swing)) {
    message("Wrote 7 figures to ", fig_dir, " (no swing tables; rebuild to add figures 8-9)")
    return(invisible(fig_dir))
  }
  multi <- x$swing |> filter(n_reports >= 2, is.finite(swing_max), final_total >= min_precinct_votes)
  multi_geos <- multi$geo

  # 8. Average interim deviation, split into method mix and within-method drift
  component_levels <- c(total = "Total deviation", mix = "From which methods were counted",
                        within = "Within vote methods")
  component_colors <- c("#0B2E4F", "#C0BA79", "#948DE5")   # navy, gold, purple (palette.md)
  interim <- x$decomp |> filter(geo %in% multi_geos, share_counted < 0.99)
  summarise_components <- \(d, by = NULL) summarise(d,
    total = mean(abs(dev_from_final)), mix = mean(abs(dev_mix)), within = mean(abs(dev_within)),
    reports = n(), .by = all_of(by))
  components <- bind_rows(
    mutate(summarise_components(interim), county = statewide_label),
    filter(summarise_components(interim, by = "county"), county %in% top_counties)
  ) |>
    tidyr::pivot_longer(c(total, mix, within), names_to = "component", values_to = "dev") |>
    mutate(
      county = factor(county, levels = c(rev(top_counties), statewide_label)),
      component = factor(component_levels[component], levels = component_levels)
    ) |>
    filter(!is.na(county))

  p8 <- ggplot(components, aes(dev, county)) +
    geom_line(aes(group = county), color = "#C4C4C4", linewidth = 0.8) +
    # total as an open ring, so it stays visible when a part lands on top of it
    geom_point(aes(color = component, shape = component, size = component), stroke = 1.2) +
    scale_color_manual(values = setNames(component_colors, component_levels), name = NULL) +
    scale_shape_manual(values = setNames(c(1, 16, 16), component_levels), name = NULL) +
    scale_size_manual(values = setNames(c(4, 2.6, 2.6), component_levels), name = NULL) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Sources of Interim Precinct Margin Deviation",
      subtitle = paste0(scope, ", ", comma(nrow(interim)), " interim reports from ",
                        comma(length(multi_geos)), " precincts"),
      x = "Mean absolute difference from final precinct margin (pp)", y = NULL,
      caption = caption,
      tag = join_notes("Parts can offset, so need not add to the total.", small_note, county_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "top")

  ggsave_medsl(fs::path(fig_dir, "8_swing_sources.png"), p8, width = 8, height = 5.5)

  # 9. Median swing by reporting speed and by how far apart the methods' margins were
  grid_data <- multi |> filter(is.finite(hours_to_full), is.finite(mode_gap))
  # thirds need spread on both axes (PA's precincts nearly all share one speed)
  if (nrow(grid_data) < 30 || n_distinct(grid_data$hours_to_full) < 3 || n_distinct(grid_data$mode_gap) < 3) {
    message("Wrote 8 figures to ", fig_dir, " (too little spread in speed or method gap for figure 9)")
    return(invisible(fig_dir))
  }
  # days only when most precincts took that long (CA's canvass), not because of one straggler
  speed_in_days <- quantile(grid_data$hours_to_full, 2/3) > 72
  speed <- if (speed_in_days) grid_data$hours_to_full / 24 else grid_data$hours_to_full
  speed_unit <- if (speed_in_days) "days" else "hours"
  thirds <- \(v, unit, fmt = "%.1f") {
    cuts <- unique(quantile(v, c(0, 1/3, 2/3, 1)))
    labels <- sprintf(paste0(fmt, "–", fmt, " %s"), head(cuts, -1), cuts[-1], unit)
    cut(v, cuts, labels = labels, include.lowest = TRUE)
  }
  cells <- grid_data |>
    mutate(speed_third = thirds(speed, speed_unit), gap_third = thirds(mode_gap, "pp", "%.0f")) |>
    summarise(median_swing = median(swing_max), precincts = n(), .by = c(speed_third, gap_third)) |>
    filter(!is.na(speed_third), !is.na(gap_third))
  dark <- median(range(cells$median_swing))

  p9 <- ggplot(cells, aes(speed_third, gap_third, fill = median_swing)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = sprintf("%.1f pp\n%s precincts", median_swing, comma(precincts)),
                  color = median_swing > dark),
              family = "StyreneB-Regular", size = 3.3, lineheight = 0.9, show.legend = FALSE) +
    scale_fill_medsl_blue_c(name = "Median largest\ninterim swing (pp)") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "#0B2E4F")) +
    labs(
      title = "Median Precinct Swing by Reporting Speed and Vote Method Gap",
      subtitle = paste0(scope, ", ", comma(nrow(grid_data)), " precincts in thirds on each axis"),
      x = paste0("Time from first results to 99% of precinct counted (", speed_unit, ")"),
      y = "Gap between vote methods' final margins",
      caption = caption,
      tag = join_notes("Swing: largest gap between an interim and the final precinct margin.",
                       small_note, simulated_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(panel.grid = element_blank(), legend.position = "right")

  ggsave_medsl(fs::path(fig_dir, "9_swing_speed_vs_mode.png"), p9, width = 8.5, height = 5.5)

  message("Wrote 9 figures to ", fig_dir)
  invisible(fig_dir)
}
