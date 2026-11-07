# Votes reported by vote method over the night: for each row of COUNTY_REPORTING (config.R),
# one graph per method, by hours since the state's polls closed: the county in color with a dot
# at each of its reports labeled with the time, and each comparison county as a thin gray line.
# Each county's lines end at its last report that added votes.
#   - district NA: each county's cumulative votes, against counties of similar size by votes
#     counted so far (county_compare_n, county_size_band below), with an All methods graph that
#     includes counties reporting only totals
#   - district set: each county's cumulative votes in its part of that district, for the county
#     and the district's other counties
#
# Every pass reads each of the state's {ST}/clean/{ST}_{county}_latest.csv files (and any
# timestamped snapshots beside them) and adds snapshots it hasn't seen to a per-county history in
# outputs/county_reporting/history/, so the lines build up over the night even from the latest files alone.
source("setup.R")
library(ggplot2)
library(scales)
library(ggmedsl)

medsl_fonts(dpi = 300)

# Define states to run for.
# District NA: the county against similar-size counties in the state
# Drict set: the county's precincts in that district against the district's other precincts
COUNTY_REPORTING = tibble::tribble(
  ~state, ~county, ~contest, ~district,
  "NY", "westchester", "gov", NA,
  "NY", "westchester", "house", 17,
)

# Methods as in reporting_speed.R: absentee counts as mail, and "all" (a county that doesn't
# break out methods) can't be compared by method, so it is left out
county_method_labels <- c(eday = "Election day", early = "Early", mail = "Mail", abs = "Mail", prov = "Provisional")
county_method_levels <- c("Election day", "Early", "Mail", "Provisional", "All methods")
# every row of a county, "all" included, for the All methods graph
county_total_labels <- c(eday = "All methods", early = "All methods", mail = "All methods", abs = "All methods",
                         prov = "All methods", all = "All methods")
county_focal_color <- medsl_colors[["purple"]]
county_other_color <- "#BDBDBD"
county_contest_labels <- c(gov = "gubernatorial", sen = "U.S. Senate", house = "U.S. House")
# Comparison counties for the district-NA figure, by votes counted so far in the contest (every
# method, the latest snapshot of each county), so the set can change over the night: the method graphs show
# the county_compare_n counties closest in size that break out methods; the All methods graph adds
# every county within county_size_band of the county's size, which may report only totals
county_compare_n <- 4
county_size_band <- c(0.5, 2)

# The state's clean/ folder: the MEDSL server's (COUNTY_CLEAN_PATH, e.g.
# /d/research/medsl-enr/26_general/NY/clean) if it has any county latest files, else the CBS
# copy, else the MEDSL backup
county_data_dir <- function(state) {
  roots <- c(CBS_DATA_PATH, MEDSL_DATA_PATH)
  dirs <- c(gsub("{ST}", state, COUNTY_CLEAN_PATH, fixed = TRUE),
            file.path(roots[nzchar(roots)], ELECTION_FOLDER, state, "clean"))
  for (dir in dirs) if (length(county_feeds(dir, state)) > 0) return(dir)
  cli::cli_abort("No {state} county latest files in {paste(dirs, collapse = ' or ')}.")
}

#' Counties with a latest file in a clean/ folder, e.g. "westchester"
county_feeds <- function(dir, state) {
  f <- list.files(dir, paste0("^", state, "_[a-z_]+_latest\\.csv$"))
  sub(paste0("^", state, "_(.+)_latest\\.csv$"), "\\1", f)
}

# Adds a county's latest file (and any timestamped snapshots) to its history; returns the
# history, one row per snapshot time x precinct x contest x district x method with total votes
update_county_history <- function(state, county, dir, history_dir) {
  stem <- paste0(state, "_", county, "_")
  history_file <- file.path(history_dir, paste0(stem, "history.parquet"))
  latest <- tryCatch(
    readr::read_csv(file.path(dir, paste0(stem, "latest.csv")), show_col_types = FALSE),
    # a synced file can be mid-overwrite; the next pass picks it up
    error = \(e) { cli::cli_alert_warning("{stem}latest.csv unreadable this pass: {conditionMessage(e)}"); NULL }
  )
  stamped <- list.files(dir, paste0("^", stem, "\\d{4}(_\\d{2}){5}\\.parquet$"), full.names = TRUE)
  history <- if (file.exists(history_file)) nanoparquet::read_parquet(history_file) else NULL
  seen <- if (is.null(history)) NULL else unique(history$time)

  new <- c(list(latest), purrr::map(stamped, nanoparquet::read_parquet)) |>
    purrr::compact() |>
    purrr::map(\(s) s |>
      dplyr::filter(!time %in% seen) |>
      dplyr::mutate(district = as.integer(district)) |>
      dplyr::summarise(total = sum(votes), .by = c(time, geo, contest, district, method))) |>
    purrr::list_rbind() |>
    dplyr::distinct(time, geo, contest, district, method, .keep_all = TRUE)
  history <- dplyr::bind_rows(history, new)

  dir.create(history_dir, recursive = TRUE, showWarnings = FALSE)
  nanoparquet::write_parquet(history, history_file)
  dplyr::mutate(history, time = lubridate::with_tz(time, TZ), county = county)
}

# Cumulative votes by method for each unit (a county or a precinct) at every time in `grid`,
# carrying each unit's last snapshot forward until it reports again (0 before its first snapshot)
carry_forward <- function(h, grid, unit, labels = county_method_labels) {
  h |>
    dplyr::mutate(label = labels[as.character(method)]) |>
    dplyr::filter(!is.na(label)) |>
    dplyr::summarise(total = sum(total), .by = c(county, dplyr::all_of(unit), label, time)) |>
    dplyr::arrange(time) |>
    # votes before time: reframe() evaluates in order, so `time` must still be the unit's
    # snapshot times when findInterval() looks them up
    dplyr::reframe(
      votes = { i <- findInterval(grid, time); ifelse(i == 0, 0, total[pmax(i, 1)]) },
      time = grid,
      .by = c(county, dplyr::all_of(unit), label)
    )
}

# One line per county x method x time: the county's total (summed over its precincts),
# ending at the county's last report that added votes. Only methods the focal county reports and
# someone has votes in are kept.
reporting_lines <- function(cum, focal_county) {
  lines <- cum |>
    dplyr::summarise(value = sum(votes), .by = c(county, label, time))
  last_report <- lines |>
    dplyr::arrange(time) |>
    dplyr::filter(value != dplyr::lag(value), .by = c(county, label)) |>
    dplyr::summarise(last = max(time), .by = county)
  lines |>
    dplyr::left_join(last_report, by = "county") |>
    dplyr::filter(is.na(last) | time <= last) |>
    dplyr::select(-last) |>
    dplyr::filter(label %in% label[county == focal_county],
                  label %in% unique(cum$label[cum$votes > 0])) |>
    dplyr::mutate(focal = county == focal_county)
}

# Method titles for each graph's title, and the suffix of its file name
county_method_titles <- c(`Election day` = "Election Day", Early = "Early", Mail = "Mail",
                          Provisional = "Provisional", `All methods` = "All Methods")
county_method_files <- c(`Election day` = "election_day", Early = "early", Mail = "mail",
                         Provisional = "provisional", `All methods` = "all_methods")

# lines: reporting_lines() output. close_time: when the state's polls closed, the x axis's zero.
# other_labels: county and text, to name each gray line at its end (for a few counties); NULL leaves
# them unnamed. Draws one graph per vote method, each with its own vote scale but the same hours
# axis, to {out_stem}_{method}.png. Files from an earlier pass under the same stem are removed
# first, so a method that dropped out doesn't linger.
plot_county_reporting <- function(lines, close_time, focal_name, others_name, y_title, y_labels, scope,
                                  source, note, out_stem, other_labels = NULL) {
  dir.create(dirname(out_stem), recursive = TRUE, showWarnings = FALSE)
  old <- list.files(dirname(out_stem), paste0("^", basename(out_stem), "(_[a-z_]+)?\\.png$"), full.names = TRUE)
  file.remove(old)

  lines$hour <- as.numeric(difftime(lines$time, close_time, units = "hours"))

  # whole-hour ticks from just before the first point to the latest, plus room on the right for
  # the labels, shared by every method's graph so they line up
  span_hours <- diff(range(lines$hour))
  step <- if (span_hours <= 6) 1 else if (span_hours <= 12) 2 else if (span_hours <= 24) 3 else if (span_hours <= 72) 6 else 24
  first <- floor(min(lines$hour) / step) * step
  hours_scale <- scale_x_continuous(
    limits = c(first, max(lines$hour) + 0.25 * (max(lines$hour) - first)),
    breaks = seq(first, max(lines$hour), by = step), expand = expansion(mult = c(0.03, 0))
  )
  # dot labels give the date too once the night runs past one day
  multi_day <- format(min(lines$time), "%F", tz = TZ) != format(max(lines$time), "%F", tz = TZ)
  close_label <- sub("^0", "", format(close_time, "%I %p ET", tz = TZ))

  methods <- intersect(county_method_levels, unique(lines$label))
  for (m in methods) {
    out_file <- paste0(out_stem, "_", county_method_files[[m]], ".png")
    plot_county_method(dplyr::filter(lines, label == m), m, focal_name, others_name, y_title, y_labels,
                       scope, source, note, hours_scale, close_label, multi_day, out_file, other_labels)
  }
  invisible(paste0(out_stem, "_", county_method_files[methods], ".png"))
}

# One method's graph: the focal county in color with a dot at each report, labeled with its
# time, and the comparison counties in gray, each named at the end of its line
plot_county_method <- function(lines, method, focal_name, others_name, y_title, y_labels, scope,
                               source, note, hours_scale, close_label, multi_day, out_file,
                               other_labels = NULL) {
  focal <- dplyr::filter(lines, focal) |> dplyr::arrange(time)
  others <- dplyr::filter(lines, !focal)
  fmt_time <- \(t) {
    clock <- sub("^0", "", format(t, "%I:%M %p", tz = TZ))
    if (multi_day) paste0(sub(" 0", " ", format(t, "%b %d", tz = TZ)), ", ", clock) else clock
  }

  # the county's reports: each time its value rose, labeled with when it came in
  reports <- dplyr::filter(focal, value != dplyr::lag(value)) |>
    dplyr::mutate(text = fmt_time(time))
  # a county with nothing in this method yet says so at the end of its line
  none <- dplyr::slice_max(focal, time, n = 1, with_ties = FALSE) |>
    dplyr::filter(value == 0) |>
    dplyr::mutate(text = paste("None reported\nas of", fmt_time(time)))
  other_ends <- if (!is.null(other_labels)) {
    dplyr::slice_max(others, time, n = 1, with_ties = FALSE, by = county) |>
      dplyr::left_join(other_labels, by = "county")
  }
  end_labels <- dplyr::bind_rows(dplyr::mutate(none, color = county_focal_color),
                                 if (!is.null(other_ends)) dplyr::mutate(other_ends, color = "#888888"))
  label_font <- theme_medsl()$text$family
  series_colors <- rlang::set_names(c(county_focal_color, county_other_color), c(focal_name, others_name))
  title <- if (method == "All methods") "Cumulative Votes Reported, All Methods"
           else paste("Cumulative", county_method_titles[[method]], "Votes Reported")
  nudge <- 0.01 * diff(range(lines$hour))

  p <- ggplot(mapping = aes(hour, value)) +
    geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_step(data = others, aes(group = county, color = others_name), linewidth = 0.5) +
    geom_step(data = focal, aes(color = focal_name), linewidth = 1) +
    geom_point(data = reports, aes(color = focal_name), size = 2) +
    # each dot's time, just above and to the right of it, pushed apart where they would overlap
    # (with a white halo, so they stay legible where they cross a gray line)
    ggrepel::geom_text_repel(data = reports, aes(label = text), color = county_focal_color, size = 2.8,
                             bg.color = "white", bg.r = 0.15,
                             family = label_font, hjust = 0, vjust = 0, nudge_x = nudge,
                             nudge_y = 0.02 * max(lines$value), direction = "both", segment.color = NA,
                             box.padding = 0.2, min.segment.length = Inf, seed = 1, show.legend = FALSE,
                             max.overlaps = Inf) +
    # gray county names to the right of each line's end
    ggrepel::geom_text_repel(data = end_labels, aes(label = text, color = I(color)), size = 2.8,
                             family = label_font, hjust = 0, lineheight = 0.9, direction = "y",
                             nudge_x = nudge, segment.color = NA, box.padding = 0.25,
                             min.segment.length = Inf, seed = 1, show.legend = FALSE, force = 1.5, max.overlaps = Inf) +
    scale_color_manual(values = series_colors, breaks = names(series_colors), name = NULL) +
    # extra room above and below so stacked labels can spread out
    scale_y_continuous(labels = y_labels, limits = c(0, NA), expand = expansion(mult = 0.08)) +
    hours_scale +
    coord_cartesian(clip = "off") +
    labs(title = title, subtitle = scope, x = paste0("Hours since polls closed (", close_label, ")"),
         y = y_title, caption = medsl_caption(source = source), tag = note) +
    theme_medsl() +
    theme(
      legend.position = "top",
      axis.title.x = element_text(margin = margin(t = 6, b = 26)),   # room for a three-line note below
      plot.tag.position = c(0.99, 0.02),
      plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0, lineheight = 1.1)
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = c(1, 0.6), shape = NA)))

  ggsave_medsl(out_file, p, width = 8, height = 5.5)
}

# One row of COUNTY_REPORTING, from every county's history in the state (update_county_history());
# writes outputs/fig2_county_reporting/{ST}/{ST}_{county}_reporting_{contest}[_{district}]_{method}.png,
# replacing the previous pass's graphs, so the folder holds one current graph per row and method
county_reporting <- function(histories, state, county, contest, district = NA,
                             poll_close = read_poll_close(),
                             fig_root = file.path(OUTPUT_DIR, "fig2_county_reporting")) {
  h <- dplyr::filter(histories, .data$contest == .env$contest)
  if (!is.na(district)) h <- dplyr::filter(h, .data$district %in% .env$district)
  if (!county %in% h$county) cli::cli_abort("No {contest} {district} rows in {state}_{county}.")
  if (!any(h$total[h$county == county] > 0)) {
    cli::cli_alert_info("{state} {county} {contest}: no votes reported yet.")
    return(invisible(NULL))
  }

  # from shortly before the first votes anywhere to the latest snapshot
  times <- sort(unique(histories$time))
  grid <- times[times >= min(h$time[h$total > 0]) - 15 * 60]

  state_name <- state.name[match(state, state.abb)]
  if (is.na(state_name)) state_name <- state
  county_name <- tools::toTitleCase(gsub("_", " ", county))
  contest_label <- dplyr::coalesce(county_contest_labels[contest], contest)

  if (is.na(district)) {
    # each county's total, against counties of similar size by votes counted so far
    sizes <- h |>
      dplyr::filter(time == max(time), .by = county) |>
      dplyr::summarise(counted = sum(total), .by = county) |>
      dplyr::filter(counted > 0) |>
      dplyr::mutate(ratio = counted / counted[county == .env$county])
    by_method <- unique(h$county[as.character(h$method) %in% names(county_method_labels)])
    method_cmp <- sizes |>
      dplyr::filter(county != .env$county, county %in% by_method) |>
      dplyr::slice_min(abs(log(ratio)), n = county_compare_n, with_ties = FALSE)
    total_cmp <- dplyr::bind_rows(method_cmp, dplyr::filter(
      sizes, county != .env$county, dplyr::between(ratio, county_size_band[1], county_size_band[2]))) |>
      dplyr::distinct(county, .keep_all = TRUE)
    cum <- dplyr::bind_rows(
      carry_forward(dplyr::filter(h, county %in% c(.env$county, method_cmp$county)), grid, character()),
      carry_forward(dplyr::filter(h, county %in% c(.env$county, total_cmp$county)), grid, character(),
                    county_total_labels)
    )
    lines <- reporting_lines(cum, county)
    focal_name <- paste(county_name, "County")
    others_name <- paste("Similar-size", state_name, "counties")
    y_title <- "Cumulative votes reported"
    y_labels <- label_number(scale_cut = cut_short_scale())
    scope <- paste0(county_name, " County, ", state_name, ", ", contest_label, " contest")
    pct <- \(r) percent(r, accuracy = 1)
    added <- setdiff(total_cmp$county, method_cmp$county)
    note <- paste0("Gray: the ", nrow(method_cmp), " counties with method breakdowns nearest ", county_name,
                   "'s votes counted\nso far (size relative to it)",
                   if (length(added) > 0) paste0(". All methods adds every county ", pct(county_size_band[1]),
                                               " to ", pct(county_size_band[2]), " its size\nand"),
                   if (length(added) == 0) ". All methods",
                   " counts precincts a county doesn't break out by method. Absentee counts as mail.")
    other_labels <- dplyr::transmute(total_cmp, county,
                                     text = paste0(tools::toTitleCase(gsub("_", " ", county)), " (", pct(ratio), ")"))
  } else {
    # each county's total in its part of the district
    cum <- carry_forward(h, grid, "geo")
    lines <- reporting_lines(cum, county)
    precincts <- cum |>
      dplyr::summarise(precincts = dplyr::n_distinct(geo), .by = county) |>
      dplyr::arrange(dplyr::desc(precincts))
    others <- dplyr::filter(precincts, county != .env$county)
    focal_name <- paste(county_name, "precincts")
    others_name <- paste("Other District", district, "counties")
    y_title <- "Cumulative votes reported"
    y_labels <- label_number(scale_cut = cut_short_scale())
    scope <- paste0(comma(precincts$precincts[precincts$county == county]), " ", county_name,
                    " County precincts in ", state_name, "'s District ", district)
    other_labels <- dplyr::transmute(others, county,
                                     text = paste0(tools::toTitleCase(gsub("_", " ", county)), " (", comma(precincts), ")"))
    note <- paste0("Gray: each other county's District ", district, " precincts (number in parentheses), in the\n",
                   "graphs of the methods it reports. Totals-only counties are left out. Absentee counts as mail.")
  }

  source <- "county election websites"
  race_id <- if (is.na(district)) contest else paste(contest, district, sep = "_")
  out_stem <- file.path(fig_root, state, paste0(state, "_", county, "_reporting_", race_id))
  close_time <- poll_close$time[match(state, poll_close$state)]
  if (is.na(close_time)) cli::cli_abort("No poll close time for {state} in the poll close file in {CONFIG_DIR}.")
  plot_county_reporting(lines, close_time, focal_name, others_name, y_title, y_labels, scope, source, note,
                        out_stem, other_labels)
}

# One pass for a state: update every county's history once,
# then draw each of the state's rows of COUNTY_REPORTING
county_reporting_state <- function(state, rows, ...) {
  dir <- county_data_dir(state)
  counties <- county_feeds(dir, state)
  history_dir <- file.path(OUTPUT_DIR, "county_reporting", "history")
  histories <- purrr::map(counties, \(cty) update_county_history(state, cty, dir, history_dir)) |>
    purrr::list_rbind()

  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    tryCatch(county_reporting(histories, state, r$county, r$contest, r$district, ...),
             error = function(e) cli::cli_alert_danger("{state} {r$county} {r$contest} {r$district} reporting: {conditionMessage(e)}"))
  }
}

if (nrow(COUNTY_REPORTING) > 0) {
  for (st in unique(COUNTY_REPORTING$state)) {
    tryCatch(county_reporting_state(st, dplyr::filter(COUNTY_REPORTING, state == st)),
             error = function(e) cli::cli_alert_danger("{st} county reporting: {conditionMessage(e)}"))
  }
} else {
  cli::cli_alert_warning("COUNTY_REPORTING is empty; fig2_county_reporting_speed.R drew nothing.")
}
