# Vote count updates from the AP feed, one figure per state: one lane for each of the state's
# U.S. Senate and governor races (race_timeline_offices below), or with the argument "house" one
# lane per U.S. House district, and along each lane one dot per interval of hours after polls
# close (timeline_bins), sized by votes added and colored by their margin.
#   Rscript code/race_timeline.R          # Senate and governor
#   Rscript code/race_timeline.R house    # U.S. House, one lane per district
source("setup.R")
library(ggplot2)
library(scales)
library(ggmedsl)

medsl_fonts(dpi = 300)

# The statewide races drawn by default, by AP office code: each state's U.S. Senate and governor
# races, special Senate elections included. U.S. House races (at-large seats as district 1) are
# drawn instead with the argument "house".
race_timeline_offices <- c("sen", "gov")

# "U.S. Senate", "U.S. Senate (special)": AP's race names without the Senate class
timeline_race_label <- function(race_name, special) {
  name <- trimws(gsub(" Class [IV]+| Unexpired Term|,? Special", "", race_name))
  dplyr::if_else(special, paste(name, "(special)"), name)
}

# Intervals after polls close, in hours: quarter hours for the first 3 (when most votes come
# in), half hours to 4, then 4-6, 6-10 and everything later. Labels give each interval's range.
timeline_bins <- c(-Inf, seq(0.25, 3, by = 0.25), 3.5, 4, 6, 10, Inf)
timeline_bin_labels <- c(paste0(c(0, head(timeline_bins[-1], -2)), "\u2013", head(timeline_bins[-1], -1)),
                         paste0(timeline_bins[length(timeline_bins) - 1], "+"))

# updates: ap_race_updates() output for one state; close_time: when its polls closed (ET).
# by_district: lanes are U.S. House districts ("District 7"), in district order
plot_race_timeline <- function(updates, close_time, scope, out_file, by_district = FALSE) {
  label_margin <- \(v) dplyr::case_when(
    is.na(v) ~ NA_character_,
    round(v, 1) == 0 ~ "Even",
    v > 0 ~ paste0("D +", abs(v)),
    TRUE ~ paste0("R +", abs(v))
  )

  # Updates are grouped into intervals of hours after polls close (timeline_bins), one dot per
  # race and interval: votes added in it, and the margin among those votes
  updates <- updates |>
    dplyr::mutate(
      hours = as.numeric(difftime(time, close_time, units = "hours")),
      bin = cut(hours, breaks = timeline_bins, labels = timeline_bin_labels, right = TRUE),
      margin_votes = update_margin * votes_added / 100
    ) |>
    dplyr::summarise(
      time = min(time),
      margin_votes = sum(margin_votes),          # NA if any update lacks a two-party margin
      votes_added = sum(votes_added),
      votes_removed = sum(votes_removed),
      .by = c(race_name, special, district, bin)
    ) |>
    dplyr::mutate(update_margin = dplyr::if_else(votes_added > 0, 100 * margin_votes / votes_added, NA_real_))
  close_label <- sub("^0", "", format(close_time, "%I:%M %p ET", tz = TZ)) |> sub(pattern = ":00", replacement = "")

  # lanes in the order races first reported, earliest at the top; districts in number order
  updates <- if (by_district) {
    updates |>
      dplyr::mutate(lane = paste("District", district)) |>
      dplyr::mutate(lane = forcats::fct_reorder(lane, district, .desc = TRUE))
  } else {
    updates |>
      dplyr::mutate(lane = timeline_race_label(race_name, special)) |>
      dplyr::mutate(first = min(time), .by = lane) |>
      dplyr::mutate(lane = forcats::fct_reorder(lane, first, .desc = TRUE))
  }
  no_margin <- any(is.na(updates$update_margin[updates$votes_added > 0]))

  p <- ggplot(updates, aes(bin, lane)) +
    # white outlines keep overlapping updates apart
    geom_point(
      data = dplyr::filter(updates, votes_added > 0),
      aes(size = votes_added, fill = update_margin),
      shape = 21, color = "white", stroke = 0.4, alpha = 0.9
    ) +
    geom_point(
      data = dplyr::filter(updates, votes_removed > 0),
      aes(shape = "Votes removed"),
      color = "#444444", size = 1.6, position = position_nudge(y = -0.28)
    ) +
    # Area runs from the night's smallest update to its largest, not from zero, so updates of
    # similar size still differ visibly. Legend: four sizes from smallest to largest.
    scale_size(range = c(1, 12), labels = comma, name = "Votes added", breaks = \(l) {
      # two significant figures, rounded inward so the end sizes stay inside the data
      unit <- 10^(floor(log10(pmax(l, 1))) - 1)
      ends <- c(ceiling(l[1] / unit[1]) * unit[1], floor(l[2] / unit[2]) * unit[2])
      unique(signif(seq(ends[1], ends[2], length.out = 4), 2))
    }) +
    scale_fill_medsl_diverging(midpoint = 0, limits = c(-40, 40), oob = squish, na.value = "#C4C4C4",
                                labels = label_margin, name = "Margin of added votes") +
    scale_shape_manual(values = c("Votes removed" = 4), name = NULL) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title    = "Vote Count Updates by Race Over Election Night",
      subtitle = scope,
      x        = paste0("Hours after polls closed (", close_label, ")"), y = NULL,
      caption  = medsl_caption(source = "Associated Press"),
      tag      = paste(c("Each dot sums a race's updates within the interval.",
                             if (no_margin) "Gray: race without both a Democrat and a Republican."), collapse = "\n")
    ) +
    theme_medsl() +
    theme(
      legend.position = "right",
      legend.box.spacing = unit(2, "lines"),                       # legend further from the chart
      legend.title = element_text(margin = margin(b = 8)),         # title clear of the color bar
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),   # 17 interval labels
      plot.tag.position = c(0.99, 0.02),
      plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0)
    ) +
    guides(fill = guide_colorbar(order = 1),
           size = guide_legend(order = 2, override.aes = list(fill = "#666666")),
           shape = guide_legend(order = 3))

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  ggsave_medsl(out_file, p, width = 10, height = max(4.5, 2 + 0.35 * nlevels(updates$lane)))
}

# Reads every AP snapshot so far once, then writes one figure per state with a race drawn,
# outputs/race_timeline/{st}/{st}_race_timeline_{timestamp}.png (House:
# {st}_house_race_timeline_{timestamp}.png), where the timestamp is the latest AP pull's time in
# ET. A state with no votes yet is skipped.
race_timeline <- function(house = FALSE, dir = ap_dir(), election_date = ELECTION_DATE,
                          fig_root = file.path(OUTPUT_DIR, "race_timeline"), poll_close = read_poll_close()) {
  offices <- if (house) "house" else race_timeline_offices
  merged <- read_ap_snapshots(dir, offices, election_date, districts = house)
  timestamp <- format(max(merged$time), "%Y_%m_%d_%H_%M_%S", tz = TZ)

  for (st in sort(unique(merged$state))) {
    tryCatch({
      state_merged <- dplyr::filter(merged, state == st)
      if (!any(state_merged$votes > 0, na.rm = TRUE)) {
        cli::cli_alert_info("{st}: no votes reported yet.")
      } else {
        state_name <- state.name[match(st, state.abb)]
        if (is.na(state_name)) state_name <- st
        close_time <- poll_close$time[poll_close$state == st]
        if (length(close_time) != 1) cli::cli_abort("no poll close time for {st} in helper_files")
        plot_race_timeline(ap_race_updates(state_merged), close_time,
                           paste(state_name, if (house) "U.S. House races" else "statewide races"),
                           file.path(fig_root, st, paste0(st, if (house) "_house", "_race_timeline_", timestamp, ".png")),
                           by_district = house)
      }
    }, error = function(e) cli::cli_alert_danger("{st} race timeline: {conditionMessage(e)}"))
  }
}

tryCatch(race_timeline(house = identical(commandArgs(trailingOnly = TRUE)[1], "house")),
         error = function(e) cli::cli_alert_danger("race_timeline: {conditionMessage(e)}"))
