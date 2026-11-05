# Vote count updates from the AP feed, one figure per state: one lane for each of the state's
# U.S. Senate and governor races (race_timeline_offices below), a dot each time AP's statewide
# count changed, sized by votes added and colored by their margin
source("setup.R")
library(ggplot2)
library(scales)
library(ggmedsl)

medsl_fonts(dpi = 300)

# The races drawn, by AP office code: each state's U.S. Senate and governor races, special
# Senate elections included. U.S. House races, at-large seats included, are left out.
race_timeline_offices <- c("sen", "gov")

# "U.S. Senate", "U.S. Senate (special)": AP's race names without the Senate class
timeline_race_label <- function(race_name, special) {
  name <- trimws(gsub(" Class [IV]+| Unexpired Term|,? Special", "", race_name))
  dplyr::if_else(special, paste(name, "(special)"), name)
}

# updates: ap_race_updates() output for one state
plot_race_timeline <- function(updates, scope, out_file) {
  label_margin <- \(v) dplyr::case_when(
    is.na(v) ~ NA_character_,
    round(v, 1) == 0 ~ "Even",
    v > 0 ~ paste0("D +", abs(v)),
    TRUE ~ paste0("R +", abs(v))
  )

  # Hourly ticks for one night; daily ticks if counting runs on for days
  span_hours <- as.numeric(difftime(max(updates$time), min(updates$time), units = "hours"))
  time_scale <- if (span_hours <= 36) {
    scale_x_datetime(
      date_breaks = if (span_hours <= 12) "1 hour" else "3 hours", timezone = TZ,
      labels = \(t) sub("^0", "", format(t, "%I %p", tz = TZ))
    )
  } else {
    scale_x_datetime(date_breaks = "1 day", timezone = TZ, labels = \(t) sub(" 0", " ", format(t, "%b %d", tz = TZ)))
  }

  # lanes in the order races first reported, earliest at the top
  updates <- updates |>
    dplyr::mutate(lane = timeline_race_label(race_name, special)) |>
    dplyr::mutate(first = min(time), .by = lane) |>
    dplyr::mutate(lane = forcats::fct_reorder(lane, first, .desc = TRUE))
  no_margin <- any(is.na(updates$update_margin[updates$votes_added > 0]))

  p <- ggplot(updates, aes(time, lane)) +
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
    time_scale +
    labs(
      title    = "Vote Count Updates by Race Over Election Night",
      subtitle = scope,
      x        = "Time (ET)", y = NULL,
      caption  = medsl_caption(source = "Associated Press, via enightdata"),
      tag      = if (no_margin) "Gray: race without both a Democrat and a Republican."
    ) +
    theme_medsl() +
    theme(
      legend.position = "right",
      legend.box.spacing = unit(2, "lines"),                       # legend further from the chart
      legend.title = element_text(margin = margin(b = 8)),         # title clear of the color bar
      plot.tag.position = c(0.99, 0.02),
      plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0)
    ) +
    guides(fill = guide_colorbar(order = 1),
           size = guide_legend(order = 2, override.aes = list(fill = "#666666")),
           shape = guide_legend(order = 3))

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  ggsave_medsl(out_file, p, width = 9, height = max(4, 1.5 + 0.35 * nlevels(updates$lane)))
}

# Reads every AP snapshot so far once, then writes one figure per state with a statewide race,
# outputs/race_timeline/{st}/{st}_race_timeline_{timestamp}.png, where the timestamp is the
# latest AP pull's time in ET. A state with no votes yet is skipped.
race_timeline <- function(offices = race_timeline_offices, dir = ap_dir(), election_date = ELECTION_DATE,
                          fig_root = file.path(OUTPUT_DIR, "race_timeline")) {
  merged <- read_ap_snapshots(dir, offices, election_date)
  timestamp <- format(max(merged$time), "%Y_%m_%d_%H_%M_%S", tz = TZ)

  for (st in sort(unique(merged$state))) {
    tryCatch({
      state_merged <- dplyr::filter(merged, state == st)
      if (!any(state_merged$votes > 0, na.rm = TRUE)) {
        cli::cli_alert_info("{st}: no votes reported yet.")
      } else {
        state_name <- state.name[match(st, state.abb)]
        if (is.na(state_name)) state_name <- st
        plot_race_timeline(ap_race_updates(state_merged), paste(state_name, "statewide races"),
                           file.path(fig_root, st, paste0(st, "_race_timeline_", timestamp, ".png")))
      }
    }, error = function(e) cli::cli_alert_danger("{st} race timeline: {conditionMessage(e)}"))
  }
}

tryCatch(race_timeline(), error = function(e) cli::cli_alert_danger("race_timeline: {conditionMessage(e)}"))
