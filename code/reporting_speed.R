# Reporting speed for each state's top contest, from the AP snapshots, as two figures: cumulative
# votes reported, and the share of votes reported (AP's percent reporting), each by hour since the
# state's polls closed. A state's top contest is whichever of its Senate and gubernatorial contests
# has the most votes. With the argument "house", the same two figures for every U.S. House district.
#   Rscript code/reporting_speed.R          # each state's top statewide contest
#   Rscript code/reporting_speed.R house    # every U.S. House district
library(ggplot2)
library(scales)
library(ggmedsl)

medsl_fonts(dpi = 300)

source("setup.R")

contest_labels <- c(sen = "Senate", gov = "Gubernatorial")
house <- identical(commandArgs(trailingOnly = TRUE)[1], "house")

# "Nov 3, 9:21 PM ET". Day and hour have no leading zero; %-d and %-I are not portable, so build them.
latest_et <- function(x) {
  paste0(
    format(x, "%b ", tz = "America/New_York"), as.integer(format(x, "%d", tz = "America/New_York")), ", ",
    as.integer(format(x, "%I", tz = "America/New_York")), format(x, ":%M %p ET", tz = "America/New_York")
  )
}

# Hour ticks spaced to the span shown, so a count that runs for days doesn't crowd the axis
hour_step <- function(last_hour) {
  if (last_hour > 48) 6 else if (last_hour > 24) 4 else if (last_hour > 12) 2 else 1
}

# d: one row per snapshot, columns hour (hours since the state's polls closed), votes (cumulative), time
# (the snapshot's time, for the note with the latest total)
# place: what the title names, the state or a district ("Georgia District 6")
plot_reporting_speed <- function(d, state_name, contest_label, out_file, place = state_name) {
  last_hour <- max(4, ceiling(max(d$hour)))

  p <- ggplot(d, aes(hour, votes)) +
    geom_line(color = unname(medsl_colors["navy"]), linewidth = 0.9) +
    geom_point(data = d[d$hour == max(d$hour), ], color = unname(medsl_colors["navy"]), size = 2.2) +
    scale_x_continuous(
      breaks = seq(0, last_hour, by = hour_step(last_hour)), limits = c(0, last_hour),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0, NA)) +
    labs(
      title    = paste("Total Votes Reported in", place),
      subtitle = paste(contest_label, "Contest Votes"),
      x        = paste("Hours since polls closed in", state_name),
      y        = "Total votes reported",
      caption  = "Source: Associated Press\nGraph: MIT Election Data + Science Lab",
      tag      = if (!is.na(d$pct[which.max(d$hour)])) {
        paste0(round(d$pct[which.max(d$hour)]), "% of votes counted as of ", latest_et(d$time[which.max(d$hour)]))
      }
    ) +
    theme_medsl() +
    theme(
      plot.tag.position = c(0.99, 0.02),
      plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0)
    )

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  ggsave_medsl(out_file, p, width = 8, height = 5, dpi = 300)
}

# d: one row per snapshot, columns hour (hours since the state's polls closed), pct (AP's percent reporting)
plot_reporting_pct <- function(d, state_name, contest_label, out_file, place = state_name) {
  last_hour <- max(4, ceiling(max(d$hour)))

  p <- ggplot(d, aes(hour, pct)) +
    geom_line(color = unname(medsl_colors["navy"]), linewidth = 0.9) +
    geom_point(data = d[d$hour == max(d$hour), ], color = unname(medsl_colors["navy"]), size = 2.2) +
    scale_x_continuous(
      breaks = seq(0, last_hour, by = hour_step(last_hour)), limits = c(0, last_hour),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), labels = label_number(suffix = "%"), limits = c(0, 100)) +
    labs(
      title    = paste("Share of Votes Reported in", place),
      subtitle = paste(contest_label, "Contest Share of Votes"),
      x        = paste("Hours since polls closed in", state_name),
      y        = "Share of votes reported",
      caption  = "Source: Associated Press\nGraph: MIT Election Data + Science Lab"
    ) +
    theme_medsl()

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  ggsave_medsl(out_file, p, width = 8, height = 5, dpi = 300)
}

# d: every Senate and gubernatorial row for one state, from read_ap_votes(). Returns the top contest
# (the one with the most votes at the latest snapshot) and its votes and AP's percent reporting by
# snapshot, or NULL when no votes are in yet.
reporting_speed_data <- function(d, close_time) {
  votes <- dplyr::summarise(d, votes = sum(votes), pct = dplyr::first(pct_reporting), .by = c(time, office))

  latest <- votes[votes$time == max(votes$time), ]
  top <- latest$office[which.max(latest$votes)]
  votes <- votes[votes$office == top, ]

  votes$hour <- as.numeric(difftime(votes$time, close_time, units = "hours"))
  votes <- votes[votes$hour >= 0, ]
  if (nrow(votes) == 0 || max(votes$votes) == 0) return(NULL)

  list(data = dplyr::arrange(votes[, c("hour", "votes", "pct", "time")], hour), contest = top)
}

reporting_speed <- function(d, state_code, poll_close, timestamp) {
  state_name <- state.name[match(state_code, state.abb)]
  if (is.na(state_name)) state_name <- state_code

  close_time <- poll_close$time[match(state_code, poll_close$state)]
  if (is.na(close_time)) cli::cli_abort("No poll close time for {state_code} in the poll close file in {CONFIG_DIR}.")

  res <- reporting_speed_data(d[d$state == state_code & d$office %in% names(contest_labels), ], close_time)
  if (is.null(res)) return(FALSE)

  out_file <- file.path(
    OUTPUT_DIR, "reporting_speed", state_code, res$contest,
    paste0(state_code, "_", res$contest, "_reporting_speed_", timestamp, ".png")
  )
  plot_reporting_speed(res$data, state_name, contest_labels[[res$contest]], out_file)

  # the second figure: share of votes reported, from AP's percent reporting (skipped if AP gave none)
  if (any(!is.na(res$data$pct))) {
    pct_file <- file.path(
      OUTPUT_DIR, "reporting_speed", state_code, res$contest,
      paste0(state_code, "_", res$contest, "_reporting_speed_pct_", timestamp, ".png")
    )
    plot_reporting_pct(res$data[!is.na(res$data$pct), ], state_name, contest_labels[[res$contest]], pct_file)
  }
  TRUE
}

# The same two figures for one U.S. House district
reporting_speed_house <- function(d, state_code, district, poll_close, timestamp) {
  state_name <- state.name[match(state_code, state.abb)]
  if (is.na(state_name)) state_name <- state_code

  close_time <- poll_close$time[match(state_code, poll_close$state)]
  if (is.na(close_time)) cli::cli_abort("No poll close time for {state_code} in the poll close file in {CONFIG_DIR}.")

  res <- reporting_speed_data(d[d$state == state_code & d$district %in% district, ], close_time)
  if (is.null(res)) return(FALSE)

  label <- paste("U.S. House District", district)
  stem <- file.path(OUTPUT_DIR, "reporting_speed", state_code, "house", paste0(state_code, "_house_", district))
  place <- paste(state_name, "District", district)
  plot_reporting_speed(res$data, state_name, label, paste0(stem, "_reporting_speed_", timestamp, ".png"), place)
  if (any(!is.na(res$data$pct))) {
    plot_reporting_pct(res$data[!is.na(res$data$pct), ], state_name, label,
                       paste0(stem, "_reporting_speed_pct_", timestamp, ".png"), place)
  }
  TRUE
}

# Statewide: one pair of figures per state with a Senate or gubernatorial contest in the AP
# snapshots, written to outputs/reporting_speed/{ST}/{contest}/.
# House: one pair per district, written to outputs/reporting_speed/{ST}/house/, named
# {ST}_house_{district}_reporting_speed[_pct]_{timestamp}.png.
d <- if (house) read_ap_votes(offices = "house", districts = TRUE) else read_ap_votes()
poll_close <- read_poll_close()
timestamp <- format(max(d$time), "%Y_%m_%d_%H_%M_%S", tz = "America/New_York")  # latest snapshot, ET

if (house) {
  races <- dplyr::distinct(d, state, district) |> dplyr::arrange(state, district)
  drawn <- 0
  for (i in seq_len(nrow(races))) {
    st <- races$state[i]
    dist <- races$district[i]
    ok <- tryCatch(reporting_speed_house(d, st, dist, poll_close, timestamp), error = function(e) {
      cli::cli_alert_danger("reporting speed {st}-{dist}: {conditionMessage(e)}")
      NA
    })
    if (isTRUE(ok)) drawn <- drawn + 1
  }
  cli::cli_alert_info("Reporting speed: drew {drawn} of {nrow(races)} House districts (the rest have no votes reported yet).")
} else {
  states <- sort(unique(d$state[d$office %in% names(contest_labels)]))
  drawn <- 0
  for (st in states) {
    ok <- tryCatch(reporting_speed(d, st, poll_close, timestamp), error = function(e) {
      cli::cli_alert_danger("reporting speed {st}: {conditionMessage(e)}")
      NA
    })
    if (isTRUE(ok)) drawn <- drawn + 1
  }
  cli::cli_alert_info("Reporting speed: drew {drawn} of {length(states)} states (the rest have no votes reported yet).")
}
