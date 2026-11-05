# Reporting speed for each state's top contest, from the AP snapshots, as two figures: cumulative
# votes reported, and the share of votes reported (AP's percent reporting), each by hour since the
# state's polls closed. A state's top contest is whichever of its Senate and gubernatorial contests
# has the most votes.
library(ggplot2)
library(scales)
library(ggmedsl)

medsl_fonts(dpi = 300)

source("setup.R")

contest_labels <- c(sen = "Senate", gov = "Gubernatorial")

# "Nov 3, 9:21 PM ET". Day and hour have no leading zero; %-d and %-I are not portable, so build them.
latest_et <- function(x) {
  paste0(
    format(x, "%b ", tz = "America/New_York"), as.integer(format(x, "%d", tz = "America/New_York")), ", ",
    as.integer(format(x, "%I", tz = "America/New_York")), format(x, ":%M %p ET", tz = "America/New_York")
  )
}

# d: one row per snapshot, columns hour (hours since the state's polls closed), votes (cumulative), time
# (the snapshot's time, for the note with the latest total)
plot_reporting_speed <- function(d, state_name, contest_label, out_file) {
  last_hour <- max(4, ceiling(max(d$hour)))

  p <- ggplot(d, aes(hour, votes)) +
    geom_line(color = unname(medsl_colors["navy"]), linewidth = 0.9) +
    geom_point(data = d[d$hour == max(d$hour), ], color = unname(medsl_colors["navy"]), size = 2.2) +
    scale_x_continuous(
      breaks = seq(0, last_hour, by = if (last_hour > 12) 2 else 1), limits = c(0, last_hour),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0, NA)) +
    labs(
      title    = paste("Total Votes Reported in", state_name),
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
plot_reporting_pct <- function(d, state_name, contest_label, out_file) {
  last_hour <- max(4, ceiling(max(d$hour)))

  p <- ggplot(d, aes(hour, pct)) +
    geom_line(color = unname(medsl_colors["navy"]), linewidth = 0.9) +
    geom_point(data = d[d$hour == max(d$hour), ], color = unname(medsl_colors["navy"]), size = 2.2) +
    scale_x_continuous(
      breaks = seq(0, last_hour, by = if (last_hour > 12) 2 else 1), limits = c(0, last_hour),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), labels = label_number(suffix = "%"), limits = c(0, 100)) +
    labs(
      title    = paste("Share of Votes Reported in", state_name),
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

# One figure per state with a Senate or gubernatorial contest in the AP snapshots, written to
# outputs/reporting_speed/{ST}/{contest}/.   Rscript code/reporting_speed.R
d <- read_ap_votes()
poll_close <- read_poll_close()
timestamp <- format(max(d$time), "%Y_%m_%d_%H_%M_%S", tz = "America/New_York")  # latest snapshot, ET

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
