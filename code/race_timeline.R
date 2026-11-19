# Vote count updates from the AP feed, one figure per state: one lane for each of the state's
# U.S. Senate and governor races (race_timeline_offices below), or with the argument "house" one
# lane per U.S. House district, and along each lane one dot per interval of hours after polls
# close (timeline_bins), sized by votes added and colored by their margin.
#   Rscript code/race_timeline.R          # Senate and governor
#   Rscript code/race_timeline.R house    # U.S. House, one lane per district
#
# Options narrow the figures to some states, races and a period of the night:
#   --state GA,PA           states by postal code (default: every state with a race)
#   --race gov              AP office codes, comma-separated: sen, gov (default sen,gov), or house
#                           for every district, house_7 for one (house_7,house_8 for several).
#                           Statewide and House races can't be mixed in one run.
#   --start, --end          the period drawn: a time in ET ("2026-11-04 01:30"; a bare "21:00" is
#                           on election day), or a number of hours after the state's polls
#                           closed (0, 2.5, -1).
#                           Default: the whole night, from the first update to the latest pull.
#   Rscript code/race_timeline.R --state GA --race sen --start 19:00 --end "2026-11-04 01:00"
#   Rscript code/race_timeline.R --race house_7,house_8 --state VA --start 0 --end 4
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

# The command-line options above as a list (state, race, start, end; NULL when not given). A bare
# first argument "house" is the same as --race house.
timeline_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) && identical(args[1], "house")) args <- c("--race", "house", args[-1])
  # --start=21:00 as well as --start 21:00
  args <- unlist(lapply(args, \(a) if (grepl("^--[a-z]+=", a)) c(sub("=.*", "", a), sub("^[^=]*=", "", a)) else a))
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    key <- sub("^--", "", args[i])
    if (!startsWith(args[i], "--") || !key %in% c("state", "race", "start", "end") || i == length(args)) {
      cli::cli_abort("Unrecognized argument {.val {args[i]}}; use --state, --race, --start or --end, each with a value.")
    }
    opts[[key]] <- args[i + 1]
    i <- i + 2
  }
  split <- \(x) if (is.null(x)) NULL else trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  list(state = toupper(split(opts$state)), race = tolower(split(opts$race)), start = opts$start, end = opts$end)
}

# --race codes as offices and House districts: list(offices, districts, house). districts is
# NULL for every district.
timeline_races <- function(race) {
  if (is.null(race)) return(list(offices = race_timeline_offices, districts = NULL, house = FALSE))
  m <- stringr::str_match(race, "^house(?:_(\\d+))?$")
  is_house <- !is.na(m[, 1])
  if (any(is_house) && !all(is_house)) cli::cli_abort("--race can't mix House and statewide races: {.val {race}}.")
  if (!any(is_house)) return(list(offices = race, districts = NULL, house = FALSE))
  districts <- if (any(is.na(m[, 2]))) NULL else as.integer(m[, 2])
  list(offices = "house", districts = districts, house = TRUE)
}

# One end of the drawn period as a time (ET): a number is hours after close_time, "HH:MM" a time
# on election day, anything else a date and time. NULL stays NULL (no limit).
timeline_bound <- function(x, close_time, election_date = ELECTION_DATE) {
  if (is.null(x)) return(NULL)
  if (grepl("^-?[0-9.]+$", x)) return(close_time + as.numeric(x) * 3600)
  if (grepl("^\\d{1,2}:\\d{2}$", x)) x <- paste(election_date, x)
  t <- suppressWarnings(lubridate::ymd_hms(x, tz = TZ, truncated = 1, quiet = TRUE))
  if (is.na(t)) cli::cli_abort("Can't read {.val {x}} as a time; use \"2026-11-03 21:00\", \"21:00\" or hours after polls close.")
  t
}

# "7 PM", "Nov 4 1:30 AM": a period's ends for the subtitle, the date only when not election day
timeline_clock <- function(t, election_date = ELECTION_DATE) {
  clock <- sub("^0", "", format(t, "%I:%M %p", tz = TZ)) |> sub(pattern = ":00", replacement = "")
  day <- as.Date(format(t, "%Y-%m-%d", tz = TZ))
  if (day == as.Date(election_date)) clock else paste(format(t, "%b", tz = TZ), as.integer(format(t, "%d", tz = TZ)), clock)
}

# updates: ap_race_updates() output for one state; close_time: when its polls closed (ET).
# by_district: lanes are U.S. House districts ("District 7"), in district order.
# window: the period drawn, c(start, end) in hours after polls close (-Inf/Inf for no limit);
# the x axis keeps only the intervals that overlap it.
plot_race_timeline <- function(updates, close_time, scope, out_file, by_district = FALSE,
                               window = c(-Inf, Inf)) {
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
  keep_bins <- timeline_bins[-1] >= window[1] & head(timeline_bins, -1) < window[2]
  updates$bin <- factor(updates$bin, levels = timeline_bin_labels[keep_bins])
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
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),   # up to 17 interval labels
      plot.tag.position = c(0.99, 0.02),
      plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0)
    ) +
    guides(fill = guide_colorbar(order = 1),
           size = guide_legend(order = 2, override.aes = list(fill = "#666666")),
           shape = guide_legend(order = 3))

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  ggsave_medsl(out_file, p, width = 10, height = max(4.5, 2 + 0.35 * nlevels(updates$lane)))
  cli::cli_alert_success("Wrote {.path {out_file}}")
}

# Reads every AP snapshot so far once, then writes one figure per state with a race drawn,
# outputs/race_timeline/{st}/{st}_race_timeline_{timestamp}.png (House:
# {st}_house_race_timeline_{timestamp}.png), where the timestamp is the latest AP pull's time in
# ET. With a start or end, the file is {st}_race_timeline_{start}_to_{end}.png instead, those
# being the period's ends in ET (or the first update and latest pull where not given). A state
# with no votes in the period is skipped.
#   states: postal codes (NULL: every state); races: as for --race (NULL: sen and gov);
#   start, end: as for --start and --end (NULL: no limit)
race_timeline <- function(states = NULL, races = NULL, start = NULL, end = NULL, dir = ap_dir(),
                          election_date = ELECTION_DATE, fig_root = file.path(OUTPUT_DIR, "race_timeline"),
                          poll_close = read_poll_close()) {
  sel <- timeline_races(races)
  # Bad times fail here, before the slow read of every snapshot. Clock times can be compared
  # now; hours after polls close only once each state's close is known (below).
  probe <- as.POSIXct(ELECTION_DATE, tz = TZ)
  from0 <- timeline_bound(start, probe, election_date)
  to0 <- timeline_bound(end, probe, election_date)
  same_kind <- !is.null(start) && !is.null(end) && grepl("^-?[0-9.]+$", start) == grepl("^-?[0-9.]+$", end)
  if (same_kind && from0 >= to0) {
    cli::cli_abort("--start ({.val {start}}) must come before --end ({.val {end}}). Times after midnight need the next day's date, e.g. \"2026-11-04 03:00\".")
  }
  house <- sel$house
  cli::cli_alert_info("Reading every AP snapshot in {.path {dir}}; this takes a few minutes.")
  merged <- read_ap_snapshots(dir, sel$offices, election_date, districts = house)
  if (!is.null(sel$districts)) merged <- dplyr::filter(merged, district %in% sel$districts)
  if (!is.null(states)) {
    missing <- setdiff(states, merged$state)
    if (length(missing)) cli::cli_alert_warning("No {paste(sel$offices, collapse = '/')} races in the AP feed for {missing}.")
    merged <- dplyr::filter(merged, state %in% states)
  }
  if (nrow(merged) == 0) cli::cli_abort("No races left to draw.")
  latest <- max(merged$time)
  windowed <- !is.null(start) || !is.null(end)
  stamp <- \(t) format(t, "%Y_%m_%d_%H_%M", tz = TZ)
  what <- if (!house) {
    "statewide races"
  } else if (is.null(sel$districts)) {
    "U.S. House races"
  } else {
    paste("U.S. House", if (length(sel$districts) > 1) "districts" else "district", paste(sort(sel$districts), collapse = ", "))
  }

  for (st in sort(unique(merged$state))) {
    tryCatch({
      state_merged <- dplyr::filter(merged, state == st)
      close_time <- poll_close$time[poll_close$state == st]
      if (length(close_time) != 1) cli::cli_abort("no poll close time for {st} in helper_files")
      # Updates are found over the whole night, then cut to the period, so the first update in
      # it counts only the votes added since the pull before
      from <- timeline_bound(start, close_time, election_date)
      to <- timeline_bound(end, close_time, election_date)
      if (!is.null(from) && !is.null(to) && from >= to) cli::cli_abort("--start must come before --end.")
      updates <- ap_race_updates(state_merged)
      if (!is.null(from)) updates <- dplyr::filter(updates, time >= from)
      if (!is.null(to)) updates <- dplyr::filter(updates, time <= to)
      if (!any(updates$votes_added > 0 | updates$votes_removed > 0)) {
        cli::cli_alert_info("{st}: no votes reported {if (windowed) 'in that period' else 'yet'}.")
      } else {
        state_name <- state.name[match(st, state.abb)]
        if (is.na(state_name)) state_name <- st
        hours <- \(t, default) if (is.null(t)) default else as.numeric(difftime(t, close_time, units = "hours"))
        scope <- paste(state_name, what)
        file_time <- format(latest, "%Y_%m_%d_%H_%M_%S", tz = TZ)   # the latest pull, as before
        if (windowed) {
          shown <- c(from %||% min(updates$time), to %||% latest)
          scope <- paste0(scope, ", ", timeline_clock(shown[1], election_date), "\u2013",
                          timeline_clock(shown[2], election_date), " ET")
          file_time <- paste0(stamp(shown[1]), "_to_", stamp(shown[2]))
        }
        plot_race_timeline(updates, close_time, scope,
                           file.path(fig_root, st, paste0(st, if (house) "_house", "_race_timeline_", file_time, ".png")),
                           by_district = house, window = c(hours(from, -Inf), hours(to, Inf)))
      }
    }, error = function(e) cli::cli_alert_danger("{st} race timeline: {conditionMessage(e)}"))
  }
}

tryCatch({
  a <- timeline_args()
  race_timeline(states = a$state, races = a$race, start = a$start, end = a$end)
}, error = function(e) cli::cli_alert_danger("race_timeline: {conditionMessage(e)}"))
