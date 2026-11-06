# Shared by the margin figures in code/ (fig4_running_margin.R, fig5_method_counted.R,
# fig6_method_running_margin.R, fig7_method_margin_by_county.R): colors, axes, and race_context(),
# which works out a race's labels, caption and time window once so every figure agrees.
#
# When AP carries the race, each caption gives AP's percent of votes reported as of the feed's
# last snapshot (ap_race_at() in R/ap_snapshots.R). Under 99% the "final" labels give the margin at
# that percent reporting, and figure 4's share counted is measured against AP's expected vote.

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
  abs = "Absentee", adv = "Advance", all = "Not split by method"
)

# "Nov 4 at 1:24 AM ET", as in vote_share.R. Day and hour have no leading zero; %-d and %-I
# are not portable, so build them.
as_of_et <- function(x) {
  paste0(
    format(x, "%b ", tz = "America/New_York"), as.integer(format(x, "%d", tz = "America/New_York")), " at ",
    as.integer(format(x, "%I", tz = "America/New_York")), format(x, ":%M %p ET", tz = "America/New_York")
  )
}

# Hourly ticks for one night; daily ticks when counting ran on through the canvass
time_axis <- function(start, end) {
  span_hours <- as.numeric(difftime(end, start, units = "hours"))
  if (span_hours <= 36) {
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
}

# When a state's polls closed (ET), from helper_files/{ELECTION_FOLDER}_poll_close.csv; NA when
# the file or the state's row is missing
poll_close_time <- function(state) {
  path <- file.path(CONFIG_DIR, paste0(ELECTION_FOLDER, "_poll_close.csv"))
  if (!file.exists(path)) return(as.POSIXct(NA))
  close <- read_poll_close(path)
  t <- close$time[close$state == state]
  if (length(t) == 1) t else as.POSIXct(NA)
}

# Whole-hour ticks for an axis of hours since polls closed, spaced to the span shown. The axis
# starts at polls close (0), or earlier when results came in before it (Texas's feeds post early).
hours_axis <- function(min_hours, max_hours, right_pad = 0.02) {
  step <- case_when(max_hours <= 12 ~ 1, max_hours <= 24 ~ 2, max_hours <= 48 ~ 4,
                    max_hours <= 96 ~ 6, max_hours <= 24 * 10 ~ 24, .default = 24 * 7)
  lower <- min(0, floor(min_hours / step) * step)
  # ticks stop at the last whole step with data, not in the padding after it
  scale_x_continuous(breaks = seq(lower, floor(max_hours / step) * step, by = step),
                     limits = c(lower, NA), expand = expansion(mult = c(0.01, right_pad)))
}

# Margin axes in figures 6 and 7: about ten labeled values instead of ggplot's default four or five
margin_breaks <- scales::breaks_pretty(n = 10)

note_theme <- theme(
  plot.tag.position = c(0.99, 0.02),
  plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0)
)
# one note per line, right-aligned, so long notes don't run into the source caption
join_notes <- \(...) { notes <- c(...); if (length(notes)) paste(notes, collapse = "\n") else NULL }

# Removes a figure's previous file before redrawing, so a figure that no longer applies
# (a state that stopped splitting by method) doesn't linger from the last run
fresh_figure <- function(fig_dir, file) {
  fs::dir_create(fig_dir)
  path <- fs::path(fig_dir, file)
  if (fs::file_exists(path)) fs::file_delete(path)
  path
}

# Points by which the files' final margin can differ from AP's before figures 4 and 6 draw AP's
ap_margin_tolerance <- 0.5

#' Where a time figure ends, and the note saying so
#'
#' Lines end 1 hour after the last update to the vote count, not at the feed's last snapshot
#' (feeds keep scraping long after the count stops). Trailing updates that together change
#' under `min_tail` of the final votes are left off too, since they don't show on the graph.
#' `time` and `total` are the running total at each snapshot; returns list(end, note).
vote_window <- function(time, total, cutoff, min_tail = 0.05) {
  d <- tibble::tibble(time, total) |>
    arrange(time) |>
    mutate(change = abs(total - lag(total, default = 0))) |>
    filter(change > 0)
  if (nrow(d) == 0) return(list(end = cutoff, note = NULL))
  final <- last(d$total)
  tail <- rev(cumsum(rev(d$change)))                 # votes changed at or after each update
  keep <- which(tail >= min_tail * final)
  last_kept <- if (length(keep)) max(keep) else 1
  end <- min(d$time[last_kept] + hours(1), cutoff)
  dropped <- if (last_kept < nrow(d)) tail[last_kept + 1] / final else 0
  note <- if (dropped > 0) {
    paste0("Lines end 1 hour after the last update; later updates under ", scales::percent(min_tail),
           " of votes (", scales::percent(floor(dropped * 1000) / 1000, accuracy = 0.1), " in all) not shown.")
  } else if (end < cutoff) {
    "Lines end 1 hour after the last update."
  }
  list(end = end, note = note)
}

# The subtitle names the files' coverage when they hold less than this share of AP's statewide vote.
# Files that cover every county still trail AP by about 1% (Georgia), from votes AP counted after
# the files' last snapshot, so the bar is set below that.
coverage_full_share <- 0.97

#' Subtitle for figures built from the state files: the race, plus, when the files cover only
#' some counties (Texas, Michigan), how many and what share of AP's statewide vote they hold,
#' e.g. "Texas U.S. Senate, 34 counties in state files (67% of votes)"
coverage_subtitle <- function(x, ctx) {
  coverage <- if (!is.na(ctx$ap_latest_total) && ctx$ap_latest_total > 0) last(x$race$total) / ctx$ap_latest_total
  if (is.null(coverage) || coverage >= coverage_full_share || !x$has_counties) return(ctx$scope)
  n <- nrow(x$county_votes)
  paste0(ctx$scope, ", ", n, if (n == 1) " county" else " counties", " in state files (",
         scales::percent(coverage, accuracy = 1), " of votes)")
}

#' The caption's AP line. Once AP has 99% in, it's dated to the pull where AP first reported 99%,
#' not AP's newest pull; under 99% (a feed that stopped short) it gives AP's percent as of the
#' feed's last snapshot, `cutoff`.
ap_caption_line <- function(ap, state, contest, district, cutoff) {
  if (ap$pct >= 99) {
    first <- ap_first_reached(state, contest, district, 99)
    if (!is.na(first)) return(paste0("99% of votes reported (AP) as of ", as_of_et(first)))
  }
  paste0(round(ap$pct), "% of votes reported (AP) as of ", as_of_et(cutoff))
}

#' Labels, caption and time window for one race from build_race()
race_context <- function(x) {
  caption <- medsl_caption(source = "state and county election websites")

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

  # Margin labels: "D +10" / "R +10" in a general, candidate surnames in a primary
  sides <- str_split_1(x$margin_label, "–")
  label_margin <- \(v) case_when(
    is.na(v) ~ NA_character_,
    round(v, 1) == 0 ~ "Even",
    v > 0 ~ paste0(sides[1], " +", abs(v)),
    TRUE ~ paste0(sides[2], " +", abs(v))
  )

  # The feed can stop before the count does (Arizona's ended while AP had ~83% in), so the
  # caption gives AP's percent reported as of the feed's last snapshot, as vote_share.R does.
  # Under 99%, "final" labels give the margin at that percent reporting (figure 7 keeps
  # its title). Primaries are left out, since AP's race for a party's primary isn't matched here.
  cutoff <- max(x$race$time)
  ap <- if (is.na(primary)) ap_race_at(state, contest, as.integer(district), cutoff) else NULL
  partial <- !is.null(ap) && ap$pct < 99
  if (!is.null(ap)) caption <- paste0(caption, "\n", ap_caption_line(ap, state, contest, as.integer(district), cutoff))

  # When the feed stopped short, the race-wide share counted is measured against AP's
  # expected vote (from AP's newest snapshot, its best estimate) instead of the feed's last
  # snapshot, so figure 4 and the 5% / 99% cutoffs reflect the real count. AP has no vote
  # methods, so the per-method shares (figures 5 and 6) stay relative to the last snapshot.
  ap_latest <- if (!is.null(ap)) ap_race_at(state, contest, as.integer(district)) else NULL
  use_expected <- partial && !is.null(ap_latest) && !is.na(ap_latest$expected)
  race <- x$race
  if (use_expected) race <- mutate(race, share_counted = total / ap_latest$expected)

  # Time figures end 1 hour after the last update that matters (vote_window()), so late
  # trickles and idle scrapes don't squeeze election night into a sliver. Totals still use
  # the last snapshot.
  counted <- filter(race, total > 0)
  window <- vote_window(race$time, race$total, cutoff)
  window_end <- window$end
  late_note <- window$note

  # The reference margin in figures 4 and 6: the files' own final margin, unless AP's newest
  # statewide margin differs from it by more than ap_margin_tolerance, as when the files cover only
  # some counties (Texas, Michigan). Then AP's margin is drawn and labeled as AP's.
  files_margin <- last(counted$final_margin)
  use_ap_margin <- x$margin_label == "D\u2013R" && !is.null(ap_latest) && !is.na(ap_latest$margin) &&
    abs(ap_latest$margin - files_margin) > ap_margin_tolerance
  final_margin <- if (use_ap_margin) ap_latest$margin else files_margin
  if (use_ap_margin) {
    message(x$race_id, ": files' margin ", label_margin(round(files_margin, 1)), " differs from AP's ",
            label_margin(round(ap_latest$margin, 1)), ", so AP's statewide margin is drawn")
  }
  final_label <- if (use_ap_margin) {
    paste0("Statewide margin (AP), ", round(ap_latest$pct), "% reporting")
  } else if (partial) {
    paste0("Margin at ", round(ap$pct), "% reporting")
  } else {
    "Final vote margin"
  }

  list(
    state = state, contest = contest, primary = primary, district = district,
    state_name = state_name, scope = paste0(state_name, " ", race_name),
    label_margin = label_margin, caption = caption, cutoff = cutoff, ap = ap, partial = partial,
    final_word = if (partial) paste0("Margin at ", round(ap$pct), "% reporting") else "Final",
    use_expected = use_expected, race = race,
    final_margin = final_margin, files_margin = files_margin, use_ap_margin = use_ap_margin,
    final_label = final_label, ap_latest_pct = if (!is.null(ap_latest)) ap_latest$pct else NA_real_,
    ap_latest_total = if (!is.null(ap_latest)) ap_latest$total else NA_real_,
    window_end = window_end, in_window = \(d) filter(d, time <= window_end),
    late_note = late_note, time_scale = time_axis(min(counted$time), window_end)
  )
}

#' The vote methods a race splits by and their color scale, or NULL when it reports one method
#' only (sources that don't split by method report only "all", so there is nothing to compare)
method_context <- function(x) {
  if (is.null(x$methods) || nrow(x$methods) < 2) return(NULL)
  levels <- intersect(names(method_labels), x$methods$method)
  list(
    levels = levels,
    scale = scale_color_manual(values = method_colors[levels], labels = method_labels[levels],
                               breaks = levels, name = "Vote method")
  )
}
