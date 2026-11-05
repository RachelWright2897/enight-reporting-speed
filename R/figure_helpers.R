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
  abs = "Absentee", adv = "Advance", all = "All methods (not split)"
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

note_theme <- theme(
  plot.tag.position = c(0.99, 0.02),
  plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0)
)
join_notes <- \(...) { notes <- c(...); if (length(notes)) paste(notes, collapse = " ") else NULL }

# Removes a figure's previous file before redrawing, so a figure that no longer applies
# (a state that stopped splitting by method) doesn't linger from the last run
fresh_figure <- function(fig_dir, file) {
  fs::dir_create(fig_dir)
  path <- fs::path(fig_dir, file)
  if (fs::file_exists(path)) fs::file_delete(path)
  path
}

#' Labels, caption and time window for one race from build_race()
race_context <- function(x) {
  caption <- medsl_caption(source = "State and county reporting sites, via enightdata")

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
  if (!is.null(ap)) {
    caption <- paste0(caption, "\n", round(ap$pct), "% of votes reported (AP) as of ", as_of_et(cutoff))
  }

  # When the feed stopped short, the race-wide share counted is measured against AP's
  # expected vote (from AP's newest snapshot, its best estimate) instead of the feed's last
  # snapshot, so figure 4 and the 5% / 99% cutoffs reflect the real count. AP has no vote
  # methods, so the per-method shares (figures 5 and 6) stay relative to the last snapshot.
  ap_latest <- if (partial) ap_race_at(state, contest, as.integer(district)) else NULL
  use_expected <- !is.null(ap_latest) && !is.na(ap_latest$expected)
  race <- x$race
  if (use_expected) race <- mutate(race, share_counted = total / ap_latest$expected)

  # Time figures end once 99% of the final vote is in, so one late canvass update
  # doesn't squeeze election night into a sliver. Totals still use the last snapshot.
  # A race that never reaches 99% (a feed that stopped early) runs to its last snapshot.
  counted <- filter(race, total > 0)
  reached <- counted$time[counted$share_counted >= 0.99]
  window_end <- if (length(reached)) min(min(reached) + hours(1), cutoff) else cutoff
  late_note <- if (max(x$updates$time) > window_end) {
    when <- format(window_end, "%b %d, %I:%M %p", tz = TZ) |> str_replace_all(c(" 0" = " "))
    paste0("Updates after 99% counted (", when, ") not shown.")
  }

  list(
    state = state, contest = contest, primary = primary, district = district,
    state_name = state_name, scope = paste0(state_name, " ", race_name),
    label_margin = label_margin, caption = caption, cutoff = cutoff, ap = ap, partial = partial,
    final_word = if (partial) paste0("Margin at ", round(ap$pct), "% reporting") else "Final",
    use_expected = use_expected, race = race,
    final_margin = last(counted$final_margin),
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
