# The running margin over the first hours after polls closed for a few races side by side, each
# against its final margin, with a mark at 1 hour after close (the early point in
# code/margin_direction.R). Lines start once 5% of the final vote is in, as figure 4 does.
#
# Defaults to three races whose early count overstated the Democratic margin (DEFAULT_ARGS);
# --state and --race pick others (see R/races.R). Each race comes from its state files where they
# can stand for the statewide count, else from AP's statewide running count (race_runs() in
# R/early_count.R); AP races are marked with a dagger.
# Writes outputs/margin_direction/early_swing.png.
#   Rscript code/early_swing_figure.R
#   Rscript code/early_swing_figure.R --state VA,NJ,WI --race sen,gov
source("setup.R")
library(ggplot2)
library(ggmedsl)

medsl_fonts(dpi = 300)
use_night_snapshots()

hours_shown <- 8                         # hours after polls close on the x axis
DEFAULT_ARGS <- c("--state", "AR,KS,PA", "--race", "gov")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) args <- DEFAULT_ARGS
run <- race_run(args)
closes <- read_poll_close()

office <- c(sen = "Senate", gov = "Governor")
runs <- race_runs(for_each_race(run), ap_running_margins(ap_offices(run)), run = run)
races <- runs |>
  purrr::map(\(x) {
    state <- str_sub(x$race_id, 1, 2)
    close <- closes$time[match(state, closes$state)]
    if (is.na(close)) stop("No poll close time for ", state)
    contest <- str_match(x$race_id, "^[A-Z]{2}_([a-z]+)")[, 2]
    state_name <- coalesce(state.name[match(state, state.abb)], state)
    x$race |>
      filter(total > 0) |>
      arrange(time) |>
      mutate(hours = as.numeric(difftime(time, close, units = "hours")),
             final = last(margin),
             race = paste0(state_name, " ", coalesce(office[contest], contest), if (x$source == "AP") "\u2020" else ""))
  }) |>
  purrr::list_rbind()
if (nrow(races) == 0) stop("No general-election races to draw")

# keep the races in the order given, and draw from 5% counted to hours_shown
race_order <- unique(races$race)
lines <- races |> filter(share_counted >= 0.05, hours <= hours_shown) |>
  mutate(race = factor(race, levels = race_order))
finals <- distinct(races, race, final) |> mutate(race = factor(race, levels = race_order))
at_1h <- races |> filter(hours <= 1) |> slice_tail(n = 1, by = race) |>
  mutate(race = factor(race, levels = race_order))

label_margin <- \(v) case_when(round(v, 1) == 0 ~ "Even", v > 0 ~ paste0("D +", abs(round(v))), TRUE ~ paste0("R +", abs(round(v))))

p <- ggplot(lines, aes(hours, margin)) +
  geom_hline(yintercept = 0, color = "#444444", linewidth = 0.4) +
  geom_hline(data = finals, aes(yintercept = final), color = medsl_colors[["gold"]], linewidth = 0.6, linetype = "dashed") +
  geom_text(data = finals, aes(x = hours_shown, y = final, label = paste("Final:", label_margin(final))),
            hjust = 1, vjust = -0.7, size = 3, color = "#444444", family = "StyreneB-Regular") +
  geom_vline(xintercept = 1, color = "#999999", linewidth = 0.3, linetype = "dotted") +
  geom_step(color = "#444444", linewidth = 0.3) +
  geom_point(aes(color = share_counted), size = 0.7) +
  geom_point(data = at_1h, aes(hours, margin), shape = 21, size = 2.6, fill = "white", color = "#444444", stroke = 0.7) +
  geom_text(data = at_1h, aes(hours, margin, label = paste("1 hour:", label_margin(margin))),
            hjust = -0.15, vjust = -0.6, size = 3, color = "#444444", family = "StyreneB-Regular") +
  facet_wrap(~race, nrow = 1) +
  scale_color_medsl_blue_c(labels = scales::percent, limits = c(0, 1), breaks = c(0, 0.5, 1), oob = scales::squish,
                           name = "Share of final vote counted") +
  scale_x_continuous(breaks = seq(0, hours_shown, by = 2), limits = c(0, hours_shown)) +
  scale_y_continuous(labels = label_margin, breaks = scales::breaks_pretty(n = 8)) +
  labs(
    title = "Running Margin in the Hours After Poll Close",
    subtitle = paste0(length(race_order), " statewide races, 2026 general election"),
    x = "Hours since polls closed", y = "D–R margin (pp)",
    caption = medsl_caption(source = if (any(str_detect(race_order, "\u2020"))) "state and county election websites; Associated Press"
                                     else "state and county election websites"),
    tag = paste0(c("Lines start once 5% of the final vote is counted. Open circle: margin 1 hour after polls closed.",
                   if (any(str_detect(race_order, "\u2020"))) "\u2020 AP's statewide running count: the state files cover only part of the state."),
                 collapse = "\n")
  ) +
  theme_medsl() +
  theme(
    legend.position = "top",
    plot.tag.position = c(0.99, 0.02),
    plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0),
    plot.margin = margin(10, 10, 30, 10)
  ) +
  guides(color = guide_colorbar(theme = theme(legend.key.width = unit(8, "lines"))))

out_dir <- file.path(OUTPUT_DIR, "margin_direction")
fs::dir_create(out_dir)
out_file <- file.path(out_dir, "early_swing.png")
ggsave_medsl(out_file, p, width = 10, height = 5)
cli::cli_alert_success("Wrote {out_file}")
