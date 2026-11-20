# Two figures for the early-count article (every statewide general race, or --state / --race as
# in R/races.R):
#
#   convergence.png      How far each race's running margin was from its final margin, by hours
#                        after polls closed: one thin line per race and the median across races.
#                        From the state files where they can stand for the statewide count, else
#                        AP's statewide running count (race_runs() in R/early_count.R).
#   method_margins.png   Each race's final margin within each vote method (mail, early in-person,
#                        election day, ...): the gap between methods is why the order they are
#                        counted in moves the running margin. State files only (AP has no vote
#                        methods), so races whose files can't stand for the statewide count
#                        (early_count_flags()) are left out.
# Writes to outputs/margin_direction/.
#   Rscript code/early_leads_figures.R
#   Rscript code/early_leads_figures.R --race gov --state GA,PA,KS
source("setup.R")
library(ggplot2)
library(ggmedsl)

medsl_fonts(dpi = 300)
use_night_snapshots()

hours_shown <- 12            # convergence x axis: hours after polls close
distance_cap <- 40           # convergence y axis tops out here (points); larger distances are clipped
grid_step <- 0.25            # hours between the median's points

run <- race_run()
closes <- read_poll_close()
exclude_file <- fs::path(CONFIG_DIR, paste0(run$folder, "_exclude.csv"))
excluded <- if (fs::file_exists(exclude_file)) readr::read_csv(exclude_file, show_col_types = FALSE)$race_id else character()
office <- c(sen = "Senate", gov = "Governor")

all_built <- for_each_race(run) |> purrr::keep(\(x) identical(x$margin_label, "D–R"))
runs <- race_runs(all_built, ap_running_margins(ap_offices(run)), excluded, run)
built <- purrr::keep(all_built, \(x) is.na(early_count_flags(x, excluded)))   # complete state files, for methods
if (length(runs) == 0) stop("No general-election races to draw")

race_label <- \(x) {
  state <- str_sub(x$race_id, 1, 2)
  contest <- str_match(x$race_id, "^[A-Z]{2}_([a-z]+)")[, 2]
  paste0(state, " ", coalesce(office[contest], contest))
}
labels <- purrr::map_chr(built, race_label)
left_out_note <- paste0("Left out: races whose files start after most votes were in, cover one county,\n",
                        "or cover only part of the state (final differs from AP's).")
n_ap <- sum(purrr::map_chr(runs, "source") == "AP")
out_dir <- file.path(OUTPUT_DIR, "margin_direction")
fs::dir_create(out_dir)
note_style <- theme(plot.tag.position = c(0.99, 0.02),
                    plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0),
                    plot.margin = margin(10, 10, 30, 10))

# ---- Convergence ---------------------------------------------------------------

lines <- purrr::map(runs, \(x) {
  label <- x$race_id
  state <- str_sub(x$race_id, 1, 2)
  close <- closes$time[match(state, closes$state)]
  if (is.na(close)) return(NULL)
  x$race |>
    filter(total > 0) |>
    arrange(time) |>
    transmute(race = label, hours = as.numeric(difftime(time, close, units = "hours")),
              distance = abs(margin - last(margin)))
}) |>
  purrr::list_rbind() |>
  filter(hours >= 0, hours <= hours_shown)

# each race's distance carried forward to a common grid, then the median across races at each step
# (a race enters the median once it has reported)
grid <- seq(0, hours_shown, by = grid_step)
median_line <- lines |>
  summarise(distance = list(approx(hours, distance, xout = grid, method = "constant", rule = 1:2)$y), .by = race) |>
  mutate(hours = list(grid)) |>
  tidyr::unnest(c(hours, distance)) |>
  filter(!is.na(distance)) |>
  summarise(distance = median(distance), races = n(), .by = hours) |>
  arrange(hours)
n_lines <- n_distinct(lines$race)

conv <- ggplot(lines, aes(hours, pmin(distance, distance_cap))) +
  geom_step(aes(group = race), color = medsl_colors[["sky"]], linewidth = 0.3, alpha = 0.6) +
  geom_step(data = median_line, aes(hours, distance), color = medsl_colors[["navy"]], linewidth = 1) +
  annotate("text", x = hours_shown, y = median_line$distance[nrow(median_line)], label = "Median race",
           hjust = 1, vjust = -0.8, size = 3.2, color = medsl_colors[["navy"]], family = "StyreneB-Regular") +
  scale_x_continuous(breaks = seq(0, hours_shown, by = 2), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = \(v) paste0(v, " pt"), limits = c(0, distance_cap), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Distance From the Final Margin by Hours After Poll Close",
    subtitle = paste(n_lines, "statewide races, 2026 general election"),
    x = "Hours since polls closed", y = "Distance from final D–R margin",
    caption = medsl_caption(source = "state and county election websites; Associated Press"),
    tag = paste0(c(paste0("Each light line is one race; distances over ", distance_cap, " points shown at ", distance_cap, "."),
                   paste0(n_lines - n_ap, " races from state files, ", n_ap,
                          " from AP's statewide running count where the state files fall short.")), collapse = "\n")
  ) +
  theme_medsl() +
  note_style
ggsave_medsl(file.path(out_dir, "convergence.png"), conv, width = 9, height = 5.5)
cli::cli_alert_success("Wrote {file.path(out_dir, 'convergence.png')}")
print(as.data.frame(filter(median_line, hours %in% c(0.5, 1, 2, 3, 4, 6, 8, 12))), row.names = FALSE)

# ---- Margin by vote method -------------------------------------------------------

methods <- purrr::map2(built, labels, \(x, label) {
  m <- x$methods
  if (is.null(m) || nrow(m) < 2) return(NULL)        # one method only: nothing to compare
  m |> filter(final_votes > 0) |>
    transmute(race = label, method, final_votes, final_margin, share = final_votes / sum(final_votes))
}) |>
  purrr::list_rbind() |>
  filter(method != "all", share >= 0.01)              # "not split by method" and slivers under 1% left off
races_split <- methods |> summarise(n = n(), .by = race) |> filter(n >= 2) |> pull(race)
methods <- methods |> filter(race %in% races_split)

# rows ordered by the gap between the most Democratic and most Republican method
order <- methods |> summarise(gap = max(final_margin) - min(final_margin), .by = race) |> arrange(gap) |> pull(race)
methods <- methods |>
  mutate(race = factor(race, levels = order),
         method = factor(method, levels = intersect(names(method_labels), unique(method))))

label_margin <- \(v) case_when(round(v) == 0 ~ "Even", v > 0 ~ paste0("D +", abs(round(v))), TRUE ~ paste0("R +", abs(round(v))))
meth <- ggplot(methods, aes(final_margin, race)) +
  geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +
  geom_line(aes(group = race), color = "#C4C4C4", linewidth = 0.6) +
  geom_point(aes(color = method, size = share), alpha = 0.9) +
  scale_color_manual(values = method_colors, labels = method_labels, name = "Vote method") +
  scale_size_area(max_size = 5, labels = scales::percent, breaks = c(0.1, 0.3, 0.6), name = "Share of race's votes") +
  scale_x_continuous(labels = label_margin, breaks = scales::breaks_pretty(n = 8)) +
  labs(
    title = "Final Margin by Vote Method",
    subtitle = paste(length(races_split), "statewide races that report votes by method, 2026 general election"),
    x = "D–R margin within the method (pp)", y = NULL,
    caption = medsl_caption(source = "state and county election websites"),
    # rows run from the largest gap between methods (top) to the smallest; methods under 1% of a
    # race's votes are left off
    tag = left_out_note
  ) +
  theme_medsl() +
  theme(legend.position = "right") +
  note_style +
  theme(plot.margin = margin(10, 10, 40, 10)) +   # room for the two-line note under the axis title
  guides(color = guide_legend(order = 1, override.aes = list(size = 3)), size = guide_legend(order = 2))
ggsave_medsl(file.path(out_dir, "method_margins.png"), meth, width = 9, height = 2.5 + 0.24 * length(races_split))
cli::cli_alert_success("Wrote {file.path(out_dir, 'method_margins.png')}")
