# How many races' margins moved toward each party between an early point in the count and the
# final result, 1 hour and 4 hours after polls closed, with the median size of the moves in each
# group: big early moves mostly toward Republicans, small late ones tilting toward Democrats.
#
# Reads outputs/margin_direction/races.csv (run code/margin_direction.R first), which takes each
# race from its state files or AP's statewide running count (race_runs() in R/early_count.R).
# Writes outputs/margin_direction/late_moves.png.
#   Rscript code/late_moves_figure.R
source("setup.R")
library(ggplot2)
library(ggmedsl)

medsl_fonts(dpi = 300)

points_shown <- c(`1 hour` = "1 hour after polls closed", `4 hours` = "4 hours after polls closed")
directions <- c("Toward Republicans", "Little change", "Toward Democrats")

out_dir <- file.path(OUTPUT_DIR, "margin_direction")
races <- readr::read_csv(file.path(out_dir, "races.csv"), show_col_types = FALSE) |>
  filter(point %in% names(points_shown), !is.na(direction))

counts <- races |>
  summarise(races = n(), median_move = median(abs(shift)), .by = c(point, direction)) |>
  tidyr::complete(point, direction = directions, fill = list(races = 0L)) |>
  mutate(panel = factor(points_shown[point], levels = points_shown),
         direction = factor(direction, levels = directions),
         # the bar height gives the count, so label only the median move
         label = if_else(direction == "Little change" | races == 0, "",
                         paste0("median ", sprintf("%.1f", median_move), " pts")))
n_by_point <- races |> count(point)
n_races <- n_distinct(races$race_id)

direction_colors <- c(`Toward Republicans` = party_colors[["Republican"]],
                      `Little change` = medsl_colors[["gold"]],
                      `Toward Democrats` = party_colors[["Democrat"]])

p <- ggplot(counts, aes(direction, races, fill = direction)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label), vjust = -0.3, size = 3.2, lineheight = 0.9, color = "#444444",
            family = "StyreneB-Regular") +
  facet_wrap(~panel, nrow = 1) +
  scale_fill_manual(values = direction_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Races by Direction the Count Moved After Each Point",
    subtitle = paste(n_races, "statewide races, 2026 general election"),
    x = NULL, y = "Races",
    caption = medsl_caption(source = "state and county election websites; Associated Press"),
    tag = paste0("Move: final margin minus the margin at that point; little change is under 1 point.\n",
                 "1-hour panel: ", n_by_point$n[n_by_point$point == "1 hour"], " of ", n_races,
                 " races (the rest had no count yet).")
  ) +
  theme_medsl() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.tag.position = c(0.99, 0.12 / 5),   # 0.12 in above the bottom of the 5 in figure
    plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0),
    plot.margin = margin(10, 10, 30, 10)
  )

out_file <- file.path(out_dir, "late_moves.png")
ggsave_medsl(out_file, p, width = 9, height = 5)
cli::cli_alert_success("Wrote {out_file}")
