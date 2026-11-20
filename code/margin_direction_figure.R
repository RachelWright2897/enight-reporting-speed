# Figure for code/margin_direction.R: for each race, the running margin early in the count against
# the final margin, one hour and four hours after polls closed. An open dot is the early margin, a
# filled dot the final one, and the line between them is colored by the way the count moved.
# Races drawn from AP's running count instead of the state files are marked with a dagger.
#
# Reads outputs/margin_direction/races.csv (run code/margin_direction.R first) and writes
# outputs/margin_direction/margin_direction.png.
#   Rscript code/margin_direction_figure.R
source("setup.R")
library(ggplot2)
library(ggmedsl)

medsl_fonts(dpi = 300)

points_shown <- c(`1 hour` = "1 hour after polls closed", `4 hours` = "4 hours after polls closed")

out_dir <- file.path(OUTPUT_DIR, "margin_direction")
races <- readr::read_csv(file.path(out_dir, "races.csv"), show_col_types = FALSE) |>
  filter(point %in% names(points_shown), !is.na(shift))

office <- c(sen = "Senate", gov = "Governor")
races <- races |>
  mutate(
    label = paste0(state, " ", coalesce(office[str_match(race_id, "^[A-Z]{2}_([a-z]+)")[, 2]], race_id),
                   if_else(source == "AP", "\u2020", "")),
    panel = factor(points_shown[point], levels = points_shown),
    direction = factor(direction, levels = c("Toward Democrats", "Toward Republicans", "Little change"))
  )
# rows ordered by the 1-hour shift: biggest move toward Democrats at top, toward Republicans at bottom
order <- races |> filter(point == "1 hour") |> arrange(shift) |> pull(label)
races <- races |> mutate(label = factor(label, levels = unique(c(order, setdiff(label, order)))))

label_margin <- \(v) case_when(v == 0 ~ "Even", v > 0 ~ paste0("D +", abs(v)), TRUE ~ paste0("R +", abs(v)))
direction_colors <- c(`Toward Democrats` = party_colors[["Democrat"]],
                      `Toward Republicans` = party_colors[["Republican"]],
                      `Little change` = medsl_colors[["gold"]])
n_races <- nlevels(droplevels(races$label))
fig_height <- 2.5 + 0.22 * n_races   # inches

p <- ggplot(races, aes(y = label)) +
  geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +
  geom_segment(aes(x = early_margin, xend = final_margin, yend = label, color = direction), linewidth = 0.9) +
  geom_point(aes(x = early_margin, shape = "Early in the count", color = direction), size = 1.9, fill = "white", stroke = 0.8) +
  geom_point(aes(x = final_margin, shape = "Final"), size = 1.9, color = "#444444") +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = direction_colors, name = "Count moved", drop = FALSE) +
  scale_shape_manual(values = c(`Early in the count` = 21, Final = 16), name = "Margin") +
  scale_x_continuous(labels = label_margin, breaks = scales::breaks_pretty(n = 6)) +
  labs(
    title = "Early and Final Margins by Race",
    subtitle = paste(n_races, "statewide races, 2026 general election"),
    x = "D–R margin (pp)", y = NULL,
    caption = medsl_caption(source = "state and county election websites; Associated Press"),
    tag = paste0("Little change: under 1 point.\n",
                 "No dot: no snapshot in the files by that time.\n",
                 "\u2020 AP's statewide running count (state files incomplete).")
  ) +
  theme_medsl() +
  theme(
    legend.position = "top",
    legend.box = "vertical",                      # direction and margin keys on separate rows
    legend.box.just = "center",
    plot.tag.position = c(0.99, 0.12 / fig_height),   # 0.12 in above the bottom edge, whatever the height
    plot.tag = element_text(size = 7, colour = "#666666", hjust = 1, vjust = 0),
    plot.margin = margin(10, 10, 30, 10)
  ) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2, override.aes = list(color = "#444444")))

out_file <- file.path(out_dir, "margin_direction.png")
ggsave_medsl(out_file, p, width = 9, height = fig_height)
cli::cli_alert_success("Wrote {out_file}")
