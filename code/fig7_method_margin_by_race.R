# Figure 7, across races: the final margin by vote method for several of a state's races in one
# graph, one row per race, each method a bubble sized by its votes. Takes the same arguments as
# the other margin figures (R/races.R); rows follow the order of --race. Writes, per state:
#   outputs/fig7_method_margin_by_race/{ST}_method_margin_by_race.png
#   Rscript code/fig7_method_margin_by_race.R --state AZ --race gov,sos,house_1,house_5
source("setup.R")

plot_method_margin_by_race <- function(races, fig_dir = file.path(OUTPUT_DIR, "fig7_method_margin_by_race")) {
  state <- str_sub(races[[1]]$race_id, 1, 2)
  out_file <- fresh_figure(fig_dir, paste0(state, "_method_margin_by_race.png"))
  state_name <- coalesce(state.name[match(state, state.abb)], state)

  races <- keep(races, \(x) !is.null(method_context(x)))
  if (length(races) == 0) {
    message(state, ": no race splits by vote method, so no figure")
    return(invisible(NULL))
  }
  ctxs <- map(races, race_context)
  # race labels without the state, which goes in the subtitle: "Governor", "U.S. House District 5"
  race_names <- map_chr(ctxs, \(ctx) str_remove(ctx$scope, paste0("^", state_name, " ")))
  margin_label <- unique(map_chr(races, "margin_label"))
  if (length(margin_label) > 1) cli::cli_abort("{state}: races measure the margin differently ({margin_label}).")

  d <- map2(races, race_names, \(x, name) mutate(x$methods, race = name)) |>
    list_rbind() |>
    mutate(race = factor(race, levels = rev(race_names)))
  levels <- intersect(names(method_labels), d$method)
  d <- mutate(d, method = factor(method, levels = levels))

  # Each race's caption carries its own AP line; keep it when the races agree
  captions <- unique(map_chr(ctxs, "caption"))
  caption <- if (length(captions) == 1) captions else medsl_caption(source = "state and county election websites")
  ap_note <- if (length(captions) > 1) "AP's percent reported differs by race; see the single-race figures."

  p <- ggplot(d, aes(final_margin, race)) +
    geom_vline(xintercept = 0, color = "#444444", linewidth = 0.4) +
    geom_point(aes(color = method, size = final_votes), alpha = 0.85) +
    scale_color_manual(values = method_colors[levels], labels = method_labels[levels],
                       breaks = levels, name = "Vote method") +
    # statewide and district races differ in size by a factor of ten, so area runs from the
    # smallest race-method to the largest with a floor, as in the county graph
    scale_size(range = c(2, 9), labels = comma, name = "Votes", breaks = \(l) {
      first <- max(l[1], signif(l[2] / 10, 1))
      steps <- pretty(c(first, l[2]), n = 3)
      sort(unique(c(first, steps[steps > first * 1.5 & steps <= l[2]])))
    }) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8),
                       labels = \(v) ctxs[[1]]$label_margin(round(v, 1))) +
    guides(color = guide_legend(order = 1, override.aes = list(size = 3)), size = guide_legend(order = 2)) +
    labs(
      title = "Final Margin by Vote Method and Race",
      subtitle = state_name,
      x = paste(margin_label, "margin (pp)"), y = NULL,
      caption = caption, tag = join_notes(ap_note)
    ) +
    theme_medsl() +
    note_theme +
    theme(legend.position = "right", legend.box.spacing = unit(2.5, "lines"))

  ggsave_medsl(out_file, p, width = 8, height = 1.8 + 0.8 * length(races))
  message("Wrote ", out_file)
  invisible(out_file)
}

if (sys.nframe() == 0) {
  built <- for_each_race(race_run())
  by_state <- split(built, map_chr(built, \(x) str_sub(x$race_id, 1, 2)))
  invisible(map(by_state, plot_method_margin_by_race))
}
