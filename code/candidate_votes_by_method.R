# Final votes for a race's top two candidates, by vote method, from the last snapshot on file.
# Each bar is labeled with the candidate's share of the two candidates' votes in that method. Writes outputs/candidate_votes_by_method/{ST}_{contest}_candidate_votes_by_method.png.
#   Rscript code/candidate_votes_by_method.R --state AZ --race gov
source("setup.R")

state_names <- setNames(state.name, state.abb)
office_names <- c(gov = "Governor", sen = "U.S. Senate", ag = "Attorney General", sos = "Secretary of State")
party_names <- c(dem = "Democrat", rep = "Republican")

# "biggs, andy" -> "Andy Biggs"
display_name <- \(x) {
  parts <- str_split_fixed(x, ",\\s*", 2)
  tools::toTitleCase(trimws(ifelse(nzchar(parts[, 2]), paste(parts[, 2], parts[, 1]), parts[, 1])))
}

plot_candidate_votes_by_method <- function(clean_dir, contest, election_date,
                                           fig_dir = file.path(OUTPUT_DIR, "candidate_votes_by_method")) {
  st <- basename(dirname(clean_dir))
  out_file <- fresh_figure(fig_dir, paste0(st, "_", contest, "_candidate_votes_by_method.png"))
  snaps <- read_snapshots(clean_dir, contest, election_date = election_date)
  as_of <- max(snaps$time)

  # each precinct x candidate x method at its latest pull, so a precinct missing from the
  # last snapshot keeps its last count
  final <- snaps |>
    slice_max(time, n = 1, by = c(geo, candidate, method), with_ties = FALSE) |>
    summarise(votes = sum(votes), .by = c(candidate, party, method))
  top_two <- final |> summarise(votes = sum(votes), .by = c(candidate, party)) |> slice_max(votes, n = 2)
  others <- final |> anti_join(top_two, by = "candidate") |> summarise(votes = sum(votes))

  levels <- intersect(names(method_labels), final$method)
  d <- final |>
    semi_join(top_two, by = "candidate") |>
    mutate(
      share = votes / sum(votes), .by = method
    ) |>
    mutate(
      name = paste0(display_name(candidate), " (", toupper(substr(party, 1, 1)), ")"),
      party_name = unname(party_names[party]),
      method = factor(method, levels = rev(levels)),
      label = percent(share, accuracy = 0.1)
    )
  fills <- setNames(party_colors[d$party_name], d$name)

  p <- ggplot(d, aes(votes, method, fill = name)) +
    geom_col(position = position_dodge(width = 0.8, reverse = TRUE), width = 0.75) +
    geom_text(aes(label = label), position = position_dodge(width = 0.8, reverse = TRUE),
              hjust = -0.08, size = 3, family = "StyreneB-Regular", color = "#444444") +
    scale_fill_manual(values = fills, name = NULL) +
    scale_y_discrete(labels = method_labels) +
    # room on the right for the largest bar's label
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.22))) +
    labs(
      title = "Votes by Candidate and Vote Method",
      subtitle = paste(state_names[[st]], office_names[[contest]] %||% contest),
      x = "Votes", y = NULL,
      caption = paste0(medsl_caption(source = "state and county election websites"),
                       "\nLast snapshot on file: ", as_of_et(as_of)),
      tag = join_notes(
        "Labels give each candidate's share of the two candidates' votes in that method.",
        if (others$votes > 0) paste0("Other candidates and write-ins (", comma(others$votes), " votes) not shown.")
      )
    ) +
    theme_medsl() +
    note_theme +
    theme(panel.grid.major.y = element_blank(), legend.position = "top")

  ggsave_medsl(out_file, p, width = 8, height = 5)
  message("Wrote ", out_file)
  invisible(out_file)
}

if (sys.nframe() == 0) {
  run <- race_run()
  contests <- if (identical(run$contest, "all")) "gov" else run$contest
  for (state_dir in run$state_dirs) for (contest in contests) {
    plot_candidate_votes_by_method(file.path(state_dir, "clean"), contest,
                                   state_election_date(run, basename(state_dir)))
  }
}
