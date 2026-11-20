# How Arizona voters cast ballots in the 2022 and 2026 generals, for the race with the most votes in
# both years: every candidate's votes in that race, split into early (mail and early in-person),
# election day and provisional.
#
# The race is picked from the statewide races on both ballots (U.S. Senate was on 2022's only), as
# the one with the most votes in each year. 2022 has vote methods only statewide, and counts mail and
# early in-person together as early ballots ("adv" in the clean file), so 2026's early in-person and
# mail are combined to match. Write-ins are left out of both years, since 2022 doesn't split them by
# method. Inputs are clean-schema files: 2022 from ~/Desktop/clean_az_2022.R, 2026 the latest AZ
# snapshot.
#   Rscript code/az_method_2022_vs_2026.R
#   Rscript code/az_method_2022_vs_2026.R --candidate hobbs     # one candidate's votes in that race (by surname)
#   Rscript code/az_method_2022_vs_2026.R --y2022 path/to/AZ_2022_general_....parquet --y2026 path/to/AZ_....parquet
# Writes outputs/az_method_2022_vs_2026/AZ_{contest}[_{surname}]_method_2022_vs_2026.png
source("setup.R")

opts <- parse_flags(commandArgs(trailingOnly = TRUE), c("y2022", "y2026", "candidate"))
who <- if (!is.null(opts$candidate)) tolower(str_squish(opts$candidate))
y2022 <- opts$y2022 %||% path.expand("~/Desktop/AZ_2022_general_2023_01_03_17_49_09.parquet")
y2026 <- opts$y2026 %||% {
  roots <- c(CBS_DATA_PATH, MEDSL_DATA_PATH)
  dirs <- file.path(roots[nzchar(roots)], ELECTION_FOLDER, "AZ", "clean")
  dirs <- dirs[fs::dir_exists(dirs)]
  max(snapshot_files(dirs[1]))
}

office_names <- c(sen = "U.S. Senate", gov = "Governor", sos = "Secretary of State", ag = "Attorney General")
combined_labels <- c(adv = "Early (mail and in-person)", eday = "Election day", prov = "Provisional")

read_methods <- function(path, year) {
  nanoparquet::read_parquet(path) |>
    mutate(across(where(is.factor), as.character)) |>
    filter(is.na(district), candidate != "write-in") |>
    # 2026 splits early ballots into early in-person and mail; 2022 can't
    mutate(method = if_else(method %in% c("early", "mail", "adv"), "adv", method)) |>
    summarise(votes = sum(votes), .by = c(contest, candidate, party, method)) |>
    mutate(year = year)
}
all_races <- bind_rows(read_methods(y2022, "2022"), read_methods(y2026, "2026"))

# Statewide races on both ballots, ranked by votes within each year
ranked <- all_races |>
  summarise(votes = sum(votes), .by = c(contest, year)) |>
  filter(n() == 2, .by = contest) |>
  mutate(rank = rank(-votes), .by = year)
top <- ranked |> summarise(worst = max(rank), votes = sum(votes), .by = contest) |> arrange(worst, desc(votes))
contest <- top$contest[1]
if (top$worst[1] > 1) message("No race led in both years; using ", contest, ", the highest in both combined")
message("Votes by race and year (statewide races on both ballots):")
print(as.data.frame(arrange(ranked, year, rank)))

d <- all_races |> filter(contest == .env$contest)
if (!is.null(who)) {
  # "hobbs" matches "hobbs, katie"; the candidate must be on the ballot in both years
  d <- filter(d, str_detect(candidate, paste0("^", who, "\\b")))
  if (n_distinct(d$candidate) != 1 || n_distinct(d$year) != 2) {
    cli::cli_abort("{.val {who}} must match one {contest} candidate in both years; matched {.val {unique(d$candidate)}}.")
  }
}
d_names <- distinct(d, candidate, party)
d <- d |>
  summarise(votes = sum(votes), .by = c(year, method)) |>
  mutate(share = votes / sum(votes), total = sum(votes), .by = year) |>
  mutate(
    year = factor(year, levels = c("2026", "2022")),
    method = factor(method, levels = rev(names(combined_labels))),
    # shares inside segments wide enough to hold them (provisional is ~0.3%)
    text = if_else(share >= 0.04, percent(share, accuracy = 0.1), "")
  )
stopifnot(setequal(unique(as.character(d$method)), names(combined_labels)), n_distinct(d$year) == 2)
totals <- distinct(d, year, total)

fills <- method_colors[c("adv", "eday", "prov")]
p <- ggplot(d, aes(share, year)) +
  geom_col(aes(fill = method), width = 0.7) +
  geom_text(aes(label = text, group = method, color = method == "adv"), position = position_stack(vjust = 0.5),
            size = 3.2, family = "StyreneB-Regular", show.legend = FALSE) +
  geom_text(data = totals, aes(x = 1.02, label = paste(comma(total), "votes")),
            hjust = 0, size = 3, family = "StyreneB-Regular", color = "#444444") +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "#222222"), guide = "none") +
  scale_fill_manual(values = unname(fills), breaks = names(combined_labels), labels = combined_labels,
                    limits = names(combined_labels), name = NULL) +
  scale_x_continuous(labels = percent, breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Share of Votes by Vote Method, 2022 and 2026",
    subtitle = if (is.null(who)) paste("Arizona", office_names[[contest]]) else {
      parts <- str_split_1(unique(d_names$candidate), ",\\s*")
      paste0(str_to_title(paste(parts[2], parts[1])), " (", toupper(substr(unique(d_names$party), 1, 1)), "), Arizona ",
             tolower(office_names[[contest]]))
    },
    x = if (is.null(who)) "Share of votes" else "Share of the candidate's votes", y = NULL,
    caption = medsl_caption(source = "Arizona Secretary of State and county election websites")
  ) +
  theme_medsl() +
  theme(legend.position = "top", panel.grid.major.y = element_blank())

out_file <- fresh_figure(file.path(OUTPUT_DIR, "az_method_2022_vs_2026"), paste0("AZ_", contest, if (!is.null(who)) paste0("_", str_extract(who, "^[a-z-]+")), "_method_2022_vs_2026.png"))
ggsave_medsl(out_file, p, width = 8, height = 3.6)
message("Wrote ", out_file)
print(as.data.frame(select(d, year, method, votes, share)))
