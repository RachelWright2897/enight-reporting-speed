# Command-line options shared by the scripts in code/: --name value (or --name=value), each
# given at most once. --state and --race take comma-separated lists.

#' The --options in `args` as a named list of strings, NULL for each one not given. Aborts on an
#' option not in `allowed`, or one without a value.
parse_flags <- function(args, allowed) {
  # --state=GA as well as --state GA
  args <- unlist(lapply(args, \(a) if (grepl("^--[a-z]+=", a)) c(sub("=.*", "", a), sub("^[^=]*=", "", a)) else a))
  opts <- rlang::set_names(vector("list", length(allowed)), allowed)
  i <- 1
  while (i <= length(args)) {
    key <- sub("^--", "", args[i])
    if (!startsWith(args[i], "--") || !key %in% allowed || i == length(args)) {
      cli::cli_abort("Unrecognized argument {.val {args[i]}}; options are {.arg {paste0('--', allowed)}}, each with a value.")
    }
    opts[[key]] <- args[i + 1]
    i <- i + 2
  }
  opts
}

#' "GA, pa" -> c("GA", "PA"); NULL stays NULL
flag_list <- function(x, upper = FALSE) {
  if (is.null(x)) return(NULL)
  out <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  out <- out[nzchar(out)]
  if (upper) toupper(out) else tolower(out)
}

# For the --start and --end of code/race_timeline.R and figure 5.
# One end of the drawn period as a time (ET): a number is hours after close_time, "HH:MM" a time
# on election day, anything else a date and time. NULL stays NULL (no limit).
timeline_bound <- function(x, close_time, election_date = ELECTION_DATE) {
  if (is.null(x)) return(NULL)
  if (grepl("^-?[0-9.]+$", x)) return(close_time + as.numeric(x) * 3600)
  if (grepl("^\\d{1,2}:\\d{2}$", x)) x <- paste(election_date, x)
  t <- suppressWarnings(lubridate::ymd_hms(x, tz = TZ, truncated = 3, quiet = TRUE))
  if (is.na(t)) cli::cli_abort("Can't read {.val {x}} as a time; use \"2026-11-03 21:00\", \"2026-11-06\", \"21:00\" or hours after polls close.")
  t
}

# "7 PM", "Nov 4 1:30 AM": a period's ends for the subtitle, the date only when not election day
timeline_clock <- function(t, election_date = ELECTION_DATE) {
  clock <- sub("^0", "", format(t, "%I:%M %p", tz = TZ)) |> sub(pattern = ":00", replacement = "")
  day <- as.Date(format(t, "%Y-%m-%d", tz = TZ))
  if (day == as.Date(election_date)) clock else paste(format(t, "%b", tz = TZ), as.integer(format(t, "%d", tz = TZ)), clock)
}
