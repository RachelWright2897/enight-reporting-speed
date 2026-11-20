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
