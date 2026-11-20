# A faster snapshot reader for scripts that only need election night and the final count
# (code/margin_direction.R, code/early_swing_figure.R). Not used unless a script calls
# use_night_snapshots().
#
# build_race() reads every pull a state has saved since election day (Pennsylvania has ~2,500).
# After use_night_snapshots(), each county feed is read only through `through` plus its latest
# file, which still gives the final total, and each file is read once per state and shared by
# that state's races. On Georgia's governor race this gives the same election-night margins as
# the full read from 29 files instead of 67; snapshots between `through` and the last file are
# skipped, so don't use it for figures of the later count.

#' Switch snapshot_files() and read_snapshot_file() to the election-night reader for this session
use_night_snapshots <- function(through = lubridate::ymd_hm(paste(as.Date(ELECTION_DATE) + 1, "12:00"), tz = TZ)) {
  env <- globalenv()
  all_files <- get("snapshot_files", envir = env)
  read_file <- get("read_snapshot_file", envir = env)
  cache <- new.env()

  assign("snapshot_files", function(clean_dir) {
    files <- all_files(clean_dir)
    stamp <- lubridate::ymd_hms(str_extract(basename(files), ts_regex), tz = TZ)
    feed <- str_remove(basename(files), paste0("_", ts_regex, "\\..*$"))
    latest <- !duplicated(feed, fromLast = TRUE)   # files come sorted by name, so by time within a feed
    files[stamp <= through | latest]
  }, envir = env)

  assign("read_snapshot_file", function(f) {
    if (!identical(cache$dir, dirname(f))) {       # a new state: drop the last state's files
      rm(list = ls(cache), envir = cache)
      cache$dir <- dirname(f)
    }
    key <- basename(f)
    if (is.null(cache[[key]])) cache[[key]] <- read_file(f)
    cache[[key]]
  }, envir = env)
  invisible(through)
}
