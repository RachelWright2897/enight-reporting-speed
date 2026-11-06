ELECTION_DATE   = Sys.getenv("ELECTION_DATE", "2026-11-03")
ELECTION_FOLDER = Sys.getenv("CBS_ELECTION_FOLDER", "26_general")

# CBS-MIT Election Data Dropbox (live) and the MEDSL copy (backup), each holding
# {ELECTION_FOLDER}/{STATE}/clean/ and {ELECTION_FOLDER}/networks/ap/clean/. Set either in
# ~/.Renviron; MEDSL_DATA_PATH defaults to the synced medsl-enr folder in Dropbox-MIT.
CBS_DATA_PATH   = Sys.getenv("CBS_DATA_PATH")
MEDSL_DATA_PATH = Sys.getenv("MEDSL_DATA_PATH", path.expand("~/Dropbox-MIT/Rachel Wright/medsl-enr"))

# AP clean files. Unset: ap_dir() falls back to {CBS or MEDSL}/{ELECTION_FOLDER}/networks/ap/clean.
if (nzchar(Sys.getenv("AP_CLEAN_PATH"))) AP_CLEAN_PATH = Sys.getenv("AP_CLEAN_PATH")

# CBS county clean files, one folder per state; county_reporting.R fills in {ST}. Falls back to
# the CBS / MEDSL folders above when this one has no county files.
COUNTY_CLEAN_PATH = Sys.getenv("COUNTY_CLEAN_PATH", "/d/research/medsl-enr/26_general/{ST}/clean")

# Everything the scripts in code/ write: each script writes to outputs/{script name}/ (git
# ignores outputs/). county_reporting.R also keeps its county histories there, in history/.
OUTPUT_DIR  = "outputs"
CONFIG_DIR  = "helper_files"

