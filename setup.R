# Sourced by every script: settings, then every function file in R/. Run scripts from the repo
# root (Rscript code/fig4_running_margin.R ...), since paths are relative to it.
source("config.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
