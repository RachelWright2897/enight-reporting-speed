# Write the two fake test states (ZZ, ZY) to data/simulated/.
# The simulator itself is simulate_state() in R/simulate.R.

source("R/simulate.R")

simulate_state(
  "ZZ",
  tibble(
    county = c("adams", "burke", "carver", "dane", "elko", "ford"),
    n_prec = c(60, 25, 40, 15, 35, 50),
    style  = c("batch", "steady", "batch", "steady", "steady", "batch"),
    lean   = c(0.55, 0.40, 0.48, 0.35, 0.60, 0.45),
    size   = c(900, 500, 700, 400, 600, 800)
  ),
  seed = 20261103
)

# A smaller, mostly steady-reporting state that leans D
simulate_state(
  "ZY",
  tibble(
    county = c("garfield", "hale", "irwin", "jasper"),
    n_prec = c(45, 20, 30, 25),
    style  = c("steady", "steady", "batch", "steady"),
    lean   = c(0.62, 0.50, 0.58, 0.44),
    size   = c(800, 450, 650, 500)
  ),
  seed = 20261104
)
