# enight-reporting

How often precincts report on election night, how far interim counts sit from the
final result, and whether those swings track reporting speed or vote method. Reads
the timestamped snapshots that cbs-enr writes from enightdata scrapes:
`{dropbox}/{election_folder}/{STATE}/clean/{STATE}_{county}_{timestamp}.parquet`.
Nothing is written back to Dropbox.

## Layout

```
config.R        settings: election date and folder, data paths, county figures to draw
setup.R         sourced by every script: config.R, then every file in R/
R/              helper functions only (reading snapshots and AP, race loop, shared figure code)
code/           the scripts that draw figures, one per figure (or figure set)
helper_files/   poll close times, election dates and excluded races, per election folder
outputs/        everything code/ writes, one subfolder per script (git ignores it)
```

## Running it

Run every script from this folder. Data is read from `CBS_DATA_PATH` if set, else
`MEDSL_DATA_PATH` (default `~/Dropbox-MIT/Rachel Wright/medsl-enr`); set either in
`~/.Renviron`. Each script writes to its own folder in `outputs/` and replaces its own last figures.

**Margin figures** from the state and county snapshots. Arguments: election folder (default:
the configured one), contest, and states. The contest is `all` (every statewide office, the
default), one statewide office (`sen`, `gov`), `house` (every U.S. House district the state's
files hold) or one district (`house_17`):

```bash
Rscript code/fig4_running_margin.R "<dropbox>/26_general" sen GA,PA          # running race margin
Rscript code/fig5_method_counted.R "<dropbox>/26_general" all GA,PA          # share of each method counted, per state
Rscript code/fig6_method_running_margin.R "<dropbox>/26_general" sen         # running margin by method
Rscript code/fig7_method_margin_by_county.R "<dropbox>/26_general" sen       # final margin by method: race-wide, and by county
Rscript code/comparisons_state_reporting_speed.R "<dropbox>/26_general" all  # cross-state comparison
Rscript code/fig6_method_running_margin.R "<dropbox>/26_general" house TX    # every Texas House district
Rscript code/fig4_running_margin.R "<dropbox>/26_general" house_17 NY        # New York's 17th
```

House figures are named by race, e.g. `TX_house_7_method_running_margin.png`; figure 7's race-wide
graph is `{race}_method_margin_districtwide.png`. The House comparison goes to
`outputs/compare_states/house/`, beside the statewide one. Figure 5 is per state, counting ballots
across every contest (House included), so it takes no district. Figures 6 and 7 are skipped for
races whose sources don't split votes by method, and 7 for race-wide-only feeds.

**From the AP feed** (`{folder}/networks/ap/clean/`, or `AP_CLEAN_PATH`):

```bash
Rscript code/reporting_speed.R         # votes and share reported by hour since poll close, per state
Rscript code/reporting_speed.R house   # the same for every U.S. House district
Rscript code/race_timeline.R           # vote count updates by race, per state
Rscript code/race_timeline.R house     # the same with one lane per U.S. House district
```

House reporting speed goes to `outputs/reporting_speed/{ST}/house/`, and the House timeline to
`outputs/race_timeline/{ST}/{ST}_house_race_timeline_{timestamp}.png`.

**County reporting**: `Rscript code/fig2_county_reporting_speed.R` draws each row of
`COUNTY_REPORTING` (`config.R`) as one graph per vote method (election day, early, mail, and for
a whole county all methods), e.g. `outputs/fig2_county_reporting/NY/NY_westchester_reporting_gov_mail.png`.
A row with a district (NY, westchester, house, 17) draws the county's precincts in that House
district. It keeps each county's history between passes in `outputs/county_reporting/history/`.

If the snapshots' `election` column is wrong (it records the configured date at
scrape time), put the real dates in `helper_files/{election folder}_dates.csv`
(`state,election`). `helper_files/26_primaries_dates.csv` is there for the 2026 primaries.

Cleaning applied while reading: snapshots before election day are dropped; rows
repeated within a pull are removed (CA Orange listed 110,250 rows twice on June 8); a
pull identical to a later pull, with lower totals in between, is dropped as
mislabeled (TX Harris has a May 26 01:04 file that is really the May 27 01:04 pull); a
feed that drops to zero is treated as a reset and its earlier pulls are dropped; a single
pull more than 2% below its feed's earlier peak is dropped as a broken scrape (partial
pages, or pulls that moved votes between precincts, as CA Orange did June 15-16);
missing votes count as 0.

## Definitions

- **Report**: a snapshot where any candidate x method count in a precinct differs
  from the previous snapshot. Changes between two scrapes collapse into one report.
- **Final**: the last snapshot on file, not certified results.
- **Margin**: D minus R when both parties are on the ballot, otherwise first minus
  second place in the final count.

## Cross-state comparison (`outputs/compare_states/`)

`code/comparisons_state_reporting_speed.R` compares every race it builds (with `house`, every
U.S. House district, in `outputs/compare_states/house/`). Swing here is frequency, on the
race-wide running margin, counted once 5% of the final vote is in:

- **Big moves**: snapshots where the margin moved more than 1 point from the previous snapshot.
- **Lead changes**: times the leader in the running count switched.
- **Speed**: hours from the race's first results (1% of the final vote) to 90% reported.

| File | Contents |
|---|---|
| `3_speed_moves_leads_by_race.png` | All three measures for every race, fastest at top |

**Benchmarks after poll close**, following Shaikh and Stewart, "Why the Wait?" (2026): at 1
hour, 4 hours, 12 hours, 2 days and 1 week after polls close, the share of the final
vote reported and the shift in two-party share from its final value (in a general,
the Republican share, so a positive shift is a blue shift still to come; in a primary,
the eventual winner's share). Poll close times (Eastern) go in
`helper_files/{election folder}_poll_close.csv`; without one, a race's clock starts at its
first results. For states that span two time zones the earlier close is used.

| File | Contents |
|---|---|
| `4_share_reported_by_benchmark.png` | Share reported at each benchmark, by race |
| `6_share_vs_distance_by_benchmark.png` | Share reported against absolute shift at 1, 4 and 12 hours |

Races whose margin isn't meaningful can be listed in `helper_files/{election folder}_exclude.csv`
(`race_id,reason`); they stay in the table but are left off the figures.
