# enight-reporting

How often precincts report on election night, how far interim counts sit from the
final result, and whether those swings track reporting speed or vote method. Reads
the timestamped snapshots that cbs-enr writes from enightdata scrapes:
`{dropbox}/{election_folder}/{STATE}/clean/{STATE}_{county}_{timestamp}.parquet`.
Nothing is written back to Dropbox.

## Running it

Run from this folder. Every state in an election folder:

```bash
Rscript R/03_run_states.R "<dropbox>/26_general" sen          # all states with a clean/ folder
Rscript R/03_run_states.R "<dropbox>/26_general" sen GA,PA    # only these states
Rscript R/03_run_states.R "<dropbox>/26_general" all          # every statewide office
```

**Each run writes to its own folder, `output/{YYYY-MM-DD_HH-MM-SS}/`**, named for when
the run started, so running it repeatedly over election night never overwrites an
earlier run. Each run folder has one subfolder per race plus `run_summary.csv`
listing which races ran and which were skipped (and why).

States without the contest are skipped. With `all`, uncontested races (no second
candidate with 1% of the vote) are skipped too.

One race at a time:

```bash
Rscript R/01_build.R "<dropbox>/26_general/GA/clean" sen            # new run folder; args: clean dir, contest, [district]
Rscript R/02_figures.R output/2026-11-03_21-05-00/GA_sen            # redraw figures for one race
```

`Rscript R/00_simulate.R` writes two fake states (ZZ, ZY) to `data/simulated/`. With
no arguments, 01 and 03 run on that, and 02 redraws ZZ_sen in the latest run.

If the snapshots' `election` column is wrong (it records the configured date at
scrape time), put the real dates in `config/{election folder}_dates.csv`
(`state,election`). `config/26_primaries_dates.csv` is there for the 2026 primaries.

Cleaning applied while reading: snapshots before election day are dropped; rows
repeated within a pull are removed (CA Orange listed 110,250 rows twice on June 8); a
pull identical to a later pull, with lower totals in between, is dropped as
mislabeled (TX Harris has a May 26 01:04 file that is really the May 27 01:04 pull); a
feed that drops to zero is treated as a reset and its earlier pulls are dropped; a single
pull more than 2% below its feed's earlier peak is dropped as a broken scrape (partial
pages, or pulls that moved votes between precincts, as CA Orange did June 15-16);
missing votes count as 0.

The margin figures (3, 8, 9) leave out precincts with fewer than 20 final votes, since
one ballot moves their margin by many points. Figure 3 stops at 100% counted and
notes how many reports fell beyond it.

## Definitions

- **Report**: a snapshot where any candidate x method count in a precinct differs
  from the previous snapshot. Changes between two scrapes collapse into one report.
- **Final**: the last snapshot on file, not certified results.
- **Margin**: D minus R when both parties are on the ballot, otherwise first minus
  second place in the final count.
- **Swing**: the largest absolute difference between any interim margin in a
  precinct and its final margin. Only precincts that reported more than once can
  swing, so the swing figures leave out single-report precincts.
- **Speed**: hours from the race's first results (1% of the final vote) until the
  precinct had 99% of its final vote.
- **Vote method gap**: the spread between the final margins of a precinct's vote
  methods, counting only methods with at least 10 votes and 5% of the precinct.
- **Swing split**: each interim deviation from the final margin is split exactly into
  *method mix* (the margin you'd get from the final per-method margins weighted by
  the methods counted so far, minus the final margin) and *within-method drift*
  (the rest). If method mix carries the swing, vote mode is the driver.

## Outputs (`output/{run}/{STATE}_{contest}/`)

| File | Contents |
|---|---|
| `precinct_reports.csv` | One row per precinct report: votes added, margin, share of the precinct's final vote counted, deviation from the precinct's final margin, margin of the added votes |
| `precinct_summary.csv` | Per precinct: number of reports, first and last report, median minutes between reports |
| `county_summary.csv` | Per county: reporting cadence, largest single update as share of county vote, votes removed |
| `method_summary.csv` | Per vote method: final votes and share, final margin, first votes, times 50% and 90% were counted, precinct reports |
| `county_method_summary.csv` | The same, per county x method |
| `precinct_report_decomposition.csv` | Each precinct report's deviation, split into method mix and within-method drift |
| `precinct_swing.csv` | Per precinct: swing, speed, vote method gap, and mean absolute deviation by part |
| `figures/` | 1–4: county count timeline, reports per precinct, interim vs final precinct margins, running race margin. 5–7, by vote method: share counted over time, running margin, final margin by method and county. 8–9, swing: sources of interim deviation, median swing by speed and vote method gap |

Figures 5–9 are skipped for states whose sources don't split votes by method (only `all`).

## Cross-state comparison (`output/{run}/comparison/`)

`03_run_states.R` finishes every run by comparing all of its races
(`Rscript R/04_compare_states.R [run folder]` redoes it for an earlier run). Swing
here is frequency, on the statewide running margin, counted once 5% of the final
vote is in:

- **Big moves**: snapshots where the margin moved more than 1 point from the previous snapshot.
- **Lead changes**: times the leader in the running count switched.
- **Speed**: hours from the race's first results (1% of the final vote) to 90% counted.

| File | Contents |
|---|---|
| `race_swing_summary.csv` | Per race: hours to 50/90/99% counted, snapshots, big moves (also counted from the start), lead changes, largest single move, final margin |
| `1_big_moves_by_speed.png` | Big moves against speed, one dot per race |
| `2_lead_changes_by_speed.png` | Lead changes against speed |
| `3_speed_moves_leads_by_race.png` | All three measures for every race, fastest at top |

**Benchmarks after poll close**, following Shaikh and Stewart, "Why the Wait?" (2026): at 1
hour, 4 hours, 12 hours, 2 days and 1 week after polls close, the share of the final
vote reported and the shift in two-party share from its final value (in a general,
the Republican share, so a positive shift is a blue shift still to come; in a primary,
the eventual winner's share). Poll close times (Eastern) go in
`config/{election folder}_poll_close.csv`; without one, a race's clock starts at its
first results. For states that span two time zones the earlier close is used.

| File | Contents |
|---|---|
| `race_benchmarks.csv` | Per race x benchmark: share reported, signed and absolute shift |
| `4_share_reported_by_benchmark.png` | Share reported at each benchmark, by race |
| `5_distance_from_final_by_benchmark.png` | Absolute shift at each benchmark, one line per race |
| `6_share_vs_distance_by_benchmark.png` | Share reported against absolute shift at 1, 4 and 12 hours |

Races whose margin isn't meaningful can be listed in `config/{election folder}_exclude.csv`
(`race_id,reason`); they stay in the table but are left off the figures.
