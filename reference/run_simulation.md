# Run simulation over design grid.

Run simulation over design grid.

## Usage

``` r
run_simulation(
  design,
  reps = 100,
  parallel = FALSE,
  workers = NULL,
  save_dir = NULL,
  extract = c("estimates", "convergence")
)
```

## Arguments

- design:

  Tibble (from
  [`create_design()`](https://jsakaluk.github.io/dySim/reference/create_design.md))
  or named list for single scenario.

- reps:

  Number of replications per scenario.

- parallel:

  Use future for parallel execution (one future per scenario).

- workers:

  Number of parallel workers.

- save_dir:

  If set, save each scenario to `scenario_<i>.rds` and optionally
  combine.

- extract:

  Elements to extract per rep.

## Value

Tibble of raw results.
