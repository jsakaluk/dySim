# Run Power Analysis

Convenience wrapper for power-focused designs. Summarizes rejection
rates.

## Usage

``` r
run_power_analysis(
  design,
  target_param = NULL,
  alpha = 0.05,
  reps = 100,
  parallel = FALSE,
  workers = NULL,
  save_dir = NULL
)
```

## Arguments

- design:

  Design from
  [`create_design()`](https://jsakaluk.github.io/dySim/reference/create_design.md);
  must include `target_param` and `alpha`.

- target_param:

  Parameter label for power (e.g. `"XA~~XB"` for latent covariance). Use
  `paste0(lvname, dist)` for latent names — e.g. `"XA"` and `"XB"` for
  defaults.

- alpha:

  Significance level.

- reps, parallel, workers, save_dir:

  Passed to `run_simulation`.

## Value

Tibble with raw results; use
[`calc_power()`](https://jsakaluk.github.io/dySim/reference/calc_bias_rmse.md)
for rejection rates.

## See also

Other simulation:
[`run_bias_analysis()`](https://jsakaluk.github.io/dySim/reference/run_bias_analysis.md),
[`run_one_scenario()`](https://jsakaluk.github.io/dySim/reference/run_one_scenario.md)
