# Summarize Bias and RMSE by Design Conditions

Compute bias and RMSE for a target parameter, grouped by design factors
(e.g. `condition_label`, `lv_cov_label`, `n_dyads`). Use when you want
to see how performance metrics vary across simulation conditions.

## Usage

``` r
summarize_bias_by(
  data,
  param,
  true_param,
  ...,
  .converged_only = TRUE,
  .mcse = FALSE
)
```

## Arguments

- data:

  Tibble of raw results from `run_bias_analysis` or `run_simulation`
  (must have `estimates` column).

- param:

  Parameter spec (e.g. `"XA~~XB"`) passed to `extract_param_estimates`.

- true_param:

  Column name containing true values (e.g. `"lv_cov"` when latent
  covariance varies), or a scalar for a fixed true value.

- ...:

  Bare column names to group by (e.g. `condition_label`, `lv_cov_label`,
  `n_dyads`). Uses
  [`group_by`](https://dplyr.tidyverse.org/reference/group_by.html).

- .converged_only:

  If `TRUE` (default), exclude non-converged rows.

- .mcse:

  If `TRUE`, use simhelpers for MCSE when available.

## Value

Tibble with group columns plus `mean_est`, `bias`, `var`, `mse`, `rmse`,
`n_reps`.

## See also

Other performance:
[`calc_bias_rmse()`](https://jsakaluk.github.io/dySim/reference/calc_bias_rmse.md),
[`extract_param_estimates()`](https://jsakaluk.github.io/dySim/reference/extract_param_estimates.md)

## Examples

``` r
# After run_bias_analysis with design that has condition_label, lv_cov, n_dyads:
# summarize_bias_by(results_bias, "XA~~XB", true_param = "lv_cov",
#   condition_label, lv_cov_label, n_dyads)
```
