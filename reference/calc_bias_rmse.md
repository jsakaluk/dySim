# Calculate Simulation Performance Metrics

Compute bias, RMSE, coverage, and power from raw simulation results.
Uses simhelpers for MCSE when available.

## Usage

``` r
calc_bias_rmse(
  data,
  estimates = "estimate",
  true_param = "true_value",
  mcse = TRUE
)

calc_coverage(
  data,
  estimates = "estimate",
  true_param = "true_value",
  ci_lower = "ci_lower",
  ci_upper = "ci_upper",
  mcse = TRUE
)

calc_power(data, pvalues = "pvalue", alpha = 0.05, mcse = TRUE)
```

## Arguments

- data:

  Tibble of raw results with columns for estimates, SEs, etc.

- estimates:

  Name or vector of point estimates.

- true_param:

  Name or vector of true parameter values.

- mcse:

  If `TRUE` (default), use simhelpers for MCSE when available.

- ci_lower, ci_upper:

  Column names or vectors for CI bounds.

- pvalues:

  Column name or vector of p-values.

- alpha:

  Significance level for rejection.

- ses:

  Optional; name or vector of standard errors.

## Value

Tibble with performance metrics.

## See also

Other performance:
[`extract_param_estimates()`](https://jsakaluk.github.io/dySim/reference/extract_param_estimates.md),
[`summarize_bias_by()`](https://jsakaluk.github.io/dySim/reference/summarize_bias_by.md)
