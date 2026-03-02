# Extract Estimates from a Fitted Model

Unified extraction of parameter estimates from lavaan or ICC_LMM fits
into a lavaan-style table (lhs, rhs, op, label, est, optionally
ci.lower/ci.upper). Used internally by `run_one_scenario`.

## Usage

``` r
extract_estimates(
  fit,
  model_type = c("lavaan", "ICC_LMM"),
  want_coverage = FALSE
)
```

## Arguments

- fit:

  Fitted lavaan or `lmerMod` object.

- model_type:

  `"lavaan"` or `"ICC_LMM"`.

- want_coverage:

  If `TRUE`, include confidence intervals for coverage.

## Value

Data frame with lavaan-style columns; `NULL` if extraction fails.
