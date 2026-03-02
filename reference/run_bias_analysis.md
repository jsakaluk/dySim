# Run Bias Analysis

Convenience wrapper for bias/RMSE/coverage designs. The design's
`analysis_model` can include `"ICC_LMM"` to compare ICC estimates (from
dyLMM) alongside dySEM-based models; ICC is mapped to `XA~~XB` for bias
and coverage. ICC_LMM requires non-negative `lv_cov` in the population
values.

## Usage

``` r
run_bias_analysis(
  design,
  true_values = NULL,
  true_param_map = NULL,
  extract = c("estimates", "convergence", "coverage"),
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
  include `true_values` for coverage.

- true_values:

  Named list of true parameter values (fixed across scenarios). Ignored
  when `true_param_map` is provided.

- true_param_map:

  Named list mapping parameter labels to design column names. Use when
  true values vary by scenario (e.g. `list("XA~~XB" = "lv_cov")`). Each
  scenario gets `list(param = design[i, col])` for coverage and bias.

- extract:

  Character vector: `"estimates"`, `"convergence"`, `"coverage"`. For
  bias/RMSE only (faster), use `c("estimates", "convergence")` to skip
  CI computation via `performance::icc`; add `"coverage"` when coverage
  is needed.

- reps, parallel, workers, save_dir:

  Passed to `run_simulation`.

## Value

Tibble with raw results; use
[`summarize_bias_by()`](https://jsakaluk.github.io/dySim/reference/summarize_bias_by.md),
[`calc_coverage()`](https://jsakaluk.github.io/dySim/reference/calc_bias_rmse.md)
for summaries.

## See also

Other simulation:
[`run_one_scenario()`](https://jsakaluk.github.io/dySim/reference/run_one_scenario.md),
[`run_power_analysis()`](https://jsakaluk.github.io/dySim/reference/run_power_analysis.md)
