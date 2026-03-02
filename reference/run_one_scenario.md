# Run One Simulation Scenario

Generate data, fit models, and extract results for a single scenario.
Supports model mismatch (DGP model can differ from analysis model). Use
`analysis_model = "ICC_LMM"` to fit an ICC model via dyLMM (requires
non-negative `lv_cov` in population values).

## Usage

``` r
run_one_scenario(
  pop_model_x = c("Uni", "Cor"),
  analysis_model,
  n_items_x,
  n_dyads,
  pop_values_x,
  x_order = "sip",
  x_stem = "x",
  x_delim1 = "",
  x_delim2 = "_",
  distinguish_1 = "A",
  distinguish_2 = "B",
  lvname = "X",
  reps = 100,
  seed = NULL,
  extract = c("estimates", "convergence"),
  true_values = NULL,
  fit_options = NULL
)
```

## Arguments

- pop_model_x:

  `"Uni"` or `"Cor"` — model that generates data.

- analysis_model:

  Character; lavaan model syntax (from dySEM or hand-written), or
  `"ICC_LMM"` for ICC via dyLMM.

- n_items_x:

  Number of indicators per partner.

- n_dyads:

  Number of dyads.

- pop_values_x:

  Named list of population values (Uni or Cor structure).

- x_order, x_stem, x_delim1, x_delim2, distinguish_1, distinguish_2:

  Variable naming.

- lvname:

  Latent variable name.

- reps:

  Number of replications.

- seed:

  Base seed; each rep gets seed + rep.

- extract:

  Character vector: `"estimates"`, `"fit"`, `"convergence"`,
  `"coverage"`.

- true_values:

  Optional named list of true values for coverage.

- fit_options:

  Named list passed to
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) (e.g.
  `list(estimator = "mlr")`).

## Value

Tibble with one row per rep and columns for requested outputs.

## See also

Other simulation:
[`run_bias_analysis()`](https://jsakaluk.github.io/dySim/reference/run_bias_analysis.md),
[`run_power_analysis()`](https://jsakaluk.github.io/dySim/reference/run_power_analysis.md)
