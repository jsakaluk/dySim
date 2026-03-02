# Run Model Choice Analysis

Fit competing models to each generated dataset, apply selection
criteria, and compute sensitivity/specificity with respect to a
correct-model mapping.

## Usage

``` r
run_modelchoice_analysis(
  design,
  correct_model,
  selection_criteria = c("chi2_diff_startsimple", "chi2_diff_startcomplex", "bic_diff"),
  alpha = 0.05,
  reps = 100,
  parallel = FALSE,
  workers = NULL,
  delta_cfi_cutoff = 0.01,
  delta_rmsea_cutoff = 0.015,
  delta_srmr_cutoff = 0.03,
  vote_criteria = NULL,
  fit_options = NULL
)
```

## Arguments

- design:

  Design from
  [`create_modelchoice_design()`](https://jsakaluk.github.io/dySim/reference/create_modelchoice_design.md).

- correct_model:

  Named vector or list: names = `pop_model_x` levels, values = analysis
  model labels (from `analysis_model_labels`).

- selection_criteria:

  Character vector: `"chi2_diff_startsimple"`,
  `"chi2_diff_startcomplex"`, `"bic_diff"`, `"aic_diff"`,
  `"sabic_diff"`, `"delta_cfi"`, `"delta_rmsea"`, `"delta_srmr"`,
  `"delta_cfi_or_delta_rmsea"`, `"delta_cfi_and_delta_rmsea"`,
  `"majority_vote"`.

- alpha:

  Alpha for chi2 difference tests.

- reps:

  Number of replications per scenario.

- parallel:

  Use future for parallel execution.

- workers:

  Number of parallel workers.

- delta_cfi_cutoff, delta_rmsea_cutoff, delta_srmr_cutoff:

  Cutoffs for delta criteria (defaults: 0.01, 0.015, 0.03).

- vote_criteria:

  Character vector of criteria for `"majority_vote"` (required when
  `"majority_vote"` is in `selection_criteria`).

- fit_options:

  Named list passed to
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

## Value

List with `raw` (full results) and `summary` (sensitivity, specificity
by scenario, DGP, criterion).

## Details

**Model order:** Design `analysis_models` must be ordered simplest first
(see
[`create_modelchoice_design`](https://jsakaluk.github.io/dySim/reference/create_modelchoice_design.md)).
**Low power:** With few items (e.g., 3), the chi2-difference test for
loading invariance has low power; BIC can prefer the wrong model because
it heavily penalizes extra parameters. Expect sensitivity to improve
with more items or larger n.

## Examples

``` r
dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
  distinguish_1 = "A", distinguish_2 = "B")
#> Error: 'build_dvn' is not an exported object from 'namespace:dySEM'
script_uni <- dySEM::scriptUni(dvn, scaleset = "FF", lvname = "X",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = "none")
#> Error: object 'dvn' not found
script_cor <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"))
#> Error: object 'dvn' not found
design <- create_modelchoice_design(
  pop_model_x = c("Uni", "Cor"),
  analysis_models = list(uni = script_uni, cor = script_cor),
  pop_values_x = list(
    Uni = list(loadings = c(.75,.8,.55), residuals = c(.44,.36,.7),
               coresids = c(.1,.12,.08)),
    Cor = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
               residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7),
               coresids = c(.1,.12,.08), lv_cov = 0.3)
  ),
  n_dyads = 100,
  n_items_x = 3,
  .seed = 12345
)
#> Error: object 'script_uni' not found
res <- run_modelchoice_analysis(design, correct_model = c(Uni = "uni", Cor = "cor"),
  reps = 25)
#> Error: object 'design' not found
```
