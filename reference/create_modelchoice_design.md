# Create Model Choice Simulation Design

Build a design grid for model choice analysis: multiple DGPs
(`pop_model_x`) with per-DGP population parameters, and a fixed set of
candidate analysis models fit to each generated dataset.

## Usage

``` r
create_modelchoice_design(
  analysis_models,
  pop_values_x,
  pop_model_x,
  pop_generator = NULL,
  ...,
  .labels = NULL,
  .seed = NULL
)
```

## Arguments

- analysis_models:

  Named list of lavaan scripts (length \>= 2). Names become
  `analysis_model_labels`. Order: most constrained (simplest) first.

- pop_values_x:

  Named list keyed by `pop_model_x`. Each element is
  `expand_pop_values_x`-compatible (fixed, conditions, cross factors).
  Uni structure: `loadings`, `residuals`, `coresids`. Cor structure:
  `loadings_p1`, `loadings_p2`, `residuals_p1`, `residuals_p2`,
  `coresids`, `lv_cov`.

- pop_model_x:

  Character vector of DGP levels (e.g. `c("Uni", "Cor")`). Names of
  `pop_values_x` must contain all of these.

- pop_generator:

  Optional named vector mapping `pop_model_x` to `"Uni"` or `"Cor"` for
  data generation. If `NULL`, inferred from parameter names (`loadings`
  -\> Uni, `loadings_p1` -\> Cor).

- ...:

  Other factors (e.g. `n_dyads`, `n_items_x`) to expand.

- .labels:

  Optional named list for `expand_pop_values_x` labels (e.g.
  `list(conditions = c("weak","strong"))`).

- .seed:

  Base seed; `seed = .seed + row_number()` per scenario.

## Value

Tibble with one row per scenario: `analysis_models` list-column,
`analysis_model_labels`, `pop_model_x`, `pop_values_x`, `pop_generator`,
and other design columns.

## Details

**Model order:** `analysis_models` must be ordered simplest (most
constrained) first. For `chi2_diff_startsimple` and
`chi2_diff_startcomplex`, the LRT compares nested models; if the complex
model is listed first, the test logic is wrong. Example for loading
invariance: `list(loading_inv = script_load, configural = script_none)`.

## See also

Other design:
[`create_design()`](https://jsakaluk.github.io/dySim/reference/create_design.md),
[`expand_pop_values_x()`](https://jsakaluk.github.io/dySim/reference/expand_pop_values_x.md)

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
  n_dyads = c(50, 100),
  n_items_x = 3,
  .seed = 12345
)
#> Error: object 'script_uni' not found
```
