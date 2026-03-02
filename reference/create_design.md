# Create Simulation Design Grid

Expand design factors into a tibble for simulation. Use named lists for
`analysis_model`, `fit_options`, and `pop_values_x` elements (e.g.
`conditions`, cross factors) to get readable label columns for grouping
and summarization.

## Usage

``` r
create_design(..., .labels = NULL, .seed = NULL)

run_multifactor(
  design,
  reps = 100,
  parallel = FALSE,
  workers = NULL,
  save_dir = NULL,
  extract = c("estimates", "convergence")
)
```

## Arguments

- ...:

  Named arguments; vectors become grid factors, scalars are repeated.
  `pop_values_x` supports fixed params, `conditions` (bundles of
  dependent params), and cross factors (see `expand_pop_values_x`).

- .labels:

  Optional named list of label vectors for unnamed varying factors:
  `analysis_model`, `fit_options`, `conditions`, `lv_cov`, etc. Example:
  `.labels = list(conditions = c("weak","strong"))`.

- .seed:

  Base seed; `seed = .seed + row_number()` per scenario.

## Value

Tibble with one row per scenario; label columns (e.g. `condition_label`,
`analysis_model_label`) when names or `.labels` are provided.

## Functions

- `run_multifactor()`: Run simulation over design grid (calls
  `run_simulation`).

## See also

Other design:
[`create_modelchoice_design()`](https://jsakaluk.github.io/dySim/reference/create_modelchoice_design.md),
[`expand_pop_values_x()`](https://jsakaluk.github.io/dySim/reference/expand_pop_values_x.md)

## Examples

``` r
dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
  distinguish_1 = "A", distinguish_2 = "B")
#> Error: 'build_dvn' is not an exported object from 'namespace:dySEM'
script <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"))
#> Error: object 'dvn' not found
design <- create_design(
  pop_model_x = "Cor",
  analysis_model = script,
  n_dyads = c(50, 100),
  n_items_x = 3,
  pop_values_x = list(
    intercepts = rep(0, 3),
    coresids = c(.1, .12, .08),
    conditions = list(
      weak = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
                 residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7)),
      strong = list(loadings_p1 = c(.7,.83,.87), loadings_p2 = c(.7,.83,.87),
                   residuals_p1 = c(.49,.31,.24), residuals_p2 = c(.49,.31,.24))
    ),
    lv_cov = list(low = 0.3, med = 0.5, high = 0.8)
  ),
  .seed = 12345
)
#> Error: object 'script' not found
```
