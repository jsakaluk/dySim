# Extract Parameter Estimates from Simulation Results

Extract point estimates for a named parameter (e.g. `"XA~~XB"`) from a
list of lavaan-style estimate tables. Handles both lhs/rhs orderings for
covariances.

## Usage

``` r
extract_param_estimates(estimates_list, param)
```

## Arguments

- estimates_list:

  List of parameter estimate data frames (from `run_simulation`).

- param:

  Parameter spec: lavaan label, or `"lhs~~rhs"` for covariances.

## Value

Numeric vector of estimates (NA where not found).

## See also

Other performance:
[`calc_bias_rmse()`](https://jsakaluk.github.io/dySim/reference/calc_bias_rmse.md),
[`summarize_bias_by()`](https://jsakaluk.github.io/dySim/reference/summarize_bias_by.md)
