# Generate data from correlated dyadic CFA population model.

Generate data from correlated dyadic CFA population model.

## Usage

``` r
generate_dyadic_cor(dvn, lvname, pop_values, n_dyads, seed = NULL)
```

## Arguments

- pop_values:

  List with `loadings_p1`, `loadings_p2`, `residuals_p1`,
  `residuals_p2`, `coresids`, `lv_cov`.

- n_dyads:

  Number of dyads.

- seed:

  Integer seed.
