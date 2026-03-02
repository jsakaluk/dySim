# Generate data from unidimensional dyadic CFA population model.

Generate data from unidimensional dyadic CFA population model.

## Usage

``` r
generate_dyadic_uni(dvn, lvname, pop_values, n_dyads, seed = NULL)
```

## Arguments

- dvn:

  Dvn list from `dySEM::build_dvn()`.

- lvname:

  Latent variable name.

- pop_values:

  List with `loadings`, `residuals`, `coresids` (length = n_items).

- n_dyads:

  Number of dyads.

- seed:

  Integer seed.
