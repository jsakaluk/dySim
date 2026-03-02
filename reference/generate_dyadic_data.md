# Generate Dyadic Data from Population Models

Generate Dyadic Data from Population Models

## Usage

``` r
generate_dyadic_data(pop_syntax, n_dyads, seed = NULL)
```

## Arguments

- pop_syntax:

  Character string of lavaan population model syntax.

- n_dyads:

  Number of dyads (rows) to generate.

- seed:

  Integer seed. If `NULL`, no seed is set.

## Value

Data frame with generated indicator variables.
