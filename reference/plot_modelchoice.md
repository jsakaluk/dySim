# Plot Model Choice Sensitivity and Specificity

Plot sensitivity and specificity from `run_modelchoice_analysis` output.

## Usage

``` r
plot_modelchoice(data, x = "n_dyads", facets = "criterion")
```

## Arguments

- data:

  Output from `run_modelchoice_analysis`, either `raw` or `summary`.

- x:

  Variable for x-axis (e.g. `"n_dyads"`, `"pop_model_x"`).

- facets:

  Character vector of variables to facet by (e.g. `"criterion"` or
  `c("criterion", "n_dyads")`). Pass `NULL` for no faceting.

## Value

A `ggplot2` object.

## See also

Other visualization:
[`plot_bias()`](https://jsakaluk.github.io/dySim/reference/plot_bias.md)

## Examples

``` r
# Single facet
plot_modelchoice(results, x = "pop_model_x", facets = "criterion")
#> Error: object 'results' not found

# Two facets (criterion and n_dyads)
plot_modelchoice(results, x = "pop_model_x", facets = c("criterion", "n_dyads"))
#> Error: object 'results' not found
```
