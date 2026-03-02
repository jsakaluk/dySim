# Plot Simulation Results

Visualization helpers for raw simulation output.

## Usage

``` r
plot_bias(data, x = "n_dyads", y = "bias", param = NULL)

plot_coverage(data, x = "n_dyads", param = NULL)

plot_power(data, x = "n_dyads", alpha = 0.05, param = NULL)
```

## Arguments

- data:

  Tibble of raw results (from `run_simulation` or `run_one_scenario`).

- x:

  Variable for x-axis (e.g. `n_dyads`).

- y:

  Variable for y-axis (e.g. `bias`, `coverage`).

- param:

  Parameter to filter (from estimates).

## Value

A `ggplot2` object (if ggplot2 installed).

## See also

Other visualization:
[`plot_modelchoice()`](https://jsakaluk.github.io/dySim/reference/plot_modelchoice.md)
