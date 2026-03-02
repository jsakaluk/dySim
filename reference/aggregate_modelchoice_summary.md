# Aggregate raw model choice results to sensitivity/specificity summary Preserves scenario-level design factors (n_dyads, etc.) when present.

Aggregate raw model choice results to sensitivity/specificity summary
Preserves scenario-level design factors (n_dyads, etc.) when present.

## Usage

``` r
aggregate_modelchoice_summary(raw, selection_criteria)
```

## Arguments

- raw:

  Raw results from run_modelchoice_analysis

- selection_criteria:

  Character vector of criteria

## Value

Tibble with pop_model_x, criterion, sensitivity, specificity, n_reps,
n_converged, plus design columns (n_dyads, etc.) when present in raw
