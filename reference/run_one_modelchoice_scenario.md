# Run One Model Choice Scenario

Generate data from one DGP, fit all candidate models, apply selection
criteria, and record chosen model and correctness. Internal use by
`run_modelchoice_analysis`.

## Usage

``` r
run_one_modelchoice_scenario(
  pop_model_x,
  pop_values_x,
  pop_generator,
  analysis_models,
  analysis_model_labels,
  n_items_x,
  n_dyads,
  lvname = "X",
  dvn = NULL,
  correct_label,
  reps = 100,
  seed = NULL,
  selection_criteria = c("chi2_diff_startsimple", "chi2_diff_startcomplex", "bic_diff"),
  alpha = 0.05,
  delta_cfi_cutoff = 0.01,
  delta_rmsea_cutoff = 0.015,
  delta_srmr_cutoff = 0.03,
  vote_criteria = NULL,
  fit_options = NULL
)
```

## Arguments

- pop_model_x:

  DGP label (e.g. "Uni", "Cor").

- pop_values_x:

  Named list of population values for data generation.

- pop_generator:

  `"Uni"` or `"Cor"` — which generator to use.

- analysis_models:

  Named list of lavaan scripts (ordered simplest to most complex).

- analysis_model_labels:

  Character vector of model names.

- n_items_x, n_dyads, lvname, dvn:

  Naming and sample size.

- correct_label:

  The analysis model label considered correct for this DGP.

- reps:

  Number of replications.

- seed:

  Base seed.

- selection_criteria:

  Character vector of criterion names.

- alpha:

  Alpha for chi2_diff variants.

- delta_cfi_cutoff, delta_rmsea_cutoff, delta_srmr_cutoff:

  Cutoffs for delta criteria.

- vote_criteria:

  Character vector of criteria for majority_vote (required when used).

- fit_options:

  Optional list passed to lavaan.

## Value

Tibble with rep, chosen\_, correct, scenario meta.
