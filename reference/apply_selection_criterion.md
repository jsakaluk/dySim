# Apply a single model selection criterion

Apply a single model selection criterion

## Usage

``` r
apply_selection_criterion(
  criterion,
  fits,
  labels,
  alpha,
  delta_cfi_cutoff = 0.01,
  delta_rmsea_cutoff = 0.015,
  delta_srmr_cutoff = 0.03,
  vote_criteria = NULL
)
```

## Arguments

- criterion:

  Name of criterion

- fits:

  List of fitted lavaan objects (ordered simplest to most complex)

- labels:

  Character vector of model names

- alpha:

  Alpha for chi2_diff

- delta_cfi_cutoff:

  Cutoff for delta_cfi (default 0.01)

- delta_rmsea_cutoff:

  Cutoff for delta_rmsea (default 0.015)

- delta_srmr_cutoff:

  Cutoff for delta_srmr (default 0.03)

- vote_criteria:

  Character vector of criteria for majority_vote

## Value

Character: chosen model label
