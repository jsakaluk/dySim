# Fit Dyadic CFA Models to Data

Fit models using a lavaan script (from dySEM or hand-written) or via
`model_type` for convenience. Supports passing lavaan options via
`fit_options`. When `model = "ICC_LMM"`, fits an ICC model via
[`dyLMM::fitICC`](https://rdrr.io/pkg/dyLMM/man/fitICC.html) instead of
lavaan (returns `lmerMod`, not lavaan).

## Usage

``` r
fit_dyadic_model(
  dat,
  model = NULL,
  model_type = c("Uni", "Cor"),
  dvn = NULL,
  x_order = "sip",
  x_stem = "x",
  x_delim1 = "",
  x_delim2 = "_",
  distinguish_1 = "A",
  distinguish_2 = "B",
  lvname = "X",
  fit_options = NULL,
  ...
)

fit_uni(
  dat,
  dvn = NULL,
  x_order = "sip",
  x_stem = "x",
  x_delim1 = "",
  x_delim2 = "_",
  distinguish_1 = "A",
  distinguish_2 = "B",
  lvname = "X",
  fit_options = NULL,
  ...
)

fit_cor(
  dat,
  dvn = NULL,
  x_order = "sip",
  x_stem = "x",
  x_delim1 = "",
  x_delim2 = "_",
  distinguish_1 = "A",
  distinguish_2 = "B",
  lvname = "X",
  fit_options = NULL,
  ...
)
```

## Arguments

- dat:

  Data frame with indicator variables (e.g., x1_A, x2_A, x1_B, x2_B).

- model:

  Character; lavaan model syntax (from
  [`dySEM::scriptUni()`](https://jsakaluk.github.io/dySEM/reference/scriptUni.html),
  [`dySEM::scriptCor()`](https://jsakaluk.github.io/dySEM/reference/scriptCor.html),
  or hand-written). When provided, used directly. Use `"ICC_LMM"` to fit
  an intraclass correlation model via dyLMM.

- model_type:

  `"Uni"` or `"Cor"`; used when `model` is `NULL` to generate script via
  dySEM.

- dvn:

  Dvn from `dySEM::build_dvn()` or
  [`dySEM::scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.html).
  If `NULL`, built from `dat` using `scrapeVarCross`.

- x_order, x_stem, x_delim1, x_delim2, distinguish_1, distinguish_2:

  Naming arguments for `scrapeVarCross` when `dvn` is `NULL`.

- lvname:

  Latent variable name (for `model_type` path).

- fit_options:

  Named list passed to
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) (e.g.
  `list(estimator = "mlr", missing = "ml")`). Ignored for `ICC_LMM`.

- ...:

  Passed to [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)
  (ignored for `ICC_LMM`).

## Value

Fitted lavaan object (or `lmerMod` for `ICC_LMM`), or `NULL` if model
did not converge.

## Functions

- `fit_uni()`: Fit unidimensional dyadic CFA.

- `fit_cor()`: Fit correlated dyadic CFA.

## See also

Other analysis:
[`fit_icc_model()`](https://jsakaluk.github.io/dySim/reference/fit_icc_model.md)
