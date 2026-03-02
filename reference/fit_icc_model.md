# Fit an ICC Model via dyLMM

Build composites from wide dyadic data, reshape to long format, and fit
a one-way random effects model to estimate the intraclass correlation
(ICC). Used internally when `model = "ICC_LMM"` in `fit_dyadic_model`.

## Usage

``` r
fit_icc_model(dat, dvn)
```

## Arguments

- dat:

  Data frame with indicator variables (e.g., x1_A, x2_A, x1_B, x2_B).

- dvn:

  Dvn from `dySEM::build_dvn()` or
  [`dySEM::scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.html)
  with `p1xvarnames` and `p2xvarnames`.

## Value

Fitted `lmerMod` object from
[`dyLMM::fitICC`](https://rdrr.io/pkg/dyLMM/man/fitICC.html), or `NULL`
on failure.

## See also

Other analysis:
[`fit_dyadic_model()`](https://jsakaluk.github.io/dySim/reference/fit_dyadic_model.md)
