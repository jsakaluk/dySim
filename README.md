
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dySim <a href="https://jsakaluk.github.io/dySim/"><img src="man/figures/logo.png" align="right" height="128" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jsakaluk/dySim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jsakaluk/dySim/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `dySim` is to simplify the process of running a variety of
simulations for the purpose of evaluating methods of dyadic data
analysis. `dySim` piggybacks heavily on the `lavaan`, `simsem`, and
`dySEM` packages.

The `dySim` logo was designed by the *amazing* Lowell Deranleau (for
logo design inquiries, email: <agangofwolves@gmail.com>).

## Installation

You can install the development version of dySim from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jsakaluk/dySim")
```

## Collaboration and Troubleshooting

Please submit any problems or feature requests via the `dySim`
[issues](https://github.com/jsakaluk/dySim/issues) page.

If you are interested in collaborating on the development of `dySim`,
please contact Dr. Sakaluk.

## General Workflow

1.  Identify population model/data generating process from which you
    wish to draw simulated random samples

2.  Save a `popModList` with the arguments appropriate for the
    population model you have chosen in Step 1

3.  Run `runDySim()` with your chosen model in Step 1 and its
    `popModList`, as well as indicating your preferences for features of
    your analysis of each simulated random sample, and what your
    analytic target is for the simulation. Behind the scenes, `dySim`
    will specify your population model’s parameters, feed it to `simsem`
    for generating random samples from this population, which will be
    analyzed using a combination of `dySEM` (for sample model scripting)
    and `lavaan` (for sample model fitting).

4.  Summarize, analyze, and vizualize the data frame returned by
    `runDySim()` in order to make sense of your simulations!

## Supported Population Models/Data Generating Processes

- Latent Actor-Partner Interdependence Model
- Latent Common Fate Model

Each kind of parameter in each model accepts a `type =` argument
(specified in the `popModList`) in order to control what kinds of values
are specified.

## Supported Sample Models

- Latent Actor-Partner Interdependence Model
- Latent Common Fate Model
- Observed Actor-Partner Interdependence Model
- Observed Common Fate Model

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dySim)

#set population parameters for model in a list with features
#needed for a latent APIM data generating mechanism
popModList.apim <- list(nItemsX = 5, #five items for measure of latent x 
                   loadValuesX_A = "moderate", #A's X-loadings are moderate
                   loadValuesX_B = "moderate", #B's X-loadings are moderate
                   residCorrValuesX = "moderate", #residual correlations between A's and B'x X-indicators
                   iccX = "weak", #intraclass correlation between A and B's latent-Xs
                   nItemsY = 5, #five items for measure of latent y 
                   loadValuesY_A = "weak", #A's Y-loadings are moderate
                   loadValuesY_B = "mixed", #B's X-loadings are moderate
                   residCorrValuesY = "mixed", #residual correlations between A's and B'x X-indicators
                   iccY = "strong",# residual intraclass correlation between A and B's latent-Xs
                   actorA = "moderate", #actor effect for A 
                   actorB = "very strong", #actor effect for B
                   partnerA = "weak",  #partner effect for A
                   partnerB = "weak") #partner effect for B

#run simulation and save combined parameter table from all 5000 sims in a df
lapim.lapim.100 <- runDySim(seed = 123,#set random seed for reproducibility
                    popMod = "L-APIM", #generate from latent APIM pop. mod
                    popModList = popModList.apim, #supply list of model parameters
                    sampSize = 100, # n for each random sample
                    sampMod = "APIM", # what model to fit in each random sample
                    sampModType = "latent", #whether to use "latent" or "observed" version of model to fit in each random sample
                    nSims = 5000, #number of random samples to draw/fit model in
                    output = "paramTable") #what kind of output to extract from each sample's analysis
```
