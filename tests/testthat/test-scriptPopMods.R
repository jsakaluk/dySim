test_that("scriptPopModAPIM produces correct output", {

  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "weak", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")
  set.seed(123)
  expect_equal(scriptPopModAPIM(popModList = popModList.apim),
               "#Measurement Model\n\n## Loadings\nX_A =~ 0.55*X_A1 + 0.65*X_A2 + 0.58*X_A3 + 0.67*X_A4 + 0.68*X_A5\nX_B =~ 0.51*X_B1 + 0.6*X_B2 + 0.67*X_B3 + 0.6*X_B4 + 0.59*X_B5\nY_A =~ 0.48*Y_A1 + 0.39*Y_A2 + 0.43*Y_A3 + 0.41*Y_A4 + 0.32*Y_A5\nY_B =~ 0.92*Y_B1 + 0.47*Y_B2 + 0.33*Y_B3 + 0.53*Y_B4 + 0.96*Y_B5\n\n## Uniquenesses\nX_A1 ~~ 0.6975*X_A1\nX_A2 ~~ 0.5775*X_A2\nX_A3 ~~ 0.6636*X_A3\nX_A4 ~~ 0.5511*X_A4\nX_A5 ~~ 0.5376*X_A5\nX_B1 ~~ 0.7399*X_B1\nX_B2 ~~ 0.64*X_B2\nX_B3 ~~ 0.5511*X_B3\nX_B4 ~~ 0.64*X_B4\nX_B5 ~~ 0.6519*X_B5\nY_A1 ~~ 0.7696*Y_A1\nY_A2 ~~ 0.8479*Y_A2\nY_A3 ~~ 0.8151*Y_A3\nY_A4 ~~ 0.8319*Y_A4\nY_A5 ~~ 0.8976*Y_A5\nY_B1 ~~ 0.1536*Y_B1\nY_B2 ~~ 0.7791*Y_B2\nY_B3 ~~ 0.8911*Y_B3\nY_B4 ~~ 0.7191*Y_B4\nY_B5 ~~ 0.0784*Y_B5\n\n## Residual Correlations\nX_A1 ~~ 0.19*X_B1\nX_A2 ~~ 0.17*X_B2\nX_A3 ~~ 0.17*X_B3\nX_A4 ~~ 0.2*X_B4\nX_A5 ~~ 0.17*X_B5\nY_A1 ~~ 0.29*Y_B1\nY_A2 ~~ 0.23*Y_B2\nY_A3 ~~ 0.25*Y_B3\nY_A4 ~~ 0.13*Y_B4\nY_A5 ~~ 0.07*Y_B5\n\n# Structural Model\n                        \n## Latent (Co)Variances\nX_A ~~ 1*X_A\nX_B ~~ 1*X_B\nY_A ~~ 1*Y_A\nY_B ~~ 1*Y_B\nX_A ~~ 0.1*X_B\nY_A ~~ 0.29*Y_B\n\n## Actor and Partner Effects\nY_A ~ 0.17*X_A\nY_B ~ 0.39*X_B\nY_A ~ 0.01*X_B\nY_B ~ 0.05*X_A")
})

test_that("scriptPopModCFM produces correct output", {

  popModList.cfm <- list(nItemsX = 5,
                         loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                         loadDyX = "weak_strong",
                         residCorrValuesX = "moderate",
                         nItemsY = 7,
                         loadValuesY_A = "moderate", loadValuesY_B = "mixed",
                         loadDyY= "strong_strong",
                         residCorrValuesY = "mixed",
                         residCorrA = "moderate", residCorrB = "very strong",
                         dyEffect = "strong")
  set.seed(123)
  expect_equal(scriptPopModCFM(popModList = popModList.cfm),
               "#Measurement Model\n\n## Lower-Order Loadings\nX_A =~ 0.55*X_A1 + 0.65*X_A2 + 0.58*X_A3 + 0.67*X_A4 + 0.68*X_A5\nX_B =~ 0.51*X_B1 + 0.6*X_B2 + 0.67*X_B3 + 0.6*X_B4 + 0.59*X_B5\nY_A =~ 0.63*Y_A1 + 0.61*Y_A2 + 0.52*Y_A3 + 0.67*Y_A4 + 0.55*Y_A5 + 0.51*Y_A6 + 0.56*Y_A7\nY_B =~ 0.96*Y_B1 + 0.91*Y_B2 + 0.78*Y_B3 + 0.74*Y_B4 + 0.99*Y_B5 + 0.75*Y_B6 + 0.79*Y_B7\n\n## Higher-Order Loadings\nX_Dy =~ 0.48*X_A + 0.77*X_B\nY_Dy =~ 0.78*Y_A + 0.79*Y_B\n\n## Uniquenesses\nX_A1 ~~ 0.6975*X_A1\nX_A2 ~~ 0.5775*X_A2\nX_A3 ~~ 0.6636*X_A3\nX_A4 ~~ 0.5511*X_A4\nX_A5 ~~ 0.5376*X_A5\nX_B1 ~~ 0.7399*X_B1\nX_B2 ~~ 0.64*X_B2\nX_B3 ~~ 0.5511*X_B3\nX_B4 ~~ 0.64*X_B4\nX_B5 ~~ 0.6519*X_B5\nY_A1 ~~ 0.6031*Y_A1\nY_A2 ~~ 0.6279*Y_A2\nY_A3 ~~ 0.7296*Y_A3\nY_A4 ~~ 0.5511*Y_A4\nY_A5 ~~ 0.6975*Y_A5\nY_A6 ~~ 0.7399*Y_A6\nY_A7 ~~ 0.6864*Y_A7\nY_B1 ~~ 0.0784*Y_B1\nY_B2 ~~ 0.1719*Y_B2\nY_B3 ~~ 0.3916*Y_B3\nY_B4 ~~ 0.4524*Y_B4\nY_B5 ~~ 0.0199*Y_B5\nY_B6 ~~ 0.4375*Y_B6\nY_B7 ~~ 0.3759*Y_B7\n\n## Residual Correlations\nX_A1 ~~ 0.14*X_B1\nX_A2 ~~ 0.12*X_B2\nX_A3 ~~ 0.2*X_B3\nX_A4 ~~ 0.19*X_B4\nX_A5 ~~ 0.17*X_B5\nY_A1 ~~ 0.33*Y_B1\nY_A2 ~~ 0.02*Y_B2\nY_A3 ~~ 0.2*Y_B3\nY_A4 ~~ 0.31*Y_B4\nY_A5 ~~ 0.1*Y_B5\nY_A6 ~~ 0.14*Y_B6\nY_A7 ~~ 0.1*Y_B7\n\n# Structural Model\n\n## Latent (Co)Variances\nX_A ~~ 0.7696*X_A\nX_B ~~ 0.4071*X_B\nY_A ~~ 0.3916*Y_A\nY_B ~~ 0.3759*Y_B\nX_Dy ~~ 1*X_Dy\nY_Dy ~~ 0.9516*Y_Dy\nX_A ~~ 0.15*Y_A\nX_B ~~ 0.35*Y_B\n\n## Dyadic Effect\nY_Dy ~ 0.22*X_Dy")
})
