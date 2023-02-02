test_that("saveParamLAPIM returns error for errant paramType", {
  expect_error(saveParamLAPIM(paramType = "schmeasurement"))
})

test_that("saveParamLAPIM returns correct output for paramType = structural ", {

  popModList <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "weak", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  set.seed(123)

  X_A_loads <- generatePopLoadsErrors(var = "X", partner = "A", type = popModList$loadValuesX_A, num = popModList$nItemsX)
  X_B_loads <- generatePopLoadsErrors(var = "X", partner = "B", type = popModList$loadValuesX_B, num = popModList$nItemsX)

  Y_A_loads <- generatePopLoadsErrors(var = "Y", partner = "A", type = popModList$loadValuesY_A, num = popModList$nItemsY)
  Y_B_loads <- generatePopLoadsErrors(var = "Y", partner = "B", type = popModList$loadValuesY_B, num = popModList$nItemsY)

  X_rescorrs <- generatePopResidCorrs(var = "X", type = popModList$residCorrValuesX, num = popModList$nItemsX)
  Y_rescorrs <- generatePopResidCorrs(var = "Y", type = popModList$residCorrValuesY, num = popModList$nItemsY)

  XA_LatentVariance <- generateLVariances(var = "X", partner = "A", value = 1)
  XB_LatentVariance <- generateLVariances(var = "X", partner = "B", value = 1)
  YA_LatentVariance <- generateLVariances(var = "Y", partner = "A", value = 1)
  YB_LatentVariance <- generateLVariances(var = "Y", partner = "B", value = 1)

  X_ICC <- generateICCs(var = "X", type = popModList$iccX)
  Y_ICC <- generateICCs(var = "Y", type = popModList$iccY)

  Actor_A <- generateActors(partner = "A", type = popModList$actorA)
  Actor_B <- generateActors(partner = "B", type = popModList$actorB)

  Partner_A <- generatePartners(partner = "A", type = popModList$partnerA)
  Partner_B <- generatePartners(partner = "B", type = popModList$partnerB)

  paramList <- list(X_A_loads = X_A_loads, X_B_loads = X_B_loads, Y_A_loads = Y_A_loads, Y_B_loads = Y_B_loads,
                    X_rescorrs = X_rescorrs, Y_rescorrs = Y_rescorrs,
                    XA_LatentVariance = XA_LatentVariance, XB_LatentVariance = XB_LatentVariance, YA_LatentVariance = YA_LatentVariance, YB_LatentVariance = YB_LatentVariance,
                    X_ICC = X_ICC, Y_ICC = Y_ICC,
                    Actor_A = Actor_A, Actor_B = Actor_B, Partner_A = Partner_A, Partner_B = Partner_B)

  expect_equal(saveParamLAPIM(paramList, paramType = "structural"),
               structure(list(sim_num = c(0, 0, 0, 0, 0, 0), pop_model = c("L_APIM",
                                                                           "L_APIM", "L_APIM", "L_APIM", "L_APIM", "L_APIM"), samp_model = c(NA,
                                                                                                                                             NA, NA, NA, NA, NA), samp_n = c(NA, NA, NA, NA, NA, NA), lhs = c("YA",
                                                                                                                                                                                                              "YB", "YA", "YB", "k1", "k2"), op = c("~", "~", "~", "~", ":=",
                                                                                                                                                                                                                                                    ":="), rhs = c("XA", "XB", "XB", "XA", "p1/a1", "p2/a2"), label = c("a1",
                                                                                                                                                                                                                                                                                                                        "a2", "p1", "p2", "k1", "k2"), est = c(0.17, 0.39, 0.01, 0.05,
                                                                                                                                                                                                                                                                                                                                                               0.0588235294117647, 0.128205128205128), se = c(NA, NA, NA, NA,
                                                                                                                                                                                                                                                                                                                                                                                                              NA, NA), z = c(NA, NA, NA, NA, NA, NA), pvalue = c(NA, NA, NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                 NA, NA, NA), ci.lower = c(NA, NA, NA, NA, NA, NA), ci.upper = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 NA, NA, NA, NA, NA), std.lv = c(NA, NA, NA, NA, NA, NA), std.all = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NA, NA, NA, NA, NA), std.nox = c(NA, NA, NA, NA, NA, NA)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               -6L), class = "data.frame"))
})
