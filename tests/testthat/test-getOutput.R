test_that("getParams returns proper output for first = TRUE", {
  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "strong", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  seed = 125
  popMod = "L-APIM"
  popModList = popModList.apim
  sampSize = 100
  nSims = 4

  set.seed(seed)
  counter <- nSims
  nSims <- nSims#will run one iteration out of loop
  nSimsLess1 <- nSims-1

  popModData <- scriptPopModAPIM(popModList = popModList, paramType = "structural")
  popModScript <- popModData$popModScript

  #sample size determines size of sample for generate
  samp <- simsem::generate(popModScript, n = sampSize)

  dvn <- dySEM::scrapeVarCross(samp,
                               x_order = "spi", x_stem = "X", x_delim1 = "_",
                               y_order = "spi", y_stem = "Y", y_delim1 = "_",
                               distinguish_1 = "A", distinguish_2 = "B")

  sample.script <- dySEM::scriptAPIM(dvn, lvxname = "X", lvyname = "Y", k = TRUE)

  sample.out <- lavaan::cfa(sample.script, data = samp, std.lv = TRUE)

  simCount <- 1

  expect_equal(getParams(fit = sample.out, sampCount = simCount, first = TRUE),
               structure(list(lhs = c("YA", "YB", "YA", "YB", "k1", "k2"), op = c("~",
                                                                                  "~", "~", "~", ":=", ":="), rhs = c("XA", "XB", "XB", "XA", "p1/a1",
                                                                                                                      "p2/a2"), label = c("a1", "a2", "p1", "p2", "k1", "k2"), est = c(0.0241014719232854,
                                                                                                                                                                                       0.603588492003531, 0.187280637593168, 0.0731775563328335, 7.77050622423724,
                                                                                                                                                                                       0.12123749425694), se = c(0.122887423235569, 0.137821674643251,
                                                                                                                                                                                                                 0.124188232846354, 0.129341772245658, 39.8642577174278, 0.215100361738359
                                                                                                                                                                                       ), z = c(0.196126432540489, 4.37948888348592, 1.50803850977469,
                                                                                                                                                                                                0.565768931895009, 0.194924141804355, 0.563632219291242), pvalue = c(0.844511207835859,
                                                                                                                                                                                                                                                                     1.18957989452273e-05, 0.13154467122048, 0.571550868888777, 0.845452334261784,
                                                                                                                                                                                                                                                                     0.573004454921354), ci.lower = c(-0.216753451771361, 0.333462973413762,
                                                                                                                                                                                                                                                                                                      -0.0561238260893604, -0.180327658965239, -70.3620031723441, -0.300351467811781
                                                                                                                                                                                                                                                                     ), ci.upper = c(0.264956395617932, 0.873714010593301, 0.430685101275697,
                                                                                                                                                                                                                                                                                     0.326682771630906, 85.9030156208186, 0.542826456325662), sim_num = c(1,
                                                                                                                                                                                                                                                                                                                                                          1, 1, 1, 1, 1)), row.names = c(NA, -6L), class = c("lavaan.data.frame",
                                                                                                                                                                                                                                                                                                                                                                                                             "data.frame"))
  )
})

test_that("getFit returns proper output for first = TRUE", {

  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "strong", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  seed = 125
  popMod = "L-APIM"
  popModList = popModList.apim
  sampSize = 100
  sampMod = "L-APIM"
  nSims = 200
  output = "modelFit"

  set.seed(seed)
  counter <- nSims
  nSims <- nSims#will run one iteration out of loop
  nSimsLess1 <- nSims-1

  popModData <- scriptPopModAPIM(popModList = popModList, paramType = "structural")
  popModScript <- popModData$popModScript

  samp <- simsem::generate(popModScript, n = sampSize)

  dvn <- dySEM::scrapeVarCross(samp,
                               x_order = "spi", x_stem = "X", x_delim1 = "_",
                               y_order = "spi", y_stem = "Y", y_delim1 = "_",
                               distinguish_1 = "A", distinguish_2 = "B")

  sample.script <- dySEM::scriptAPIM(dvn, lvxname = "X", lvyname = "Y", k = TRUE)

  sample.out <- lavaan::cfa(sample.script, data = samp, std.lv = TRUE)

  simCount <- 1

  expect_equal(getFit(fit = sample.out, sampCount = simCount, first = TRUE),
               structure(list(cfi = 0.926794347268539, tli = 0.9244072064186,
                              aic = 5040.68201415171, bic = 5160.51984270716, rmsea = 0.0581768550482474,
                              srmr = 0.0798786066847033, sim_num = 1), class = "data.frame", row.names = "fitMeasures")
               )
})

test_that("getFit returns proper output for first = FALSE", {

  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "strong", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  seed = 125
  popMod = "L-APIM"
  popModList = popModList.apim
  sampSize = 100
  sampMod = "L-APIM"
  nSims = 200
  output = "modelFit"

  set.seed(seed)
  counter <- nSims
  nSims <- nSims#will run one iteration out of loop
  nSimsLess1 <- nSims-1

  popModData <- scriptPopModAPIM(popModList = popModList, paramType = "structural")
  popModScript <- popModData$popModScript

  samp <- simsem::generate(popModScript, n = sampSize)

  dvn <- dySEM::scrapeVarCross(samp,
                               x_order = "spi", x_stem = "X", x_delim1 = "_",
                               y_order = "spi", y_stem = "Y", y_delim1 = "_",
                               distinguish_1 = "A", distinguish_2 = "B")

  sample.script <- dySEM::scriptAPIM(dvn, lvxname = "X", lvyname = "Y", k = TRUE)

  sample.out <- lavaan::cfa(sample.script, data = samp, std.lv = TRUE)

  simCount <- 2

  expect_equal(getFit(fit = sample.out, sampCount = simCount, first = FALSE),
               structure(list(cfi = 0.926794347268539, tli = 0.9244072064186,
                              aic = 5040.68201415171, bic = 5160.51984270716, rmsea = 0.0581768550482474,
                              srmr = 0.0798786066847033, sim_num = 2), class = "data.frame", row.names = "fitMeasures")
  )
})
