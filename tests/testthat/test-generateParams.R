#### generateObsNames####
test_that("generateObsNames produces correct output", {
  expect_equal(generateObsNames(lv = "X", person = "A", items = 3), list("X_A1", "X_A2", "X_A3"))
})

#### generateLVNames####
test_that("generateLVNames() produces the correct output", {
  expect_equal(generateLVNames(lv = "X", person = "A"), "X_A")
  expect_equal(generateLVNames(lv = "Y", person = "B"), "Y_B")
})

#### generatePopLoadsErrors####

test_that("generatePopLoadsErrors() produces the correct output for weak", {
  set.seed(123)
  expect_equal(generatePopLoadsErrors(var = "X", partner = "A", type = "weak", num = 5)$loadings, "X_A =~ 0.35*X_A1 + 0.45*X_A2 + 0.38*X_A3 + 0.47*X_A4 + 0.48*X_A5")
  set.seed(123)
  expect_equal(generatePopLoadsErrors(var = "X", partner = "A", type = "weak", num = 5)$errors, "X_A1 ~~ 0.8775*X_A1\nX_A2 ~~ 0.7975*X_A2\nX_A3 ~~ 0.8556*X_A3\nX_A4 ~~ 0.7791*X_A4\nX_A5 ~~ 0.7696*X_A5")
})

test_that("generatePopLoadsErrors() produces the correct output for strong", {
  set.seed(123)
  expect_equal(generatePopLoadsErrors(var = "X", partner = "A", type = "strong", num = 5)$loadings, "X_A =~ 0.74*X_A1 + 0.82*X_A2 + 0.76*X_A3 + 0.83*X_A4 + 0.84*X_A5")
  set.seed(123)
  expect_equal(generatePopLoadsErrors(var = "X", partner = "A", type = "strong", num = 5)$errors, "X_A1 ~~ 0.4524*X_A1\nX_A2 ~~ 0.3276*X_A2\nX_A3 ~~ 0.4224*X_A3\nX_A4 ~~ 0.3111*X_A4\nX_A5 ~~ 0.2944*X_A5")
})

test_that("generatePopLoadsErrors() produces the correct output for very strong", {
  set.seed(123)
  expect_equal(generatePopLoadsErrors(var = "X", partner = "A", type = "very strong", num = 5)$loadings, "X_A =~ 0.9*X_A1 + 0.96*X_A2 + 0.91*X_A3 + 0.97*X_A4 + 0.98*X_A5")
  set.seed(123)
  expect_equal(generatePopLoadsErrors(var = "X", partner = "A", type = "very strong", num = 5)$errors, "X_A1 ~~ 0.19*X_A1\nX_A2 ~~ 0.0784*X_A2\nX_A3 ~~ 0.1719*X_A3\nX_A4 ~~ 0.0591*X_A4\nX_A5 ~~ 0.0396000000000001*X_A5")
})

test_that("generatePopLoadsErrors() produces error for errant type", {
  expect_error(generatePopLoadsErrors(var = "X", partner = "A", type = "yuge", num = 5))
})
#### generatePopCFMLoadsErrors####
test_that("generatePopCFMLoadsErrors() produces the correct output for weak_weak", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "weak_weak")$loadings, "X_Dy =~ 0.35*X_A + 0.45*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "weak_weak")$errors, "X_A ~~ 0.8775*X_A\nX_B ~~ 0.7975*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for weak_moderate", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "weak_moderate")$loadings, "X_Dy =~ 0.35*X_A + 0.65*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "weak_moderate")$errors, "X_A ~~ 0.8775*X_A\nX_B ~~ 0.5775*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for weak_very strong", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "weak_very strong")$loadings, "X_Dy =~ 0.35*X_A + 0.96*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "weak_very strong")$errors, "X_A ~~ 0.8775*X_A\nX_B ~~ 0.0784*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for moderate_weak strong", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "moderate_weak")$loadings, "X_Dy =~ 0.55*X_A + 0.45*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "moderate_weak")$errors, "X_A ~~ 0.6975*X_A\nX_B ~~ 0.7975*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for moderate_moderate strong", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "moderate_moderate")$loadings, "X_Dy =~ 0.55*X_A + 0.65*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "moderate_moderate")$errors, "X_A ~~ 0.6975*X_A\nX_B ~~ 0.5775*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for moderate_strong strong", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "moderate_strong")$loadings, "X_Dy =~ 0.55*X_A + 0.82*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "moderate_strong")$errors, "X_A ~~ 0.6975*X_A\nX_B ~~ 0.3276*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for moderate_very strong strong", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "moderate_very strong")$loadings, "X_Dy =~ 0.55*X_A + 0.96*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "moderate_very strong")$errors, "X_A ~~ 0.6975*X_A\nX_B ~~ 0.0784*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for strong_weak", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "strong_weak")$loadings, "X_Dy =~ 0.74*X_A + 0.45*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "strong_weak")$errors, "X_A ~~ 0.4524*X_A\nX_B ~~ 0.7975*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for strong_moderate", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "strong_moderate")$loadings, "X_Dy =~ 0.74*X_A + 0.65*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "strong_moderate")$errors, "X_A ~~ 0.4524*X_A\nX_B ~~ 0.5775*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for strong_very strong", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "strong_very strong")$loadings, "X_Dy =~ 0.74*X_A + 0.96*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "strong_very strong")$errors, "X_A ~~ 0.4524*X_A\nX_B ~~ 0.0784*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for very strong_weak", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "very strong_weak")$loadings, "X_Dy =~ 0.9*X_A + 0.45*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "very strong_weak")$errors, "X_A ~~ 0.19*X_A\nX_B ~~ 0.7975*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for very strong_moderate", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "very strong_moderate")$loadings, "X_Dy =~ 0.9*X_A + 0.65*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "very strong_moderate")$errors, "X_A ~~ 0.19*X_A\nX_B ~~ 0.5775*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for very strong_strong", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "very strong_strong")$loadings, "X_Dy =~ 0.9*X_A + 0.82*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "very strong_strong")$errors, "X_A ~~ 0.19*X_A\nX_B ~~ 0.3276*X_B")
})

test_that("generatePopCFMLoadsErrors() produces the correct output for very strong_very strong", {
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "very strong_very strong")$loadings, "X_Dy =~ 0.9*X_A + 0.96*X_B")
  set.seed(123)
  expect_equal(generatePopCFMLoadsErrors(var = "X", type = "very strong_very strong")$errors, "X_A ~~ 0.19*X_A\nX_B ~~ 0.0784*X_B")
})

test_that("generatePopCFMLoadsErrors() produces error with errant type", {
  expect_error(generatePopCFMLoadsErrors(var = "X", type = "yuge_yuge"))
})
#### generatePopResidCorrs####
test_that("generatePopResidCorrs produces correct output for weak", {
  set.seed(123)
  expect_equal(generatePopResidCorrs(var = "X", type = "weak", num = 5)$rescors, "X_A1 ~~ 0.04*X_B1\nX_A2 ~~ 0.08*X_B2\nX_A3 ~~ 0.05*X_B3\nX_A4 ~~ 0.09*X_B4\nX_A5 ~~ 0.09*X_B5")
})


test_that("generatePopResidCorrs produces correct output for strong", {
  set.seed(123)
  expect_equal(generatePopResidCorrs(var = "X", type = "strong", num = 5)$rescors, "X_A1 ~~ 0.24*X_B1\nX_A2 ~~ 0.28*X_B2\nX_A3 ~~ 0.25*X_B3\nX_A4 ~~ 0.29*X_B4\nX_A5 ~~ 0.29*X_B5")
})

test_that("generatePopResidCorrs produces correct output for very strong", {
  set.seed(123)
  expect_equal(generatePopResidCorrs(var = "X", type = "very strong", num = 5)$rescors, "X_A1 ~~ 0.34*X_B1\nX_A2 ~~ 0.39*X_B2\nX_A3 ~~ 0.35*X_B3\nX_A4 ~~ 0.4*X_B4\nX_A5 ~~ 0.4*X_B5")
})

test_that("generatePopResidCorrs produces error for errant type", {
  set.seed(123)
  expect_error(generatePopResidCorrs(var = "X", type = "yuge", num = 5))
})
#### generateICCs####
test_that("generateICCs produces correct output for weak", {
  set.seed(123)
  expect_equal(generateICCs(var = "X", type = "weak")$icc, "X_A ~~ 0.04*X_B")
})

test_that("generateICCs produces correct output for moderate", {
  set.seed(123)
  expect_equal(generateICCs(var = "X", type = "moderate")$icc, "X_A ~~ 0.14*X_B")
})

test_that("generateICCs produces correct output for very strong", {
  set.seed(123)
  expect_equal(generateICCs(var = "X", type = "very strong")$icc, "X_A ~~ 0.34*X_B")
})
#### generateIPCs####
test_that("generateIPCs produces correct output for weak", {
  set.seed(123)
  expect_equal(generateIPCs(var = "A", type = "weak")$ipc, "X_A ~~ 0.04*Y_A")
})

test_that("generateIPCs produces correct output for strong", {
  set.seed(123)
  expect_equal(generateIPCs(var = "A", type = "strong")$ipc, "X_A ~~ 0.24*Y_A")
})
#### generateLVariances####
test_that("generateIPCs produces correct output", {
  expect_equal(generateLVariances(var = "X", value = 1, partner = "A"), "X_A ~~ 1*X_A")
})

#### generateActors####
test_that("generateActors produces correct output for weak", {
  set.seed(123)
  expect_equal(generateActors(partner = "A", type = "weak")$actorEffect, "Y_A ~ 0.04*X_A")
})

test_that("generateActors produces correct output for strong", {
  set.seed(123)
  expect_equal(generateActors(partner = "A", type = "strong")$actorEffect, "Y_A ~ 0.24*X_A")
})
#### generatePartners####
test_that("generatePartners produces correct output for weak", {
  set.seed(123)
  expect_equal(generatePartners(partner = "A", type = "weak")$partnerEffect, "Y_A ~ 0.04*X_B")
})

test_that("generatePartners produces correct output for moderate", {
  set.seed(123)
  expect_equal(generatePartners(partner = "A", type = "moderate")$partnerEffect, "Y_A ~ 0.14*X_B")
})

test_that("generatePartners produces correct output for strong", {
  set.seed(123)
  expect_equal(generatePartners(partner = "A", type = "strong")$partnerEffect, "Y_A ~ 0.24*X_B")
})

test_that("generatePartners produces correct output for very strong", {
  set.seed(123)
  expect_equal(generatePartners(partner = "A", type = "very strong")$partnerEffect, "Y_A ~ 0.34*X_B")
})
#### generatePopCFMDySlopeError####
test_that("generatePopCFMDySlopeError produces correct output for weak", {
  set.seed(123)
  expect_equal(generatePopCFMDySlopeError(type = "weak")$slope, "Y_Dy ~ 0.04*X_Dy")
  set.seed(123)
  expect_equal(generatePopCFMDySlopeError(type = "weak")$residual, "Y_Dy ~~ 0.9984*Y_Dy")
})

test_that("generatePopCFMDySlopeError produces correct output for moderate", {
  set.seed(123)
  expect_equal(generatePopCFMDySlopeError(type = "moderate")$slope, "Y_Dy ~ 0.14*X_Dy")
  set.seed(123)
  expect_equal(generatePopCFMDySlopeError(type = "moderate")$residual, "Y_Dy ~~ 0.9804*Y_Dy")
})

test_that("generatePopCFMDySlopeError produces correct output for very strong", {
  set.seed(123)
  expect_equal(generatePopCFMDySlopeError(type = "very strong")$slope, "Y_Dy ~ 0.34*X_Dy")
  set.seed(123)
  expect_equal(generatePopCFMDySlopeError(type = "very strong")$residual, "Y_Dy ~~ 0.8844*Y_Dy")
})

test_that("generatePopCFMDySlopeError produces correct error for errant type", {
  expect_error(generatePopCFMDySlopeError(type = "yuge"))
})
