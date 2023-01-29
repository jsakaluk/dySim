test_that("saveParamDf returns error for errant paramType", {
  expect_error(saveParamDf(paramType = "schmeasurement"))
})
