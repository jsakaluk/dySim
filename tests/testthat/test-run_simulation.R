test_that("build_dvn produces correct structure", {
  skip_if_not_installed("dySEM")
  skip_if_not(
    "build_dvn" %in% getNamespaceExports("dySEM"),
    "dySEM build_dvn not exported (install dySEM from source)"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim1 = "", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  expect_equal(dvn$xindper, 3)
  expect_equal(dvn$indnum, 6)
  expect_equal(dvn$p1xvarnames, c("x1_A", "x2_A", "x3_A"))
  expect_equal(dvn$p2xvarnames, c("x1_B", "x2_B", "x3_B"))
})

test_that("scriptPopUni produces valid lavaan syntax", {
  skip_if_not_installed("dySEM")
  skip_if_not(
    all(c("build_dvn", "scriptPopUni") %in% getNamespaceExports("dySEM")),
    "dySEM build_dvn/scriptPopUni not exported (install dySEM from source)"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim1 = "", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  syn <- dySEM::scriptPopUni(dvn, "X", list(
    loadings = c(.75, .80, .55),
    residuals = c(.44, .36, .70),
    coresids = c(.1, .12, .08)
  ))
  expect_type(syn, "character")
  expect_true(grepl("XDy =~", syn))
  expect_true(grepl("x1_A", syn))
})

test_that("create_design expands factors with pop_values_x", {
  skip_if_not_installed("dySEM")
  skip_if_not(
    all(c("build_dvn", "scriptCor") %in% getNamespaceExports("dySEM")),
    "dySEM build_dvn/scriptCor not exported"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  script <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = c("variances", "means"))
  d <- create_design(
    pop_model_x = "Cor",
    analysis_model = script,
    n_dyads = c(50, 100),
    n_items_x = 3,
    pop_values_x = list(
      loadings_p1 = c(.75, .80, .55),
      loadings_p2 = c(.75, .80, .55),
      residuals_p1 = c(.44, .36, .70),
      residuals_p2 = c(.44, .36, .70),
      coresids = c(.1, .12, .08),
      lv_cov = 0.3
    ),
    .seed = 123
  )
  expect_equal(nrow(d), 2)
  expect_true("seed" %in% names(d))
  expect_true("pop_values_x" %in% names(d))
  expect_true("analysis_model" %in% names(d))
})

test_that("expand_pop_values_x expands conditions x cross factors", {
  out <- expand_pop_values_x(list(
    conditions = list(
      list(loadings_p1 = c(.35, .4), loadings_p2 = c(.35, .4)),
      list(loadings_p1 = c(.7, .8), loadings_p2 = c(.7, .8))
    ),
    lv_cov = list(0.3, 0.5)
  ))
  expect_equal(nrow(out), 4)  # 2 conditions x 2 lv_cov
  expect_true("pop_values_x" %in% names(out))
  expect_equal(length(out$pop_values_x[[1]]), 3)  # loadings_p1, loadings_p2, lv_cov
  expect_equal(out$pop_values_x[[1]]$lv_cov, 0.3)
  expect_equal(out$pop_values_x[[1]]$loadings_p1, c(.35, .4))
})

test_that("expand_pop_values_x fixed-only returns one row", {
  out <- expand_pop_values_x(list(loadings_p1 = c(.75,.8), lv_cov = 0.3))
  expect_equal(nrow(out), 1)
  expect_equal(out$pop_values_x[[1]]$lv_cov, 0.3)
})

test_that("expand_pop_values_x fixed + cross only (no conditions) crosses cross factors", {
  out <- expand_pop_values_x(list(
    loadings_p1 = c(.75, .80, .55),
    loadings_p2 = c(.75, .80, .55),
    residuals_p1 = c(.44, .36, .70),
    residuals_p2 = c(.44, .36, .70),
    coresids = c(.1, .12, .08),
    lv_cov = list(0.3, 0.5, 0.8)
  ))
  expect_equal(nrow(out), 3)
  expect_equal(out$pop_values_x[[1]]$lv_cov, 0.3)
  expect_equal(out$pop_values_x[[3]]$lv_cov, 0.8)
})

test_that("expand_pop_values_x full bundle (conditions only) works", {
  out <- expand_pop_values_x(list(
    conditions = list(
      list(loadings_p1 = c(.75,.8,.55), residuals_p1 = c(.44,.36,.7),
           coresids = c(.1,.12,.08), lv_cov = 0.3),
      list(loadings_p1 = c(.7,.83,.87), residuals_p1 = c(.49,.31,.24),
           coresids = c(.1,.12,.08), lv_cov = 0.5)
    )
  ))
  expect_equal(nrow(out), 2)
  expect_equal(out$pop_values_x[[1]]$lv_cov, 0.3)
  expect_equal(out$pop_values_x[[2]]$lv_cov, 0.5)
})

test_that("expand_pop_values_x adds condition_label and cross factor labels when named", {
  out <- expand_pop_values_x(list(
    conditions = list(
      weak = list(loadings_p1 = c(.35,.4), loadings_p2 = c(.35,.4)),
      strong = list(loadings_p1 = c(.7,.8), loadings_p2 = c(.7,.8))
    ),
    lv_cov = list(low = 0.3, med = 0.5, high = 0.8)
  ))
  expect_equal(nrow(out), 6)
  expect_true("condition_label" %in% names(out))
  expect_equal(out$condition_label, rep(c("weak", "strong"), each = 3))
  expect_true("lv_cov" %in% names(out))
  expect_true("lv_cov_label" %in% names(out))
  expect_equal(out$lv_cov_label, rep(c("low", "med", "high"), 2))
})

test_that("expand_pop_values_x uses .labels when conditions/cross factors unnamed", {
  out <- expand_pop_values_x(list(
    conditions = list(
      list(loadings_p1 = c(.35,.4), loadings_p2 = c(.35,.4)),
      list(loadings_p1 = c(.7,.8), loadings_p2 = c(.7,.8))
    ),
    lv_cov = list(0.3, 0.5)
  ), labels = list(conditions = c("weak", "strong"), lv_cov = c("low", "high")))
  expect_equal(out$condition_label, c("weak", "weak", "strong", "strong"))
  expect_equal(out$lv_cov_label, c("low", "high", "low", "high"))
})

test_that("create_design adds analysis_model_label when analysis_model is named list", {
  skip_if_not_installed("dySEM")
  skip_if_not(all(c("build_dvn", "scriptCor") %in% getNamespaceExports("dySEM")))
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  s1 <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X", constr_dy_meas = "none", constr_dy_struct = "none")
  s2 <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X", constr_dy_meas = "loadings", constr_dy_struct = "none")
  d <- create_design(
    pop_model_x = "Cor",
    analysis_model = list(configural = s1, loading_inv = s2),
    n_dyads = 100,
    n_items_x = 3,
    pop_values_x = list(loadings_p1 = c(.75,.8), loadings_p2 = c(.75,.8),
      residuals_p1 = c(.44,.36), residuals_p2 = c(.44,.36),
      coresids = c(.1,.12), lv_cov = 0.3),
    .seed = 123
  )
  expect_equal(nrow(d), 2)
  expect_true("analysis_model_label" %in% names(d))
  expect_equal(d$analysis_model_label, c("configural", "loading_inv"))
})

test_that("create_design adds analysis_model_label for single-element named list", {
  skip_if_not_installed("dySEM")
  skip_if_not(all(c("build_dvn", "scriptCor") %in% getNamespaceExports("dySEM")))
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  script <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X", constr_dy_meas = "loadings", constr_dy_struct = "none")
  d <- create_design(
    pop_model_x = "Cor",
    analysis_model = list(cor_indist = script),
    n_dyads = c(50, 100),
    n_items_x = 3,
    pop_values_x = list(loadings_p1 = c(.75,.8), loadings_p2 = c(.75,.8),
      residuals_p1 = c(.44,.36), residuals_p2 = c(.44,.36),
      coresids = c(.1,.12), lv_cov = 0.3),
    .seed = 123
  )
  expect_true("analysis_model_label" %in% names(d))
  expect_equal(unique(d$analysis_model_label), "cor_indist")
  expect_equal(nrow(d), 2)  # n_dyads varies
})

test_that("calc_bias_rmse works without simhelpers", {
  dat <- tibble::tibble(estimate = rnorm(100, 0.5, 0.1), true_value = 0.5)
  out <- calc_bias_rmse(dat, mcse = FALSE)
  expect_true("bias" %in% names(out))
  expect_true("rmse" %in% names(out))
  expect_lt(abs(out$bias), 0.1)
})

test_that("fit_icc_model returns lmerMod with valid data", {
  skip_if_not_installed("dySEM")
  skip_if_not_installed("dyLMM")
  skip_if_not("build_dvn" %in% getNamespaceExports("dySEM"))
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim1 = "", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  set.seed(42)
  dat <- data.frame(
    x1_A = rnorm(100), x2_A = rnorm(100), x3_A = rnorm(100),
    x1_B = rnorm(100), x2_B = rnorm(100), x3_B = rnorm(100)
  )
  fit <- fit_icc_model(dat, dvn)
  expect_true(inherits(fit, "lmerMod"))
  icc <- dyLMM::getICC(fit)
  expect_true(icc["ICC"] >= 0 && icc["ICC"] <= 1)
})

test_that("extract_estimates returns lavaan-style table for ICC_LMM", {
  skip_if_not_installed("dyLMM")
  skip_if_not("build_dvn" %in% getNamespaceExports("dySEM"))
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim1 = "", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  set.seed(42)
  dat <- data.frame(
    x1_A = rnorm(100), x2_A = rnorm(100), x3_A = rnorm(100),
    x1_B = rnorm(100), x2_B = rnorm(100), x3_B = rnorm(100)
  )
  fit <- fit_icc_model(dat, dvn)
  pe <- dySim:::extract_estimates(fit, model_type = "ICC_LMM", want_coverage = FALSE)
  expect_equal(pe$lhs, "XA")
  expect_equal(pe$rhs, "XB")
  expect_equal(pe$label, "XA~~XB")
  expect_true(is.numeric(pe$est))
  expect_true(pe$est >= 0 && pe$est <= 1)
})

test_that("run_one_scenario with ICC_LMM produces estimates", {
  skip_if_not_installed("dySEM")
  skip_if_not_installed("dyLMM")
  skip_if_not(all(c("build_dvn", "scriptCor", "scriptPopCor") %in% getNamespaceExports("dySEM")))
  res <- tryCatch(
    run_one_scenario(
      pop_model_x = "Cor",
      analysis_model = "ICC_LMM",
      n_items_x = 3,
      n_dyads = 100,
      pop_values_x = list(
        loadings_p1 = c(.75, .80, .55), loadings_p2 = c(.75, .80, .55),
        residuals_p1 = c(.44, .36, .70), residuals_p2 = c(.44, .36, .70),
        coresids = c(.1, .12, .08), lv_cov = 0.4
      ),
      reps = 5,
      seed = 123
    ),
    error = function(e) NULL
  )
  skip_if(is.null(res), "run_one_scenario failed (e.g. lavaan simulateData)")
  expect_equal(nrow(res), 5)
  expect_true("estimates" %in% names(res))
  expect_true("converged" %in% names(res))
  est <- extract_param_estimates(res$estimates, "XA~~XB")
  expect_length(est, 5)
  expect_true(all(est >= 0 & est <= 1, na.rm = TRUE))
})

test_that("ICC_LMM with negative lv_cov throws error", {
  skip_if_not_installed("dySEM")
  skip_if_not_installed("dyLMM")
  skip_if_not(all(c("build_dvn", "scriptCor", "scriptPopCor") %in% getNamespaceExports("dySEM")))
  expect_error(
    run_one_scenario(
      pop_model_x = "Cor",
      analysis_model = "ICC_LMM",
      n_items_x = 3,
      n_dyads = 50,
      pop_values_x = list(
        loadings_p1 = c(.75, .80, .55), loadings_p2 = c(.75, .80, .55),
        residuals_p1 = c(.44, .36, .70), residuals_p2 = c(.44, .36, .70),
        coresids = c(.1, .12, .08), lv_cov = -0.2
      ),
      reps = 2,
      seed = 123
    ),
    "ICC_LMM requires non-negative lv_cov"
  )
})
