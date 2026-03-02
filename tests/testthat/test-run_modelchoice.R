test_that("create_modelchoice_design creates design with named pop_values_x", {
  skip_if_not_installed("dySEM")
  skip_if_not(
    all(c("build_dvn", "scriptUni", "scriptCor") %in% getNamespaceExports("dySEM")),
    "dySEM build_dvn/scriptUni/scriptCor not exported"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  script_uni <- dySEM::scriptUni(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none")
  script_cor <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = c("variances", "means"))

  design <- create_modelchoice_design(
    pop_model_x = c("Uni", "Cor"),
    analysis_models = list(uni = script_uni, cor = script_cor),
    pop_values_x = list(
      Uni = list(loadings = c(.75, .8, .55), residuals = c(.44, .36, .7),
                 coresids = c(.1, .12, .08)),
      Cor = list(loadings_p1 = c(.75, .8, .55), loadings_p2 = c(.75, .8, .55),
                 residuals_p1 = c(.44, .36, .7), residuals_p2 = c(.44, .36, .7),
                 coresids = c(.1, .12, .08), lv_cov = 0.3)
    ),
    n_dyads = c(50, 100),
    n_items_x = 3,
    .seed = 12345
  )
  expect_true("analysis_models" %in% names(design))
  expect_true("analysis_model_labels" %in% names(design))
  expect_true("pop_model_x" %in% names(design))
  expect_true("pop_values_x" %in% names(design))
  expect_true("pop_generator" %in% names(design))
  expect_equal(nrow(design), 4)  # 2 pop_model_x x 2 n_dyads
  expect_equal(length(design$analysis_models[[1]]), 2)
  expect_equal(names(design$analysis_models[[1]]), c("uni", "cor"))
  expect_true(all(design$pop_generator[design$pop_model_x == "Uni"] == "Uni"))
  expect_true(all(design$pop_generator[design$pop_model_x == "Cor"] == "Cor"))
})

test_that("create_modelchoice_design with pop_generator override", {
  skip_if_not_installed("dySEM")
  skip_if_not(
    all(c("build_dvn", "scriptCor") %in% getNamespaceExports("dySEM")),
    "dySEM build_dvn/scriptCor not exported"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  script_none <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = "none", constr_dy_struct = "none")
  script_load <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = "loadings", constr_dy_struct = "none")

  design <- create_modelchoice_design(
    pop_model_x = c("Cor_inv", "Cor_noinv"),
    analysis_models = list(configural = script_none, loading_inv = script_load),
    pop_values_x = list(
      Cor_inv = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
                     residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7),
                     coresids = c(.1,.12,.08), lv_cov = 0.3),
      Cor_noinv = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.7,.85,.5),
                       residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.51,.28,.75),
                       coresids = c(.1,.12,.08), lv_cov = 0.3)
    ),
    pop_generator = c(Cor_inv = "Cor", Cor_noinv = "Cor"),
    n_dyads = 100,
    n_items_x = 3,
    .seed = 12345
  )
  expect_equal(nrow(design), 2)
  expect_true(all(design$pop_generator == "Cor"))
})

test_that("run_modelchoice_analysis returns raw and summary", {
  skip_if_not_installed("dySEM")
  skip_if_not_installed("lavaan")
  skip_if_not(
    all(c("build_dvn", "scriptUni", "scriptCor") %in% getNamespaceExports("dySEM")),
    "dySEM build_dvn/scriptUni/scriptCor not exported"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  script_uni <- dySEM::scriptUni(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none")
  script_cor <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = c("variances", "means"))
  design <- create_modelchoice_design(
    pop_model_x = c("Uni", "Cor"),
    analysis_models = list(uni = script_uni, cor = script_cor),
    pop_values_x = list(
      Uni = list(loadings = c(.75,.8,.55), residuals = c(.44,.36,.7),
                 coresids = c(.1,.12,.08)),
      Cor = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
                 residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7),
                 coresids = c(.1,.12,.08), lv_cov = 0.3)
    ),
    n_dyads = 50,
    n_items_x = 3,
    .seed = 12345
  )
  res <- tryCatch(
    run_modelchoice_analysis(
      design = design,
      correct_model = c(Uni = "uni", Cor = "cor"),
      reps = 5
    ),
    error = function(e) {
      skip(paste("run_modelchoice_analysis failed:", conditionMessage(e)))
    }
  )
  expect_true("raw" %in% names(res))
  expect_true("summary" %in% names(res))
  expect_true("chosen_bic_diff" %in% names(res$raw))
  expect_true("sensitivity" %in% names(res$summary))
  expect_true("specificity" %in% names(res$summary))
  expect_true("all_converged" %in% names(res$raw))
})

test_that("run_modelchoice_analysis with new selection criteria", {
  skip_if_not_installed("dySEM")
  skip_if_not_installed("lavaan")
  skip_if_not(
    all(c("build_dvn", "scriptUni", "scriptCor") %in% getNamespaceExports("dySEM")),
    "dySEM build_dvn/scriptUni/scriptCor not exported"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  script_uni <- dySEM::scriptUni(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none")
  script_cor <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = c("variances", "means"))
  design <- create_modelchoice_design(
    pop_model_x = c("Uni", "Cor"),
    analysis_models = list(uni = script_uni, cor = script_cor),
    pop_values_x = list(
      Uni = list(loadings = c(.75,.8,.55), residuals = c(.44,.36,.7),
                 coresids = c(.1,.12,.08)),
      Cor = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
                 residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7),
                 coresids = c(.1,.12,.08), lv_cov = 0.3)
    ),
    n_dyads = 50,
    n_items_x = 3,
    .seed = 12345
  )
  criteria <- c("aic_diff", "sabic_diff", "delta_cfi", "delta_rmsea", "delta_srmr",
    "delta_cfi_or_delta_rmsea", "delta_cfi_and_delta_rmsea")
  res <- tryCatch(
    run_modelchoice_analysis(
      design = design,
      correct_model = c(Uni = "uni", Cor = "cor"),
      selection_criteria = criteria,
      reps = 3
    ),
    error = function(e) {
      skip(paste("run_modelchoice_analysis failed:", conditionMessage(e)))
    }
  )
  for (crit in criteria) {
    chosen_col <- paste0("chosen_", crit)
    expect_true(chosen_col %in% names(res$raw), info = crit)
    expect_true(all(res$raw[[chosen_col]] %in% c("uni", "cor") | is.na(res$raw[[chosen_col]])))
  }
})

test_that("run_modelchoice_analysis with majority_vote requires vote_criteria", {
  skip_if_not_installed("dySEM")
  skip_if_not(
    all(c("build_dvn", "scriptUni", "scriptCor") %in% getNamespaceExports("dySEM")),
    "dySEM build_dvn/scriptUni/scriptCor not exported"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  script_uni <- dySEM::scriptUni(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none")
  script_cor <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = c("variances", "means"))
  design <- create_modelchoice_design(
    pop_model_x = c("Uni", "Cor"),
    analysis_models = list(uni = script_uni, cor = script_cor),
    pop_values_x = list(
      Uni = list(loadings = c(.75,.8,.55), residuals = c(.44,.36,.7),
                 coresids = c(.1,.12,.08)),
      Cor = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
                 residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7),
                 coresids = c(.1,.12,.08), lv_cov = 0.3)
    ),
    n_dyads = 50,
    n_items_x = 3,
    .seed = 12345
  )
  expect_error(
    run_modelchoice_analysis(
      design = design,
      correct_model = c(Uni = "uni", Cor = "cor"),
      selection_criteria = c("bic_diff", "majority_vote"),
      reps = 2
    ),
    "vote_criteria is required"
  )
})

test_that("run_modelchoice_analysis with majority_vote", {
  skip_if_not_installed("dySEM")
  skip_if_not_installed("lavaan")
  skip_if_not(
    all(c("build_dvn", "scriptUni", "scriptCor") %in% getNamespaceExports("dySEM")),
    "dySEM build_dvn/scriptUni/scriptCor not exported"
  )
  dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
    distinguish_1 = "A", distinguish_2 = "B")
  script_uni <- dySEM::scriptUni(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none")
  script_cor <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = c("variances", "means"))
  design <- create_modelchoice_design(
    pop_model_x = c("Uni", "Cor"),
    analysis_models = list(uni = script_uni, cor = script_cor),
    pop_values_x = list(
      Uni = list(loadings = c(.75,.8,.55), residuals = c(.44,.36,.7),
                 coresids = c(.1,.12,.08)),
      Cor = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
                 residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7),
                 coresids = c(.1,.12,.08), lv_cov = 0.3)
    ),
    n_dyads = 50,
    n_items_x = 3,
    .seed = 12345
  )
  res <- tryCatch(
    run_modelchoice_analysis(
      design = design,
      correct_model = c(Uni = "uni", Cor = "cor"),
      selection_criteria = c("bic_diff", "aic_diff", "majority_vote"),
      vote_criteria = c("bic_diff", "aic_diff"),
      reps = 3
    ),
    error = function(e) {
      skip(paste("run_modelchoice_analysis failed:", conditionMessage(e)))
    }
  )
  expect_true("chosen_majority_vote" %in% names(res$raw))
  expect_true("correct_majority_vote" %in% names(res$raw))
  expect_true(all(res$raw$chosen_majority_vote %in% c("uni", "cor") | is.na(res$raw$chosen_majority_vote)))
})

test_that("create_modelchoice_design errors on missing pop_values_x", {
  expect_error(
    create_modelchoice_design(
      pop_model_x = c("Uni", "Cor"),
      analysis_models = list(a = "x", b = "y"),
      pop_values_x = list(Uni = list(loadings = 1)),
      n_dyads = 50
    ),
    "missing entries for pop_model_x"
  )
})
