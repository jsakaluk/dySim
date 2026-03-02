#' Fit an ICC Model via dyLMM
#'
#' Build composites from wide dyadic data, reshape to long format, and fit a
#' one-way random effects model to estimate the intraclass correlation (ICC).
#' Used internally when \code{model = "ICC_LMM"} in \code{fit_dyadic_model}.
#'
#' @param dat Data frame with indicator variables (e.g., x1_A, x2_A, x1_B, x2_B).
#' @param dvn Dvn from \code{dySEM::build_dvn()} or \code{dySEM::scrapeVarCross()}
#'   with \code{p1xvarnames} and \code{p2xvarnames}.
#' @return Fitted \code{lmerMod} object from \code{dyLMM::fitICC}, or \code{NULL}
#'   on failure.
#' @family analysis
#' @export
fit_icc_model <- function(dat, dvn) {
  if (!requireNamespace("dyLMM", quietly = TRUE)) {
    stop("Package 'dyLMM' is required for ICC_LMM analysis.")
  }
  if (is.null(dvn) || is.null(dvn$p1xvarnames) || is.null(dvn$p2xvarnames)) {
    stop("`dvn` must have `p1xvarnames` and `p2xvarnames`.")
  }
  dat_comp <- dyLMM::build_composites(dat, dvn = dvn, x_name = "x")
  dat_long <- dyLMM::wide_to_long(dat_comp, y_p1 = "x_p1", y_p2 = "x_p2")
  fit <- tryCatch(
    suppressWarnings(dyLMM::fitICC(dat_long, y = "y", dyad_id = "dyad_id")),
    error = function(e) NULL
  )
  fit
}

#' Fit Dyadic CFA Models to Data
#'
#' Fit models using a lavaan script (from dySEM or hand-written) or via
#' \code{model_type} for convenience. Supports passing lavaan options via
#' \code{fit_options}. When \code{model = "ICC_LMM"}, fits an ICC model via
#' \code{dyLMM::fitICC} instead of lavaan (returns \code{lmerMod}, not lavaan).
#'
#' @param dat Data frame with indicator variables (e.g., x1_A, x2_A, x1_B, x2_B).
#' @param model Character; lavaan model syntax (from \code{dySEM::scriptUni()},
#'   \code{dySEM::scriptCor()}, or hand-written). When provided, used directly.
#'   Use \code{"ICC_LMM"} to fit an intraclass correlation model via dyLMM.
#' @param model_type \code{"Uni"} or \code{"Cor"}; used when \code{model} is
#'   \code{NULL} to generate script via dySEM.
#' @param dvn Dvn from \code{dySEM::build_dvn()} or \code{dySEM::scrapeVarCross()}.
#'   If \code{NULL}, built from \code{dat} using \code{scrapeVarCross}.
#' @param x_order,x_stem,x_delim1,x_delim2,distinguish_1,distinguish_2 Naming
#'   arguments for \code{scrapeVarCross} when \code{dvn} is \code{NULL}.
#' @param lvname Latent variable name (for \code{model_type} path).
#' @param fit_options Named list passed to \code{lavaan::cfa()} (e.g.
#'   \code{list(estimator = "mlr", missing = "ml")}). Ignored for \code{ICC_LMM}.
#' @param ... Passed to \code{lavaan::cfa()} (ignored for \code{ICC_LMM}).
#' @return Fitted lavaan object (or \code{lmerMod} for \code{ICC_LMM}), or
#'   \code{NULL} if model did not converge.
#' @family analysis
#' @export
fit_dyadic_model <- function(dat,
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
                             ...) {
  if (is.null(dvn)) {
    dvn <- dySEM::scrapeVarCross(
      dat = dat,
      x_order = x_order,
      x_stem = x_stem,
      x_delim1 = x_delim1,
      x_delim2 = x_delim2,
      distinguish_1 = distinguish_1,
      distinguish_2 = distinguish_2,
      verbose = FALSE
    )
  }

  if (!is.null(model) && is.character(model) && length(model) == 1 &&
      nchar(trimws(model)) > 0 && trimws(model) == "ICC_LMM") {
    return(fit_icc_model(dat = dat, dvn = dvn))
  }

  if (!is.null(model) && is.character(model) && length(model) == 1 && nchar(model) > 0) {
    script <- model
  } else {
    model_type <- match.arg(model_type)
    script <- if (model_type == "Uni") {
      dySEM::scriptUni(dvn, scaleset = "FF", lvname = lvname,
        constr_dy_meas = c("loadings", "intercepts", "residuals"),
        constr_dy_struct = "none")
    } else {
      dySEM::scriptCor(dvn, scaleset = "FF", lvname = lvname,
        constr_dy_meas = c("loadings", "intercepts", "residuals"),
        constr_dy_struct = c("variances", "means"))
    }
  }

  cfa_args <- c(list(model = script, data = dat), list(...))
  if (is.list(fit_options) && length(fit_options) > 0) {
    cfa_args <- c(cfa_args, fit_options)
  }

  fit <- tryCatch(
    do.call(lavaan::cfa, cfa_args),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(fit) && !lavaan::lavInspect(fit, "converged")) {
    fit <- NULL
  }
  fit
}

#' @describeIn fit_dyadic_model Fit unidimensional dyadic CFA.
#' @export
fit_uni <- function(dat, dvn = NULL, x_order = "sip", x_stem = "x", x_delim1 = "",
  x_delim2 = "_", distinguish_1 = "A", distinguish_2 = "B", lvname = "X",
  fit_options = NULL, ...) {
  fit_dyadic_model(dat, model_type = "Uni", dvn = dvn,
    x_order = x_order, x_stem = x_stem, x_delim1 = x_delim1, x_delim2 = x_delim2,
    distinguish_1 = distinguish_1, distinguish_2 = distinguish_2, lvname = lvname,
    fit_options = fit_options, ...)
}

#' @describeIn fit_dyadic_model Fit correlated dyadic CFA.
#' @export
fit_cor <- function(dat, dvn = NULL, x_order = "sip", x_stem = "x", x_delim1 = "",
  x_delim2 = "_", distinguish_1 = "A", distinguish_2 = "B", lvname = "X",
  fit_options = NULL, ...) {
  fit_dyadic_model(dat, model_type = "Cor", dvn = dvn,
    x_order = x_order, x_stem = x_stem, x_delim1 = x_delim1, x_delim2 = x_delim2,
    distinguish_1 = distinguish_1, distinguish_2 = distinguish_2, lvname = lvname,
    fit_options = fit_options, ...)
}
