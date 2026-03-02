#' Extract Estimates from a Fitted Model
#'
#' Unified extraction of parameter estimates from lavaan or ICC_LMM fits into
#' a lavaan-style table (lhs, rhs, op, label, est, optionally ci.lower/ci.upper).
#' Used internally by \code{run_one_scenario}.
#'
#' @param fit Fitted lavaan or \code{lmerMod} object.
#' @param model_type \code{"lavaan"} or \code{"ICC_LMM"}.
#' @param want_coverage If \code{TRUE}, include confidence intervals for coverage.
#' @return Data frame with lavaan-style columns; \code{NULL} if extraction fails.
#' @keywords internal
extract_estimates <- function(fit, model_type = c("lavaan", "ICC_LMM"), want_coverage = FALSE) {
  model_type <- match.arg(model_type)
  if (model_type == "lavaan") {
    pe <- lavaan::parameterEstimates(fit, ci = want_coverage)
    idx <- pe$op %in% c("=~", "~~", "~1") & !is.na(pe$est)
    return(pe[idx, , drop = FALSE])
  }
  if (model_type == "ICC_LMM") {
    icc <- tryCatch(
      dyLMM::getICC(fit, ci = if (want_coverage) 0.95 else NULL),
      error = function(e) {
        tryCatch(
          {
            vc <- lme4::VarCorr(fit)
            s2_dyad <- as.numeric(vc[[1]][1, 1])
            s2_resid <- as.numeric(attr(vc, "sc")^2)
            tot <- s2_dyad + s2_resid
            c(ICC = if (tot > 0) s2_dyad / tot else NA_real_)
          },
          error = function(e2) c(ICC = NA_real_)
        )
      }
    )
    icc_val <- as.numeric(icc["ICC"])
    pe <- data.frame(
      lhs = "XA",
      rhs = "XB",
      op = "~~",
      label = "XA~~XB",
      est = if (length(icc_val) && !is.na(icc_val)) icc_val else NA_real_,
      ci.lower = if (!is.null(icc["CI_low"]) && !is.na(icc["CI_low"])) as.numeric(icc["CI_low"]) else NA_real_,
      ci.upper = if (!is.null(icc["CI_high"]) && !is.na(icc["CI_high"])) as.numeric(icc["CI_high"]) else NA_real_,
      stringsAsFactors = FALSE
    )
    return(pe)
  }
  NULL
}

#' Extract Parameter Estimates from Simulation Results
#'
#' Extract point estimates for a named parameter (e.g. \code{"XA~~XB"}) from
#' a list of lavaan-style estimate tables. Handles both lhs/rhs orderings for
#' covariances.
#'
#' @param estimates_list List of parameter estimate data frames (from \code{run_simulation}).
#' @param param Parameter spec: lavaan label, or \code{"lhs~~rhs"} for covariances.
#' @return Numeric vector of estimates (NA where not found).
#' @family performance
#' @export
extract_param_estimates <- function(estimates_list, param) {
  parts <- NULL
  if (grepl("~~", param, fixed = TRUE)) {
    parts <- strsplit(param, "~~", fixed = TRUE)[[1]]
    if (length(parts) != 2) parts <- NULL
  }
  vapply(estimates_list, function(pe) {
    if (is.null(pe) || nrow(pe) == 0) return(NA_real_)
    idx <- which(pe$label == param)
    if (length(idx) == 0 && !is.null(parts)) {
      idx <- which((pe$lhs == parts[1] & pe$rhs == parts[2]) |
                  (pe$lhs == parts[2] & pe$rhs == parts[1]))
    }
    if (length(idx) == 0) return(NA_real_)
    est_col <- pe[["est"]]
    if (is.null(est_col)) return(NA_real_)
    as.double(est_col[idx[1]])
  }, NA_real_)
}

#' Calculate Simulation Performance Metrics
#'
#' Compute bias, RMSE, coverage, and power from raw simulation results.
#' Uses \pkg{simhelpers} for MCSE when available.
#'
#' @param data Tibble of raw results with columns for estimates, SEs, etc.
#' @param estimates Name or vector of point estimates.
#' @param true_param Name or vector of true parameter values.
#' @param ses Optional; name or vector of standard errors.
#' @param ci_lower,ci_upper Optional; for coverage calculation.
#' @param mcse If \code{TRUE} (default), use simhelpers for MCSE when available.
#' @return Tibble with performance metrics.
#' @family performance
#' @export
calc_bias_rmse <- function(data,
                           estimates = "estimate",
                           true_param = "true_value",
                           mcse = TRUE) {
  est_col <- if (is.character(estimates)) data[[estimates]] else estimates
  true_col <- if (is.character(true_param)) data[[true_param]] else true_param
  if (is.null(est_col) || is.null(true_col)) {
    stop("`estimates` and `true_param` must identify valid columns or vectors.")
  }
  d <- tibble::tibble(est = est_col, true = true_col)
  n_ok <- sum(!is.na(d$est))
  if (requireNamespace("simhelpers", quietly = TRUE) && mcse && n_ok > 0) {
    simhelpers::calc_absolute(d, estimates = est, true_param = true,
      criteria = c("bias", "variance", "mse", "rmse"))
  } else {
    bias <- if (n_ok == 0) NA_real_ else mean(d$est - d$true, na.rm = TRUE)
    mse <- if (n_ok == 0) NA_real_ else mean((d$est - d$true)^2, na.rm = TRUE)
    var_val <- if (n_ok == 0) NA_real_ else mean((d$est - mean(d$est, na.rm = TRUE))^2, na.rm = TRUE)
    tibble::tibble(
      n_reps = n_ok,
      bias = bias,
      var = var_val,
      mse = mse,
      rmse = if (is.na(mse)) NA_real_ else sqrt(mse)
    )
  }
}

#' @rdname calc_bias_rmse
#' @param ci_lower,ci_upper Column names or vectors for CI bounds.
#' @export
calc_coverage <- function(data,
                         estimates = "estimate",
                         true_param = "true_value",
                         ci_lower = "ci_lower",
                         ci_upper = "ci_upper",
                         mcse = TRUE) {
  if (is.character(ci_lower)) ci_lower <- data[[ci_lower]]
  if (is.character(ci_upper)) ci_upper <- data[[ci_upper]]
  if (is.null(ci_lower) || is.null(ci_upper)) {
    stop("`ci_lower` and `ci_upper` required for coverage.")
  }
  true_col <- if (is.character(true_param)) data[[true_param]] else true_param
  est_col <- if (is.character(estimates)) data[[estimates]] else estimates
  d <- tibble::tibble(
    est = est_col,
    true = true_col,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  if (requireNamespace("simhelpers", quietly = TRUE) && mcse) {
    simhelpers::calc_coverage(d, estimates = "est", true_param = "true",
      lower_bound = "ci_lower", upper_bound = "ci_upper")
  } else {
    covered <- d$true >= d$ci_lower & d$true <= d$ci_upper
    tibble::tibble(
      n_reps = sum(!is.na(covered)),
      coverage = mean(covered, na.rm = TRUE),
      mean_ci_width = mean(d$ci_upper - d$ci_lower, na.rm = TRUE)
    )
  }
}

#' Summarize Bias and RMSE by Design Conditions
#'
#' Compute bias and RMSE for a target parameter, grouped by design factors
#' (e.g. \code{condition_label}, \code{lv_cov_label}, \code{n_dyads}). Use when
#' you want to see how performance metrics vary across simulation conditions.
#'
#' @param data Tibble of raw results from \code{run_bias_analysis} or
#'   \code{run_simulation} (must have \code{estimates} column).
#' @param param Parameter spec (e.g. \code{"XA~~XB"}) passed to
#'   \code{extract_param_estimates}.
#' @param true_param Column name containing true values (e.g. \code{"lv_cov"}
#'   when latent covariance varies), or a scalar for a fixed true value.
#' @param ... Bare column names to group by (e.g. \code{condition_label},
#'   \code{lv_cov_label}, \code{n_dyads}). Uses
#'   \code{\link[dplyr]{group_by}}.
#' @param .converged_only If \code{TRUE} (default), exclude non-converged rows.
#' @param .mcse If \code{TRUE}, use \pkg{simhelpers} for MCSE when available.
#' @return Tibble with group columns plus \code{mean_est}, \code{bias}, \code{var},
#'   \code{mse}, \code{rmse}, \code{n_reps}.
#' @family performance
#' @export
#' @examples
#' # After run_bias_analysis with design that has condition_label, lv_cov, n_dyads:
#' # summarize_bias_by(results_bias, "XA~~XB", true_param = "lv_cov",
#' #   condition_label, lv_cov_label, n_dyads)
summarize_bias_by <- function(data,
                              param,
                              true_param,
                              ...,
                              .converged_only = TRUE,
                              .mcse = FALSE) {
  if (!"estimates" %in% names(data)) {
    stop("`data` must have an `estimates` column (from run_bias_analysis or run_simulation).")
  }
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    stop("Provide at least one grouping variable in `...`.")
  }

  true_col <- if (is.character(true_param) && length(true_param) == 1) {
    if (!true_param %in% names(data)) {
      stop("`true_param` column \"", true_param, "\" not found in data.")
    }
    data[[true_param]]
  } else {
    rep(true_param, nrow(data))
  }

  d <- tibble::tibble(
    est = extract_param_estimates(data$estimates, param),
    true = true_col
  )
  d <- dplyr::bind_cols(data[, setdiff(names(data), "estimates"), drop = FALSE], d)

  if (.converged_only && "converged" %in% names(d)) {
    d <- dplyr::filter(d, .data$converged)
  }

  grp <- dplyr::group_by(d, !!!dots)
  out <- dplyr::group_modify(grp, function(.x, .y) {
    b <- calc_bias_rmse(.x, estimates = "est", true_param = "true", mcse = .mcse)
    n_est <- sum(!is.na(.x$est))
    b$mean_est <- if (n_est == 0) NA_real_ else mean(.x$est, na.rm = TRUE)
    b
  })
  dplyr::ungroup(out)
}

#' @rdname calc_bias_rmse
#' @param pvalues Column name or vector of p-values.
#' @param alpha Significance level for rejection.
#' @export
calc_power <- function(data,
                      pvalues = "pvalue",
                      alpha = 0.05,
                      mcse = TRUE) {
  pv <- if (is.character(pvalues)) data[[pvalues]] else pvalues
  if (is.null(pv)) stop("`pvalues` required.")
  d <- tibble::tibble(pvalue = pv)
  if (requireNamespace("simhelpers", quietly = TRUE) && mcse) {
    simhelpers::calc_rejection(d, p_values = pvalue, alpha = alpha)
  } else {
    rej <- pv < alpha
    tibble::tibble(
      n_reps = sum(!is.na(rej)),
      rejection_rate = mean(rej, na.rm = TRUE)
    )
  }
}
