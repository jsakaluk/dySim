#' Create Simulation Design Grid
#'
#' Expand design factors into a tibble for simulation. Use named lists for
#' \code{analysis_model}, \code{fit_options}, and \code{pop_values_x} elements
#' (e.g. \code{conditions}, cross factors) to get readable label columns for
#' grouping and summarization.
#'
#' @param ... Named arguments; vectors become grid factors, scalars are repeated.
#'   \code{pop_values_x} supports fixed params, \code{conditions} (bundles of
#'   dependent params), and cross factors (see \code{expand_pop_values_x}).
#' @param .labels Optional named list of label vectors for unnamed varying
#'   factors: \code{analysis_model}, \code{fit_options}, \code{conditions},
#'   \code{lv_cov}, etc. Example: \code{.labels = list(conditions = c("weak","strong"))}.
#' @param .seed Base seed; \code{seed = .seed + row_number()} per scenario.
#' @return Tibble with one row per scenario; label columns (e.g.
#'   \code{condition_label}, \code{analysis_model_label}) when names or
#'   \code{.labels} are provided.
#' @family design
#' @export
#' @examples
#' dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
#'   distinguish_1 = "A", distinguish_2 = "B")
#' script <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = c("variances", "means"))
#' design <- create_design(
#'   pop_model_x = "Cor",
#'   analysis_model = script,
#'   n_dyads = c(50, 100),
#'   n_items_x = 3,
#'   pop_values_x = list(
#'     intercepts = rep(0, 3),
#'     coresids = c(.1, .12, .08),
#'     conditions = list(
#'       weak = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
#'                  residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7)),
#'       strong = list(loadings_p1 = c(.7,.83,.87), loadings_p2 = c(.7,.83,.87),
#'                    residuals_p1 = c(.49,.31,.24), residuals_p2 = c(.49,.31,.24))
#'     ),
#'     lv_cov = list(low = 0.3, med = 0.5, high = 0.8)
#'   ),
#'   .seed = 12345
#' )
create_design <- function(..., .labels = NULL, .seed = NULL) {
  dots <- list(...)
  lbl <- if (is.null(.labels)) list() else .labels

  # Helper: build label column for a list factor (including single-element named lists)
  list_factor_tibble <- function(x, label_col, lbl_key) {
    if (!is.list(x) || length(x) == 0) return(NULL)
    lab <- names(x)
    if (is.null(lab) || all(!nzchar(lab))) lab <- lbl[[lbl_key]]
    if (is.null(lab)) {
      if (length(x) <= 1) return(NULL)  # no names/labels and single/empty: no label col
      lab <- paste0(label_col, "_", seq_along(x))
    }
    if (length(lab) != length(x)) stop("Label length for ", lbl_key, " must match.")
    tibble::tibble(!!rlang::sym(sub("_label", "", label_col)) := unname(x),
                  !!rlang::sym(label_col) := lab)
  }

  # Pre-process analysis_model and fit_options when they are varying (list)
  expand_parts <- list()
  if ("analysis_model" %in% names(dots)) {
    am <- dots$analysis_model
    dots <- dots[setdiff(names(dots), "analysis_model")]
    am_tbl <- list_factor_tibble(am, "analysis_model_label", "analysis_model")
    if (!is.null(am_tbl)) {
      expand_parts <- c(expand_parts, list(am_tbl))
    } else {
      dots$analysis_model <- am
    }
  }
  if ("fit_options" %in% names(dots)) {
    fo <- dots$fit_options
    dots <- dots[setdiff(names(dots), "fit_options")]
    # Varying: list of option sets (each element is a list)
    is_varying_fitopts <- is.list(fo) && length(fo) > 1 &&
      all(vapply(fo, is.list, logical(1)))
    fo_tbl <- if (is_varying_fitopts) {
      list_factor_tibble(fo, "fit_options_label", "fit_options")
    } else NULL
    if (!is.null(fo_tbl)) {
      expand_parts <- c(expand_parts, list(fo_tbl))
    } else {
      dots$fit_options <- fo
    }
  }

  # pop_values_x expansion with labels
  if ("pop_values_x" %in% names(dots)) {
    pv <- dots$pop_values_x
    dots <- dots[setdiff(names(dots), "pop_values_x")]
    pv_labels <- lbl[intersect(names(lbl), c("conditions", names(pv)))]
    expanded_pv <- expand_pop_values_x(pv, labels = pv_labels)
    expand_parts <- c(expand_parts, list(expanded_pv))
  }

  # Remaining factors
  if (length(dots) > 0) {
    other <- tidyr::expand_grid(!!!dots)
    expand_parts <- c(expand_parts, list(other))
  }

  grid <- if (length(expand_parts) == 0) {
    tibble::tibble(.rows = 1)
  } else {
    do.call(tidyr::expand_grid, expand_parts)
  }

  if (!is.null(.seed)) {
    grid$seed <- .seed + seq_len(nrow(grid))
  }
  grid
}

#' @describeIn create_design Run simulation over design grid (calls \code{run_simulation}).
#' @export
run_multifactor <- function(design, reps = 100, parallel = FALSE, workers = NULL,
                           save_dir = NULL, extract = c("estimates", "convergence")) {
  run_simulation(design = design, reps = reps, parallel = parallel, workers = workers,
    save_dir = save_dir, extract = extract)
}
