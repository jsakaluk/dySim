#' Create Model Choice Simulation Design
#'
#' Build a design grid for model choice analysis: multiple DGPs (\code{pop_model_x})
#' with per-DGP population parameters, and a fixed set of candidate analysis
#' models fit to each generated dataset.
#'
#' @details
#' **Model order:** \code{analysis_models} must be ordered simplest (most
#' constrained) first. For \code{chi2_diff_startsimple} and
#' \code{chi2_diff_startcomplex}, the LRT compares nested models; if the complex
#' model is listed first, the test logic is wrong. Example for loading invariance:
#' \code{list(loading_inv = script_load, configural = script_none)}.
#'
#' @param analysis_models Named list of lavaan scripts (length >= 2). Names become
#'   \code{analysis_model_labels}. Order: most constrained (simplest) first.
#' @param pop_values_x Named list keyed by \code{pop_model_x}. Each element is
#'   \code{expand_pop_values_x}-compatible (fixed, conditions, cross factors).
#'   Uni structure: \code{loadings}, \code{residuals}, \code{coresids}. Cor
#'   structure: \code{loadings_p1}, \code{loadings_p2}, \code{residuals_p1},
#'   \code{residuals_p2}, \code{coresids}, \code{lv_cov}.
#' @param pop_model_x Character vector of DGP levels (e.g. \code{c("Uni", "Cor")}).
#'   Names of \code{pop_values_x} must contain all of these.
#' @param pop_generator Optional named vector mapping \code{pop_model_x} to
#'   \code{"Uni"} or \code{"Cor"} for data generation. If \code{NULL}, inferred
#'   from parameter names (\code{loadings} -> Uni, \code{loadings_p1} -> Cor).
#' @param ... Other factors (e.g. \code{n_dyads}, \code{n_items_x}) to expand.
#' @param .labels Optional named list for \code{expand_pop_values_x} labels
#'   (e.g. \code{list(conditions = c("weak","strong"))}).
#' @param .seed Base seed; \code{seed = .seed + row_number()} per scenario.
#' @return Tibble with one row per scenario: \code{analysis_models} list-column,
#'   \code{analysis_model_labels}, \code{pop_model_x}, \code{pop_values_x},
#'   \code{pop_generator}, and other design columns.
#' @family design
#' @export
#' @examples
#' dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim2 = "_",
#'   distinguish_1 = "A", distinguish_2 = "B")
#' script_uni <- dySEM::scriptUni(dvn, scaleset = "FF", lvname = "X",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = "none")
#' script_cor <- dySEM::scriptCor(dvn, scaleset = "FF", lvname = "X",
#'   constr_dy_meas = c("loadings", "intercepts", "residuals"),
#'   constr_dy_struct = c("variances", "means"))
#' design <- create_modelchoice_design(
#'   pop_model_x = c("Uni", "Cor"),
#'   analysis_models = list(uni = script_uni, cor = script_cor),
#'   pop_values_x = list(
#'     Uni = list(loadings = c(.75,.8,.55), residuals = c(.44,.36,.7),
#'                coresids = c(.1,.12,.08)),
#'     Cor = list(loadings_p1 = c(.75,.8,.55), loadings_p2 = c(.75,.8,.55),
#'                residuals_p1 = c(.44,.36,.7), residuals_p2 = c(.44,.36,.7),
#'                coresids = c(.1,.12,.08), lv_cov = 0.3)
#'   ),
#'   n_dyads = c(50, 100),
#'   n_items_x = 3,
#'   .seed = 12345
#' )
create_modelchoice_design <- function(analysis_models,
                                     pop_values_x,
                                     pop_model_x,
                                     pop_generator = NULL,
                                     ...,
                                     .labels = NULL,
                                     .seed = NULL) {
  if (!is.list(analysis_models) || length(analysis_models) < 2) {
    stop("`analysis_models` must be a named list of length >= 2.")
  }
  if (is.null(names(analysis_models)) || any(!nzchar(names(analysis_models)))) {
    stop("`analysis_models` must have names (used as analysis_model_labels).")
  }
  if (!is.list(pop_values_x) || is.null(names(pop_values_x))) {
    stop("`pop_values_x` must be a named list keyed by pop_model_x.")
  }
  missing_pmx <- setdiff(pop_model_x, names(pop_values_x))
  if (length(missing_pmx) > 0) {
    stop("`pop_values_x` missing entries for pop_model_x: ", paste(missing_pmx, collapse = ", "))
  }

  dots <- list(...)
  lbl <- if (is.null(.labels)) list() else .labels

  # Infer pop_generator from parameter names when NULL
  infer_generator <- function(pv) {
    if (!is.list(pv)) return("Cor")
    if ("loadings" %in% names(pv) && !"loadings_p1" %in% names(pv)) "Uni" else "Cor"
  }

  # For each pop_model_x, expand its pop_values and bind
  pop_blocks <- list()
  for (pm in pop_model_x) {
    pv <- pop_values_x[[pm]]
    expanded <- expand_pop_values_x(pv, labels = lbl)
    expanded$pop_model_x <- pm
    gen <- if (!is.null(pop_generator) && pm %in% names(pop_generator)) {
      pop_generator[[pm]]
    } else {
      infer_generator(expanded$pop_values_x[[1]])
    }
    expanded$pop_generator <- gen
    pop_blocks[[pm]] <- expanded
  }
  pop_block <- dplyr::bind_rows(pop_blocks)

  # Other factors from dots
  if (length(dots) > 0) {
    other_grid <- do.call(tidyr::expand_grid, dots)
    grid <- tidyr::expand_grid(pop_block, other_grid)
  } else {
    grid <- pop_block
  }

  # Add analysis_models as list-column (same for all rows)
  grid$analysis_models <- rep(list(analysis_models), nrow(grid))
  grid$analysis_model_labels <- rep(list(names(analysis_models)), nrow(grid))

  # Reorder so analysis columns are early
  am_cols <- c("analysis_models", "analysis_model_labels")
  other_cols <- setdiff(names(grid), am_cols)
  grid <- grid[, c(am_cols, other_cols)]

  if (!is.null(.seed)) {
    grid$seed <- .seed + seq_len(nrow(grid))
  }

  grid
}
