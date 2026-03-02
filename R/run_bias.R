#' Run Bias Analysis
#'
#' Convenience wrapper for bias/RMSE/coverage designs. The design's
#' \code{analysis_model} can include \code{"ICC_LMM"} to compare ICC estimates
#' (from dyLMM) alongside dySEM-based models; ICC is mapped to \code{XA~~XB}
#' for bias and coverage. ICC_LMM requires non-negative \code{lv_cov} in the
#' population values.
#'
#' @param design Design from \code{create_design()}; include \code{true_values} for coverage.
#' @param true_values Named list of true parameter values (fixed across scenarios).
#'   Ignored when \code{true_param_map} is provided.
#' @param true_param_map Named list mapping parameter labels to design column names.
#'   Use when true values vary by scenario (e.g. \code{list("XA~~XB" = "lv_cov")}).
#'   Each scenario gets \code{list(param = design[i, col])} for coverage and bias.
#' @param extract Character vector: \code{"estimates"}, \code{"convergence"}, \code{"coverage"}.
#'   For bias/RMSE only (faster), use \code{c("estimates", "convergence")} to skip CI
#'   computation via \code{performance::icc}; add \code{"coverage"} when coverage is needed.
#' @param reps,parallel,workers,save_dir Passed to \code{run_simulation}.
#' @return Tibble with raw results; use \code{summarize_bias_by()}, \code{calc_coverage()} for summaries.
#' @family simulation
#' @export
run_bias_analysis <- function(design,
                             true_values = NULL,
                             true_param_map = NULL,
                             extract = c("estimates", "convergence", "coverage"),
                             reps = 100,
                             parallel = FALSE,
                             workers = NULL,
                             save_dir = NULL) {
  if (is.list(design) && !inherits(design, "data.frame")) design <- tibble::as_tibble(design)

  if (!is.null(true_param_map)) {
    if (!is.list(true_param_map) || is.null(names(true_param_map))) {
      stop("`true_param_map` must be a named list (param_label = design_column_name).")
    }
    design$true_values <- lapply(seq_len(nrow(design)), function(i) {
      vals <- list()
      for (j in seq_along(true_param_map)) {
        param_lbl <- names(true_param_map)[j]
        col <- true_param_map[[j]]
        if (!col %in% names(design)) {
          stop("`true_param_map` column \"", col, "\" not found in design.")
        }
        v <- design[[col]][i]
        if (is.list(v) && length(v) == 1 && !inherits(v, "data.frame")) v <- v[[1]]
        vals[[param_lbl]] <- as.numeric(v)[1]
      }
      vals
    })
  } else if (!is.null(true_values)) {
    design$true_values <- rep(list(true_values), nrow(design))
  }

  run_simulation(design = design, reps = reps, parallel = parallel,
    workers = workers, save_dir = save_dir,
    extract = extract)
}
