#' Run Power Analysis
#'
#' Convenience wrapper for power-focused designs. Summarizes rejection rates.
#'
#' @param design Design from \code{create_design()}; must include \code{target_param} and \code{alpha}.
#' @param target_param Parameter label for power (e.g. \code{"XA~~XB"} for latent covariance).
#'   Use \code{paste0(lvname, dist)} for latent names — e.g. \code{"XA"} and \code{"XB"} for defaults.
#' @param alpha Significance level.
#' @param reps,parallel,workers,save_dir Passed to \code{run_simulation}.
#' @return Tibble with raw results; use \code{calc_power()} for rejection rates.
#' @family simulation
#' @export
run_power_analysis <- function(design,
                              target_param = NULL,
                              alpha = 0.05,
                              reps = 100,
                              parallel = FALSE,
                              workers = NULL,
                              save_dir = NULL) {
  res <- run_simulation(design = design, reps = reps, parallel = parallel,
    workers = workers, save_dir = save_dir,
    extract = c("estimates", "convergence"))
  if (!is.null(target_param)) {
    attr(res, "target_param") <- target_param
    attr(res, "alpha") <- alpha
  }
  res
}
