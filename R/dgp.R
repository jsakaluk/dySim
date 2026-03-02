#' Generate Dyadic Data from Population Models
#'
#' @param pop_syntax Character string of lavaan population model syntax.
#' @param n_dyads Number of dyads (rows) to generate.
#' @param seed Integer seed. If \code{NULL}, no seed is set.
#' @return Data frame with generated indicator variables.
#' @family data-generation
#' @name generate_dyadic_data
NULL

#' @rdname generate_dyadic_data
#' @keywords internal
generate_dyadic_data <- function(pop_syntax, n_dyads, seed = NULL) {
  if (!is.character(pop_syntax) || length(pop_syntax) != 1) {
    stop("`pop_syntax` must be a character string.")
  }
  if (!is.numeric(n_dyads) || length(n_dyads) != 1 || n_dyads < 1) {
    stop("`n_dyads` must be a positive integer.")
  }
  n_dyads <- as.integer(n_dyads)
  if (!is.null(seed)) set.seed(seed)
  lavaan::simulateData(model = pop_syntax, sample.nobs = n_dyads, seed = seed)
}

#' Generate data from unidimensional dyadic CFA population model.
#' @param dvn Dvn list from \code{dySEM::build_dvn()}.
#' @param lvname Latent variable name.
#' @param pop_values List with \code{loadings}, \code{residuals}, \code{coresids} (length = n_items).
#' @param n_dyads Number of dyads.
#' @param seed Integer seed.
#' @export
generate_dyadic_uni <- function(dvn, lvname, pop_values, n_dyads, seed = NULL) {
  pop_syntax <- dySEM::scriptPopUni(dvn, lvname, pop_values)
  generate_dyadic_data(pop_syntax, n_dyads, seed)
}

#' Generate data from correlated dyadic CFA population model.
#' @param pop_values List with \code{loadings_p1}, \code{loadings_p2}, \code{residuals_p1}, \code{residuals_p2}, \code{coresids}, \code{lv_cov}.
#' @param n_dyads Number of dyads.
#' @param seed Integer seed.
#' @export
generate_dyadic_cor <- function(dvn, lvname, pop_values, n_dyads, seed = NULL) {
  pop_syntax <- dySEM::scriptPopCor(dvn, lvname, pop_values)
  generate_dyadic_data(pop_syntax, n_dyads, seed)
}
