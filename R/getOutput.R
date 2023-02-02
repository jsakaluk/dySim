#' @name getOutput
#' @rdname getOutput
#'
#' @title Helper functions for getting output from sample lavaan models
#'
#' @param fit lavaan object fit to a particular random sample's simulated data
#' @param first logical for whether output is from first random sample (TRUE) or not
#' @param sampCount counter of the current sample
#'
#' @return df of estimated parameter values and their statistical tests
#' @family helpers
#'

getParams <- function(fit, first = NULL, sampCount = NULL){

  if(isTRUE(first)){
    params <- lavaan::parameterestimates(fit, standardized = TRUE) %>%
      dplyr::filter(.data$op == "~"|.data$op == ":=") %>%
      dplyr::mutate(sim_num = 1)
  }else if(!isTRUE(first)){
    sampCount <- sampCount
    params <- lavaan::parameterestimates(fit, standardized = TRUE) %>%
      dplyr::filter(.data$op == "~"|.data$op == ":=") %>%
      dplyr::mutate(sim_num = sampCount)
  }

  return(params)
}
