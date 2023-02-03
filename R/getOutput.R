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

getParamsMLM <- function(fit, first = NULL, sampCount = NULL){

  apim_dis_A_out <- summary(fit$apim_dis_A)
  apim_dis_A_conf <- confint(fit$apim_dis_A)

  a1.est <- apim_dis_A_out$tTable[3,1]
  a1.se <- apim_dis_A_out$tTable[3,2]
  a1.test <- apim_dis_A_out$tTable[3,3]
  a1.p <- apim_dis_A_out$tTable[3,4]
  a1.upper <- apim_dis_A_conf[3,2]
  a1.lower <- apim_dis_A_conf[3,1]

  a2.est <- apim_dis_A_out$tTable[2,1]
  a2.se <- apim_dis_A_out$tTable[2,2]
  a2.test <- apim_dis_A_out$tTable[2,3]
  a2.p <- apim_dis_A_out$tTable[2,4]
  a2.upper <- apim_dis_A_conf[2,2]
  a2.lower <- apim_dis_A_conf[2,1]

  apim_dis_B_out <- summary(fit$apim_dis_B)
  apim_dis_B_conf <- confint(fit$apim_dis_B)

  p1.est <- apim_dis_B_out$tTable[3,1]
  p1.se <- apim_dis_B_out$tTable[3,2]
  p1.test <- apim_dis_B_out$tTable[3,3]
  p1.p <- apim_dis_B_out$tTable[3,4]
  p1.upper <- apim_dis_B_conf[3,2]
  p1.lower <- apim_dis_B_conf[3,1]

  p2.est <- apim_dis_B_out$tTable[2,1]
  p2.se <- apim_dis_B_out$tTable[2,2]
  p2.test <- apim_dis_B_out$tTable[2,3]
  p2.p <- apim_dis_B_out$tTable[2,4]
  p2.upper <- apim_dis_B_conf[2,2]
  p2.lower <- apim_dis_B_conf[2,1]

  k1.est <- p1.est/a1.est
  k2.est <- p2.est/a2.est

  lhs <- c("Y_A", "Y_B", "Y_A", "Y_B", "k1", "k2")
  op <- c("~", "~", "~", "~", ":=", ":=")
  rhs <- c("X_A", "X_B", "X_B", "X_A", "p1/a1", "p2/a2")
  label <- c("a1", "a2", "p1", "p2", "k1", "k2")
  est <- c(a1.est, a2.est, p1.est, p2.est, k1.est, k2.est)
  se <- c(a1.se, a2.se, p1.se, p2.se, NA, NA)
  z <- c(a1.test, a2.test, p1.test, p2.test, NA, NA)
  pvalue <- c(a1.p, a2.p, p1.p, p2.p, NA, NA)
  ci.lower <- c(a1.lower, a2.lower, p1.lower, p2.lower, NA, NA)
  ci.upper <- c(a1.upper, a2.upper, p1.upper, p2.upper, NA, NA)

  mlm.cols <- cbind(lhs, op, rhs, label, est, se, z, pvalue, ci.lower, ci.upper)
  mlm.df <- as.data.frame(mlm.cols)

  if(isTRUE(first)){
    params <- mlm.df %>%
      dplyr::mutate(sim_num = 1)
  }else if(!isTRUE(first)){
    sampCount <- sampCount
    params <- mlm.df %>%
      dplyr::mutate(sim_num = sampCount)
  }
  return(params)
}

#Function contributions from Omar Camanto (@omarjcamanto)
getFit <- function(fit, first = NULL, sampCount = NULL){

  if(isTRUE(first)){

    fitMeasures <- lavaan::fitmeasures(fit)
    fitMeasuresTransp <- data.frame(t(data.frame(fitMeasures))) %>%
      dplyr::select(.data$cfi, .data$tli, .data$aic, .data$bic, .data$rmsea, .data$srmr) %>%
      dplyr::mutate(sim_num = 1)
  }else if(!isTRUE(first)){

    sampCount <- sampCount

    fitMeasures <- lavaan::fitmeasures(fit)

    fitMeasuresTransp <- data.frame(t(data.frame(fitMeasures))) %>%
      dplyr::select(.data$cfi, .data$tli, .data$aic, .data$bic, .data$rmsea, .data$srmr) %>%
      dplyr::mutate(sim_num = sampCount)
  }

  return(fitMeasuresTransp)
}
