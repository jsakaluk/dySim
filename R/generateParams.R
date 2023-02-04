#' @name generateParams
#' @rdname generateParams
#'
#' @title Helper functions for scripters to generate parameter values of particular kinds of
#' parameters for dyadic SEMs using lavaan notation
#'
#' @param lv character for name of LV
#' @param var character for stem of items for a given LV
#' @param partner character designation for a partner
#' @param person character designation for a partner
#' @param type character input for magnitude threshold heuristic for "weak", "moderate", "strong", and "very strong" (for first or lower-order loadings)
#' and any combination therein (e.g., "weak_strong", "very strong_very strong") (for higher-order loadings as in CFM)
#' @param items number of items for which parameters are needed
#' @param num number of items for which parameters are needed
#' @param value numeric value for a given parameter
#'
#' @return vector (or list of  vectors) of lavaan script elements and/or parameter values
#' @family helpers
#'

generateObsNames <- function(lv = "X", person = "A", items){
  #generate indicator names based on var, partner, and num
  varnames <- list()
  varnums <- seq_along(1:items)
  for(i in 1:items){
    varnames[[i]] <- paste(lv, "_", person,varnums[[i]], sep = "")
  }
  return(varnames)
}

#' @rdname generateParams
generateLVNames <- function(lv = "X", person = "A"){
  lvname <- paste(lv, "_", person, sep = "")
  return(lvname)
}

#' @rdname generateParams
generatePopLoadsErrors <- function(var = "X", partner = "A", type = NULL, num = NULL){

  lv <- var
  person <- partner
  items <- num

  varnames <- generateObsNames(lv, person, items)
  #determine loading values for
  if(type == "nil"){
    values <- rep(0, num)

  }else if(type == "weak"){
    #loadings fall between .3 and .49
    values <- stats::runif(n = num, min = .3, max = .49)

  }else if(type == "moderate"){
    values <- stats::runif(n = num, min = .5, max = .69)

  }else if(type == "strong"){
    values <- stats::runif(n = num, min = .7, max = .85)

  }else if(type == "very strong"){
    values <- stats::runif(n = num, min = .86, max = .99)

  }else if(type == "mixed"){
    values <- stats::runif(n = num, min = .3, max = .99)

  }else{
    stop("loadValue arguments can only be nil, weak, moderate, strong, very strong, or mixed")
  }

  #values can be rounded to 2 decimals
  values <- round(values, 2)
  errors <- 1-(values^2)

  #combine pop values and variable names w lavaan constraint operator
  lav_loads <- list()
  for(i in 1:num){
    lav_loads[[i]] <- paste(values[[i]], "*", varnames[[i]], sep = "")
  }

  lav_loads <- paste(lav_loads, collapse = " + ")

  #generate lvName
  lvname <- generateLVNames(lv, person)

  loadScript <- paste(lvname, " =~ ", lav_loads, sep = "")

  resVariances <- list()
  for(i in 1:num){
    resVariances[[i]] <- paste(varnames[[i]], " ~~ ", errors[[i]], "*", varnames[[i]], sep = "")
  }
  resVariances <- paste(resVariances, collapse ="\n")

  varnames <- as.character(varnames)

  loadErrorList <- list(loadings = loadScript,
                        errors = resVariances,
                        varnames = varnames,
                        loadValues = values,
                        errorValues = errors)
  return(loadErrorList)
}

#' @rdname generateParams
generatePopCFMLoadsErrors <- function(var = "X", type = NULL){

  lv <- var

  if(var == "X"){
    lvnames <- c("X_A", "X_B")
  }else if(var == "Y"){
    lvnames <- c("Y_A", "Y_B")
  }

  #determine loading values for
  if(type == "weak_weak"){
    #loadings fall between .3 and .49
    value_A <- stats::runif(n = 1, min = .3, max = .49)
    value_B <- stats::runif(n = 1, min = .3, max = .49)
    values <- c(value_A, value_B)
  }else if(type == "weak_moderate"){
    value_A <- stats::runif(n = 1, min = .3, max = .49)
    value_B <- stats::runif(n = 1, min = .5, max = .69)
    values <- c(value_A, value_B)
  }else if(type == "weak_strong"){
    value_A <- stats::runif(n = 1, min = .3, max = .49)
    value_B <- stats::runif(n = 1, min = .7, max = .85)
    values <- c(value_A, value_B)
  }else if(type == "weak_very strong"){
    value_A <- stats::runif(n = 1, min = .3, max = .49)
    value_B <- stats::runif(n = 1, min = .86, max = .99)
    values <- c(value_A, value_B)
  }else if(type == "moderate_weak"){
    #loadings fall between .3 and .49
    value_A <- stats::runif(n = 1, min = .5, max = .69)
    value_B <- stats::runif(n = 1, min = .3, max = .49)
    values <- c(value_A, value_B)
  }else if(type == "moderate_moderate"){
    value_A <- stats::runif(n = 1, min = .5, max = .69)
    value_B <- stats::runif(n = 1, min = .5, max = .69)
    values <- c(value_A, value_B)
  }else if(type == "moderate_strong"){
    value_A <- stats::runif(n = 1, min = .5, max = .69)
    value_B <- stats::runif(n = 1, min = .7, max = .85)
    values <- c(value_A, value_B)
  }else if(type == "moderate_very strong"){
    value_A <- stats::runif(n = 1, min = .5, max = .69)
    value_B <- stats::runif(n = 1, min = .86, max = .99)
    values <- c(value_A, value_B)
  }else if(type == "strong_weak"){
    #loadings fall between .3 and .49
    value_A <- stats::runif(n = 1, min = .7, max = .85)
    value_B <- stats::runif(n = 1, min = .3, max = .49)
    values <- c(value_A, value_B)
  }else if(type == "strong_moderate"){
    value_A <- stats::runif(n = 1, min = .7, max = .85)
    value_B <- stats::runif(n = 1, min = .5, max = .69)
    values <- c(value_A, value_B)
  }else if(type == "strong_strong"){
    value_A <- stats::runif(n = 1, min = .7, max = .85)
    value_B <- stats::runif(n = 1, min = .7, max = .85)
    values <- c(value_A, value_B)
  }else if(type == "strong_very strong"){
    value_A <- stats::runif(n = 1, min = .7, max = .85)
    value_B <- stats::runif(n = 1, min = .86, max = .99)
    values <- c(value_A, value_B)
  }else if(type == "very strong_weak"){
    #loadings fall between .3 and .49
    value_A <- stats::runif(n = 1, min = .86, max = .99)
    value_B <- stats::runif(n = 1, min = .3, max = .49)
    values <- c(value_A, value_B)
  }else if(type == "very strong_moderate"){
    value_A <- stats::runif(n = 1, min = .86, max = .99)
    value_B <- stats::runif(n = 1, min = .5, max = .69)
    values <- c(value_A, value_B)
  }else if(type == "very strong_strong"){
    value_A <- stats::runif(n = 1, min = .86, max = .99)
    value_B <- stats::runif(n = 1, min = .7, max = .85)
    values <- c(value_A, value_B)
  }else if(type == "very strong_very strong"){
    value_A <- stats::runif(n = 1, min = .86, max = .99)
    value_B <- stats::runif(n = 1, min = .86, max = .99)
    values <- c(value_A, value_B)
  }else{
    stop("loadValue arguments can only be weak_weak, weak_moderate, weak_strong, weak_very strong,
         moderate_weak, moderate_moderate, moderate_strong, moderate_very strong,
         strong_weak, strong_moderate, strong_strong, strong_very strong,
         very strong_weak, very strong_moderate, very strong_strong, or very strong_very strong")
  }

  #values can be rounded to 2 decimals
  values <- round(values, 2)
  errors <- 1-(values^2)

  #combine pop values and variable names w lavaan constraint operator
  loads <- list()
  for(i in 1:2){
    loads[[i]] <- paste(values[[i]], "*", lvnames[[i]], sep = "")
  }

  loads <- paste(loads, collapse = " + ")

  #generate lvName
  if(var == "X"){
    lvname <- "X_Dy"
  }else if(var == "Y"){
    lvname <- "Y_Dy"
  }

  loadScript <- paste(lvname, " =~ ", loads, sep = "")

  resLatVariances <- list()
  for(i in 1:2){
    resLatVariances[[i]] <- paste(lvnames[[i]], " ~~ ", errors[[i]], "*", lvnames[[i]], sep = "")
  }
  resLatVariances <- paste(resLatVariances, collapse ="\n")

  varnames <- as.character(lvnames)

  loadErrorList <- list(loadings = loadScript,
                        errors = resLatVariances,
                        varnames = varnames,
                        loadValues = values,
                        errorValues = errors)
  return(loadErrorList)
}

#' @rdname generateParams
generatePopResidCorrs <- function(var = "X", type = NULL, num = NULL){

  lv <- var
  items <- num

  varnames_A <- generateObsNames(lv, person = "A", items)
  varnames_B <- generateObsNames(lv, person = "B", items)

  #determine residual covariances values for
  if(type == "nil"){
    values <- rep(0, num)

  }else if(type == "weak"){
    values <- stats::runif(n = num, min = .01, max = .10)

  }else if(type == "moderate"){
    values <- stats::runif(n = num, min = .11, max = .20)

  }else if(type == "strong"){
    values <- stats::runif(n = num, min = .21, max = .30)

  }else if(type == "very strong"){
    values <- stats::runif(n = num, min = .31, max = .41)

  }else if(type == "mixed"){
    values <- stats::runif(n = num, min = .01, max = .41)

  }else{
    stop("residCorrValues arguments can only be nil, weak, moderate, strong, very strong, or mixed")
  }

  #values can be rounded to 2 decimals
  values <- round(values, 2)

  rescors <- list()
  for(i in 1:num){
    rescors[[i]] <- paste(varnames_A[[i]], " ~~ ", values[[i]], "*", varnames_B[[i]], sep = "")
  }

  rescors <- paste(rescors, collapse ="\n")

  varnames_A <- as.character(varnames_A)
  varnames_B <- as.character(varnames_B)

  rescorsList <- list(rescors = rescors,
                      varnames_A = varnames_A,
                      residualValues = values,
                      varnames_B = varnames_B)
  return(rescorsList)
}

#' @rdname generateParams
generateICCs <- function(var = "X", type = NULL){
  #determine residual covariances values for
  if(type == "nil"){
    values <- 0

  }else if(type == "weak"){
    values <- stats::runif(n = 1, min = .01, max = .10)

  }else if(type == "moderate"){
    values <- stats::runif(n = 1, min = .11, max = .20)

  }else if(type == "strong"){
    values <- stats::runif(n = 1, min = .21, max = .30)

  }else if(type == "very strong"){
    values <- stats::runif(n = 1, min = .31, max = .41)
  }

  values <- round(values, 2)

  if(var == "X"){
    var1 <- "X_A"
    var2 <- "X_B"
    icc <- paste(var1, " ~~ ", values, "*", var2, sep = "")
  }else if(var == "Y"){
    var1 <- "Y_A"
    var2 <- "Y_B"
    icc <- paste(var1, " ~~ ", values, "*", var2, sep = "")
  }

  iccList <- list(icc = icc,
                  var1 = var1,
                  var2 = var2,
                  iccValue = values)
  return(iccList)
}

#' @rdname generateParams
generateIPCs <- function(var = "A", type = NULL){
  #determine residual covariances values for
  if(type == "nil"){
    values <- 0

  }else if(type == "weak"){
    values <- stats::runif(n = 1, min = .01, max = .10)

  }else if(type == "moderate"){
    values <- stats::runif(n = 1, min = .11, max = .20)

  }else if(type == "strong"){
    values <- stats::runif(n = 1, min = .21, max = .30)

  }else if(type == "very strong"){
    values <- stats::runif(n = 1, min = .31, max = .41)
  }

  values <- round(values, 2)

  if(var == "A"){
    var1 <- "X_A"
    var2 <- "Y_A"
    ipc <- paste(var1, " ~~ ", values, "*", var2, sep = "")
  }else if(var == "B"){
    var1 <- "X_B"
    var2 <- "Y_B"
    ipc <- paste(var1, " ~~ ", values, "*", var2, sep = "")
  }

  ipcList <- list(ipc = ipc,
                  var1 = var1,
                  var2 = var2,
                  iccValue = values)
  return(ipcList)
}

#' @rdname generateParams
generateLVariances <- function(var = "X", value = 1, partner = "A"){

  latentvariance <- paste(var, "_", partner, " ~~ ", value, "*", var, "_", partner, sep = "")
  return(latentvariance)
}

#' @rdname generateParams
generateActors <- function(partner = "A", type = NULL){
  if(type == "nil"){
    values <- 0

  }else if(type == "weak"){
    values <- stats::runif(n = 1, min = .01, max = .10)

  }else if(type == "moderate"){
    values <- stats::runif(n = 1, min = .11, max = .20)

  }else if(type == "strong"){
    values <- stats::runif(n = 1, min = .21, max = .30)

  }else if(type == "very strong"){
    values <- stats::runif(n = 1, min = .31, max = .41)
  }

  values <- round(values, 2)

  if(partner == "A"){
    outvar <- "Y_A"
    predvar <- "X_A"

    actorEffect <- paste(outvar, " ~ ", values, "*", predvar, sep = "")

  }else if(partner == "B"){
    outvar <- "Y_B"
    predvar <- "X_B"

    actorEffect <- paste(outvar, " ~ ", values, "*", predvar, sep = "")
  }

  actorList <- list(actorEffect = actorEffect,
                    predvar = predvar,
                    outvar = outvar,
                    slopeValue = values)

  return(actorList)
}

#' @rdname generateParams
generatePartners <- function(partner = "A", type = NULL){
  if(type == "nil"){
    values <- 0

  }else if(type == "weak"){
    values <- stats::runif(n = 1, min = .01, max = .10)

  }else if(type == "moderate"){
    values <- stats::runif(n = 1, min = .11, max = .20)

  }else if(type == "strong"){
    values <- stats::runif(n = 1, min = .21, max = .30)

  }else if(type == "very strong"){
    values <- stats::runif(n = 1, min = .31, max = .41)
  }

  values <- round(values, 2)

  if(partner == "A"){
    outvar <- "Y_A"
    predvar <- "X_B"

    partnerEffect <- paste(outvar, " ~ ", values, "*", predvar, sep = "")

  }else if(partner == "B"){
    outvar <- "Y_B"
    predvar <- "X_A"

    partnerEffect <- paste(outvar, " ~ ", values, "*", predvar, sep = "")
  }

  partnerList <- list(partnerEffect = partnerEffect,
                    predvar = predvar,
                    outvar = outvar,
                    slopeValue = values)

  return(partnerList)
}

#' @rdname generateParams
generatePopCFMDySlopeError <- function(type = NULL){
  if(type == "nil"){
    values <- 0

  }else if(type == "weak"){
    values <- stats::runif(n = 1, min = .01, max = .10)

  }else if(type == "moderate"){
    values <- stats::runif(n = 1, min = .11, max = .20)

  }else if(type == "strong"){
    values <- stats::runif(n = 1, min = .21, max = .30)

  }else if(type == "very strong"){
    values <- stats::runif(n = 1, min = .31, max = .41)
  }else{
    stop("type argument can only be nil, weak, moderate, strong, very strong, or mixed")
  }

  values <- round(values, 2)

  dyEffect <- paste0("Y_Dy ~ ", values, "*X_Dy")

  errors <- 1-(values^2)

  resY_DyVariance<- paste0("Y_Dy", " ~~ ", errors, "*", "Y_Dy")

  slopeErrorList <- list(slope = dyEffect,
                         residual = resY_DyVariance)
  return(slopeErrorList)
}
