#' @name scriptPopMods
#' @rdname scriptPopMods
#'
#' @title Helper functions for taking output from generateParams helpers
#' to write population model script using lavaan notation
#'
#' @param popModList list with necessary parameter options for a given popMod
#' @param ... arguments to pass onto helper functions (e.g., saveParamDf)
#'
#' @return a lavaan script with parameter values for a latent APIM
#' @family helpers
#' @export
#'
#' @examples
#' popModList.apim <- list(nItemsX = 5,
#' loadValuesX_A = "moderate", loadValuesX_B = "moderate",
#' residCorrValuesX = "moderate", iccX = "weak",
#' nItemsY = 5,
#' loadValuesY_A = "weak", loadValuesY_B = "mixed",
#' residCorrValuesY = "mixed", iccY = "strong",
#' actorA = "moderate", actorB = "very strong",
#' partnerA = "weak", partnerB = "weak")
#'
#' scriptPopModAPIM(popModList = popModList.apim, paramType = "structural")

scriptPopModAPIM <- function(popModList, ...){

  X_A_loads <- generatePopLoadsErrors(var = "X", partner = "A", type = popModList$loadValuesX_A, num = popModList$nItemsX)
  X_B_loads <- generatePopLoadsErrors(var = "X", partner = "B", type = popModList$loadValuesX_B, num = popModList$nItemsX)

  Y_A_loads <- generatePopLoadsErrors(var = "Y", partner = "A", type = popModList$loadValuesY_A, num = popModList$nItemsY)
  Y_B_loads <- generatePopLoadsErrors(var = "Y", partner = "B", type = popModList$loadValuesY_B, num = popModList$nItemsY)

  X_rescorrs <- generatePopResidCorrs(var = "X", type = popModList$residCorrValuesX, num = popModList$nItemsX)
  Y_rescorrs <- generatePopResidCorrs(var = "Y", type = popModList$residCorrValuesY, num = popModList$nItemsY)

  XA_LatentVariance <- generateLVariances(var = "X", partner = "A", value = 1)
  XB_LatentVariance <- generateLVariances(var = "X", partner = "B", value = 1)
  YA_LatentVariance <- generateLVariances(var = "Y", partner = "A", value = 1)
  YB_LatentVariance <- generateLVariances(var = "Y", partner = "B", value = 1)

  X_ICC <- generateICCs(var = "X", type = popModList$iccX)
  Y_ICC <- generateICCs(var = "Y", type = popModList$iccY)

  Actor_A <- generateActors(partner = "A", type = popModList$actorA)
  Actor_B <- generateActors(partner = "B", type = popModList$actorB)

  Partner_A <- generatePartners(partner = "A", type = popModList$partnerA)
  Partner_B <- generatePartners(partner = "B", type = popModList$partnerB)

  paramList <- list(X_A_loads = X_A_loads, X_B_loads = X_B_loads, Y_A_loads = Y_A_loads, Y_B_loads = Y_B_loads,
                    X_rescorrs = X_rescorrs, Y_rescorrs = Y_rescorrs,
                    XA_LatentVariance = XA_LatentVariance, XB_LatentVariance = XB_LatentVariance, YA_LatentVariance = YA_LatentVariance, YB_LatentVariance = YB_LatentVariance,
                    X_ICC = X_ICC, Y_ICC = Y_ICC,
                    Actor_A = Actor_A, Actor_B = Actor_B, Partner_A = Partner_A, Partner_B = Partner_B)

  popModParams <- saveParamLAPIM(paramList, ...)


  popModScript <- sprintf("#Measurement Model\n\n## Loadings\n%s\n%s\n%s\n%s\n\n## Uniquenesses\n%s\n%s\n%s\n%s\n\n## Residual Correlations\n%s\n%s\n\n# Structural Model
                        \n## Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n## Actor and Partner Effects\n%s\n%s\n%s\n%s",
                        X_A_loads$loadings, X_B_loads$loadings, Y_A_loads$loadings, Y_B_loads$loadings,
                        X_A_loads$errors, X_B_loads$errors, Y_A_loads$errors, Y_B_loads$errors,
                        X_rescorrs$rescors, Y_rescorrs$rescors,
                        XA_LatentVariance, XB_LatentVariance, YA_LatentVariance, YB_LatentVariance, X_ICC$icc, Y_ICC$icc,
                        Actor_A$actorEffect, Actor_B$actorEffect, Partner_A$partnerEffect, Partner_B$partnerEffect)

  popModList <- list(popModScript = popModScript,
                     popModParams = popModParams)

  return(popModList)
  #cat(popModScript, "\n")
}

#' @rdname scriptPopMods
scriptPopModCFM <- function(popModList){

  X_A_loads <- generatePopLoadsErrors(var = "X", partner = "A", type = popModList$loadValuesX_A, num = popModList$nItemsX)
  X_B_loads <- generatePopLoadsErrors(var = "X", partner = "B", type = popModList$loadValuesX_B, num = popModList$nItemsX)
  X_Dy_Loads <- generatePopCFMLoadsErrors(var = "X", type = popModList$loadDyX)

  Y_A_loads <- generatePopLoadsErrors(var = "Y", partner = "A", type = popModList$loadValuesY_A, num = popModList$nItemsY)
  Y_B_loads <- generatePopLoadsErrors(var = "Y", partner = "B", type = popModList$loadValuesY_B, num = popModList$nItemsY)
  Y_Dy_Loads <- generatePopCFMLoadsErrors(var = "Y", type = popModList$loadDyY)

  X_rescorrs <- generatePopResidCorrs(var = "X", type = popModList$residCorrValuesX, num = popModList$nItemsX)
  Y_rescorrs <- generatePopResidCorrs(var = "Y", type = popModList$residCorrValuesY, num = popModList$nItemsY)

  X_Dy_LatentVariance <- generateLVariances(var = "X", partner = "Dy", value = 1)

  Dy_Slope_Error <- generatePopCFMDySlopeError(type = popModList$dyEffect)

  A_IPC <- generateIPCs(var = "A", type = popModList$residCorrA)
  B_IPC <- generateIPCs(var = "B", type = popModList$residCorrB)

  popModScript <- sprintf("#Measurement Model\n\n## Lower-Order Loadings\n%s\n%s\n%s\n%s\n\n## Higher-Order Loadings\n%s\n%s\n\n## Uniquenesses\n%s\n%s\n%s\n%s\n\n## Residual Correlations\n%s\n%s\n\n# Structural Model\n\n## Latent (Co)Variances\n%s\n%s\n%s\n%s\n%s\n%s\n\n## Dyadic Effect\n%s",
                        X_A_loads$loadings, X_B_loads$loadings, Y_A_loads$loadings, Y_B_loads$loadings,
                        X_Dy_Loads$loadings, Y_Dy_Loads$loadings,
                        X_A_loads$errors, X_B_loads$errors, Y_A_loads$errors, Y_B_loads$errors,
                        X_rescorrs$rescors, Y_rescorrs$rescors,
                        X_Dy_Loads$errors, Y_Dy_Loads$errors, X_Dy_LatentVariance, Dy_Slope_Error$residual, A_IPC$ipc, B_IPC$ipc,
                        Dy_Slope_Error$slope)

  return(popModScript)
  #cat(popModScript, "\n")
}
