#' Title
#'
#' @param seed numeric input for random seed to use
#' @param popMod character of dyadic model to use as population data generating mechanism. Currently supports "L-APIM".
#' @param popModList popModList with necessary parameter options for a given popMod
#' @param sampSize numeric input for sample size of each randomly drawn sample
#' @param sampMod character of dyadic model to use as analytic model within each randomly drawn sample, including "L-APIM", and "O-APIM"
#' @param nSims numeric input for number of randomly drawn samples to draw from popMod
#' @param output character of type of output to extract from each analyzed sampMod. Currently supports "paramTable"
#'
#' @return a tibble of chosen output from nSims random samples
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
#' apim.sim.out <- runDySim(seed = 123,
#' popMod = "L-APIM",
#' popModList = popModList.apim,
#' sampSize = 100,
#' sampMod = "L-APIM",
#' nSims = 10,
#' output = "paramTable")
#'
#' @importFrom rlang .data

runDySim <- function(seed = NULL,
                     popMod = NULL,
                     popModList = NULL,
                     sampSize = NULL,
                     sampMod = NULL,
                     nSims = NULL,
                     output = "paramTable"){

  set.seed(seed)
  counter <- nSims
  nSims <- nSims#will run one iteration out of loop
  nSimsLess1 <- nSims-1

  if(popMod == "L-APIM"){
    popModData <- scriptPopModAPIM(popModList = popModList, paramType = "structural")
    popModScript <- popModData$popModScript
  }

  #sample size determines size of sample for generate
  samp <- simsem::generate(popModScript, n = sampSize)

  #sampMod + #sampMod type calls appropriate scripter from dySEM
  if(sampMod == "L-APIM"){
    #Should build options for noninvariance

    dvn <- dySEM::scrapeVarCross(samp,
                                 x_order = "spi", x_stem = "X", x_delim1 = "_",
                                 y_order = "spi", y_stem = "Y", y_delim1 = "_",
                                 distinguish_1 = "A", distinguish_2 = "B")

    sample.script <- dySEM::scriptAPIM(dvn, lvxname = "X", lvyname = "Y", k = TRUE)

    sample.out <- lavaan::cfa(sample.script, data = samp, std.lv = TRUE)

    simCount <- 1

    if(output == "paramTable"){
      sim.dat <- getParams(fit = sample.out, sampCount = simCount, first = TRUE)
    }


    if(nSims > 1){
      for(i in cli::cli_progress_along(1:nSimsLess1, name = paste("Simulating", counter, "samples of", sampSize, "dyads."))){
        samp <- simsem::generate(popModScript, n = sampSize)

        simCount <- i+1

        sample.out <- lavaan::cfa(sample.script, data = samp, std.lv = TRUE)

        if(output == "paramTable"){
          sim.out <- getParams(fit = sample.out, sampCount = simCount, first = FALSE)
        }

        sim.dat <- dplyr::bind_rows(sim.dat, sim.out)
      }
    }
    sim.dat <- sim.dat %>%
      dplyr::mutate(pop_mod = popMod,
                    samp_mod = sampMod,
                    samp_n = sampSize) %>%
      dplyr::relocate(.data$sim_num, .data$pop_mod, .data$samp_mod, .data$samp_n) %>%
      dplyr::arrange(.data$sim_num, dplyr::desc(.data$op), .data$label)
    #nSims determines number of loops

  }
  else if(sampMod == "O-APIM"){
    samp <- samp %>%
      #dplyr::rowwise() %>%
      dplyr::mutate(X_A = rowMeans(dplyr::across(dplyr::starts_with("X_A"))),
                    X_B = rowMeans(dplyr::across(dplyr::starts_with("X_B"))),
                    Y_A = rowMeans(dplyr::across(dplyr::starts_with("Y_A"))),
                    Y_B = rowMeans(dplyr::across(dplyr::starts_with("Y_B"))))

    #script
    sample.script <- dySEM::scriptObsAPIM(X1 = "X_A", Y1 = "Y_A",
                                          X2 = "X_B", Y2 = "Y_B",
                                          k = TRUE)

    #fit script to first sample
    sample.out <- lavaan::cfa(sample.script, data = samp)
    simCount <- 1

    #get output from first sample
    if(output == "paramTable"){
      #probably should make a function for cleaning up/labeling output
      sim.dat <- getParams(fit = sample.out, sampCount = simCount, first = TRUE)
    }


    if(nSims > 1){
      #nSims determines number of loops
      for(i in cli::cli_progress_along(1:nSimsLess1, name = paste("Simulating", counter, "samples of", sampSize, "dyads."))){
        samp <- simsem::generate(popModScript, n = sampSize)
        samp <- samp %>%
          #dplyr::rowwise() %>%
          dplyr::mutate(X_A = rowMeans(dplyr::across(dplyr::starts_with("X_A"))),
                        X_B = rowMeans(dplyr::across(dplyr::starts_with("X_B"))),
                        Y_A = rowMeans(dplyr::across(dplyr::starts_with("Y_A"))),
                        Y_B = rowMeans(dplyr::across(dplyr::starts_with("Y_B"))))

        sample.out <- lavaan::cfa(sample.script, data = samp)

        simCount <- i+1

        if(output == "paramTable"){
          sim.out <- getParams(fit = sample.out, sampCount = simCount, first = FALSE)
        }

        sim.dat <- dplyr::bind_rows(sim.dat, sim.out)
      }
    }
    sim.dat <- sim.dat %>%
      dplyr::mutate(pop_mod = popMod,
                    samp_mod = sampMod,
                    samp_n = sampSize) %>%
      dplyr::relocate(.data$sim_num, .data$pop_mod, .data$samp_mod, .data$samp_n) %>%
      dplyr::arrange(.data$sim_num, dplyr::desc(.data$op), .data$label)

  }
  sim.out <- list(sim.dat = sim.dat,
                  pop.params = popModData$popModParams)
  #output determines what to scrape from each iteration
  return(sim.out)
}
