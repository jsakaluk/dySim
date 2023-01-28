#' Title
#'
#' @param seed numeric input for random seed to use
#' @param popMod character of dyadic model to use as population data generating mechanism. Currently supports "L-APIM"
#' @param popModList popModList with necessary parameter options for a given popMod
#' @param sampSize numeric input for sample size of each randomly drawn sample
#' @param sampMod character of dyadic model to use as analytic model within each randomly drawn sample. Currently supports "APIM"
#' @param sampModType character of type of specification for chosen sampMod. "latent" or "observed"
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
#' sampMod = "APIM",
#' sampModType = "latent",
#' nSims = 10,
#' output = "paramTable")
#'
#' @importFrom rlang .data

runDySim <- function(seed = NULL,
                     popMod = NULL,
                     popModList = NULL,
                     sampSize = NULL,
                     sampMod = NULL,
                     sampModType = NULL,
                     nSims = NULL,
                     output = "paramTable"){

  set.seed(seed)
  counter <- nSims
  nSims <- nSims#will run one iteration out of loop
  nSimsLess1 <- nSims-1

  if(popMod == "L-APIM"){
    popModScript <- scriptPopModAPIM(popModList = popModList)
  }

  #sample size determines size of sample for generate
  samp <- simsem::generate(popModScript, n = sampSize)

  #sampMod + #sampMod type calls appropriate scripter from dySEM
  if(sampMod == "APIM" & sampModType == "latent"){
    #Should build options for noninvariance

    dvn <- dySEM::scrapeVarCross(samp,
                                 x_order = "spi", x_stem = "X", x_delim1 = "_",
                                 y_order = "spi", y_stem = "Y", y_delim1 = "_",
                                 distinguish_1 = "A", distinguish_2 = "B")

    sample.script <- dySEM::scriptAPIM(dvn, lvxname = "X", lvyname = "Y", k = TRUE)

    sample.out <- lavaan::cfa(sample.script, data = samp, std.lv = TRUE)


    if(output == "paramTable"){
      #probably should make a function for cleaning up/labeling output
      sim.dat <- lavaan::parameterestimates(sample.out, standardized = TRUE) %>%
        dplyr::filter(.data$op == "~"|.data$op == ":=") %>%
        dplyr::mutate(pop_model = popMod,
                      samp_model = "L-APIM",
                      samp_n = sampSize) %>%
        dplyr::relocate(.data$pop_model, .data$samp_model, .data$samp_n) %>%
        dplyr::arrange(dplyr::desc(.data$op), .data$label)
    }

    if(nSims > 1){
      for(i in cli::cli_progress_along(1:nSimsLess1, name = paste("Simulating", counter, "samples of", sampSize, "dyads."))){
        samp <- simsem::generate(popModScript, n = sampSize)
        sample.out <- lavaan::cfa(sample.script, data = samp, std.lv = TRUE)
        sim.out <- lavaan::parameterestimates(sample.out, standardized = TRUE) %>%
          dplyr::filter(.data$op == "~"|.data$op == ":=") %>%
          dplyr::mutate(pop_model = popMod,
                        samp_model = "L-APIM",
                        samp_n = sampSize) %>%
          dplyr::relocate(.data$pop_model, .data$samp_model, .data$samp_n) %>%
          dplyr::arrange(dplyr::desc(.data$op), .data$label)
        sim.dat <- dplyr::bind_rows(sim.dat, sim.out)
      }
    }
    #nSims determines number of loops

  }
  else if(sampMod == "APIM" & sampModType == "observed"){
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

    #get output from first sample
    if(output == "paramTable"){
      #probably should make a function for cleaning up/labeling output
      sim.dat <- lavaan::parameterestimates(sample.out, standardized = TRUE) %>%
        dplyr::filter(.data$op == "~"|.data$op == ":=") %>%
        dplyr::mutate(pop_model = popMod,
                      samp_model = "O-APIM",
                      samp_n = sampSize) %>%
        dplyr::relocate(.data$pop_model, .data$samp_model, .data$samp_n) %>%
        dplyr::arrange(dplyr::desc(.data$op), .data$label)
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

        sim.out <- lavaan::parameterestimates(sample.out, standardized = TRUE) %>%
          dplyr::filter(.data$op == "~"|.data$op == ":=") %>%
          dplyr::mutate(pop_model = popMod,
                        samp_model = "O-APIM",
                        samp_n = sampSize) %>%
          dplyr::relocate(.data$pop_model, .data$samp_model, .data$samp_n) %>%
          dplyr::arrange(dplyr::desc(.data$op), .data$label)

        sim.dat <- dplyr::bind_rows(sim.dat, sim.out)
      }
    }

  }
  #output determines what to scrape from each iteration
  return(sim.dat)
}
