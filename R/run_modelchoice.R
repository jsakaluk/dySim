#' Run One Model Choice Scenario
#'
#' Generate data from one DGP, fit all candidate models, apply selection criteria,
#' and record chosen model and correctness. Internal use by
#' \code{run_modelchoice_analysis}.
#'
#' @param pop_model_x DGP label (e.g. "Uni", "Cor").
#' @param pop_values_x Named list of population values for data generation.
#' @param pop_generator \code{"Uni"} or \code{"Cor"} — which generator to use.
#' @param analysis_models Named list of lavaan scripts (ordered simplest to most complex).
#' @param analysis_model_labels Character vector of model names.
#' @param n_items_x,n_dyads,lvname,dvn Naming and sample size.
#' @param correct_label The analysis model label considered correct for this DGP.
#' @param reps Number of replications.
#' @param seed Base seed.
#' @param selection_criteria Character vector of criterion names.
#' @param alpha Alpha for chi2_diff variants.
#' @param delta_cfi_cutoff,delta_rmsea_cutoff,delta_srmr_cutoff Cutoffs for delta criteria.
#' @param vote_criteria Character vector of criteria for majority_vote (required when used).
#' @param fit_options Optional list passed to lavaan.
#' @return Tibble with rep, chosen_<criterion>, correct, scenario meta.
#' @keywords internal
run_one_modelchoice_scenario <- function(pop_model_x,
                                         pop_values_x,
                                         pop_generator,
                                         analysis_models,
                                         analysis_model_labels,
                                         n_items_x,
                                         n_dyads,
                                         lvname = "X",
                                         dvn = NULL,
                                         correct_label,
                                         reps = 100,
                                         seed = NULL,
                                         selection_criteria =
                                           c("chi2_diff_startsimple", "chi2_diff_startcomplex", "bic_diff"),
                                         alpha = 0.05,
                                         delta_cfi_cutoff = 0.01,
                                         delta_rmsea_cutoff = 0.015,
                                         delta_srmr_cutoff = 0.03,
                                         vote_criteria = NULL,
                                         fit_options = NULL) {
  if (is.null(dvn)) {
    dvn <- dySEM::build_dvn(n_items_x = n_items_x, x_stem = "x", x_delim2 = "_",
      distinguish_1 = "A", distinguish_2 = "B")
  }
  base_seed <- if (is.null(seed)) sample.int(1e6, 1) else seed
  K <- length(analysis_models)
  if (K < 2) stop("Need at least 2 analysis models.")
  n_dyads <- as.integer(if (is.list(n_dyads) || length(n_dyads) > 1) n_dyads[[1]] else n_dyads[1])

  out <- vector("list", reps)
  for (r in seq_len(reps)) {
    rep_seed <- base_seed + r
    pop_syntax <- if (pop_generator == "Uni") {
      dySEM::scriptPopUni(dvn, lvname, pop_values_x)
    } else {
      dySEM::scriptPopCor(dvn, lvname, pop_values_x)
    }
    dat <- generate_dyadic_data(pop_syntax, n_dyads, seed = rep_seed)

    # Fit all models
    fits <- vector("list", K)
    for (i in seq_len(K)) {
      fits[[i]] <- fit_dyadic_model(dat, model = analysis_models[[i]], dvn = dvn,
        fit_options = fit_options)
    }

    any_fail <- any(vapply(fits, is.null, logical(1)))
    row <- tibble::tibble(rep = r, pop_model_x = pop_model_x, correct_label = correct_label)
    for (crit in selection_criteria) {
      chosen <- NA_character_
      if (!any_fail) {
        chosen <- apply_selection_criterion(crit, fits, analysis_model_labels, alpha,
          delta_cfi_cutoff = delta_cfi_cutoff, delta_rmsea_cutoff = delta_rmsea_cutoff,
          delta_srmr_cutoff = delta_srmr_cutoff, vote_criteria = vote_criteria)
      }
      row[[paste0("chosen_", crit)]] <- chosen
    }
    row$all_converged <- !any_fail
    for (crit in selection_criteria) {
      row[[paste0("correct_", crit)]] <- if (any_fail) NA else
        (row[[paste0("chosen_", crit)]] == correct_label)
    }
    out[[r]] <- row
  }

  dplyr::bind_rows(out)
}

#' Extract fit index from lavaan object
#' @param fit Fitted lavaan object
#' @param idx Index name (e.g. "cfi", "rmsea", "srmr")
#' @return Numeric value or NA
#' @keywords internal
get_fit_index <- function(fit, idx) {
  if (is.null(fit)) return(NA_real_)
  m <- tryCatch(lavaan::fitMeasures(fit, idx), error = function(e) NA_real_)
  if (length(m) == 0) return(NA_real_)
  as.numeric(m)[1]
}

#' Apply a single model selection criterion
#' @param criterion Name of criterion
#' @param fits List of fitted lavaan objects (ordered simplest to most complex)
#' @param labels Character vector of model names
#' @param alpha Alpha for chi2_diff
#' @param delta_cfi_cutoff Cutoff for delta_cfi (default 0.01)
#' @param delta_rmsea_cutoff Cutoff for delta_rmsea (default 0.015)
#' @param delta_srmr_cutoff Cutoff for delta_srmr (default 0.03)
#' @param vote_criteria Character vector of criteria for majority_vote
#' @return Character: chosen model label
#' @keywords internal
apply_selection_criterion <- function(criterion, fits, labels, alpha,
  delta_cfi_cutoff = 0.01, delta_rmsea_cutoff = 0.015, delta_srmr_cutoff = 0.03,
  vote_criteria = NULL) {
  K <- length(fits)
  if (criterion == "bic_diff") {
    bics <- vapply(fits, function(f) lavaan::fitMeasures(f, "bic"), numeric(1))
    idx <- which.min(bics)
    if (length(idx) > 1) idx <- idx[1]
    return(labels[idx])
  }

  if (criterion == "aic_diff") {
    aics <- vapply(fits, function(f) lavaan::fitMeasures(f, "aic"), numeric(1))
    idx <- which.min(aics)
    if (length(idx) > 1) idx <- idx[1]
    return(labels[idx])
  }

  if (criterion == "sabic_diff") {
    sabics <- vapply(fits, function(f) lavaan::fitMeasures(f, "bic2"), numeric(1))
    idx <- which.min(sabics)
    if (length(idx) > 1) idx <- idx[1]
    return(labels[idx])
  }

  pval_from_lrt <- function(cmp) {
    if (is.null(cmp) || nrow(cmp) < 2) return(NA_real_)
    for (col in c("Pr(>Chisq)", "p value", "Pr(>Chi)")) {
      if (col %in% names(cmp)) return(as.numeric(cmp[2, col]))
    }
    NA_real_
  }

  if (criterion == "chi2_diff_startsimple") {
    current <- 1L
    for (i in seq_len(K - 1)) {
      cmp <- tryCatch(lavaan::lavTestLRT(fits[[i]], fits[[i + 1]]), error = function(e) NULL)
      pval <- pval_from_lrt(cmp)
      if (is.na(pval)) break
      if (pval < alpha) current <- i + 1L
    }
    return(labels[current])
  }

  if (criterion == "chi2_diff_startcomplex") {
    current <- K
    for (i in rev(seq_len(K - 1))) {
      cmp <- tryCatch(lavaan::lavTestLRT(fits[[i]], fits[[i + 1]]), error = function(e) NULL)
      pval <- pval_from_lrt(cmp)
      if (is.na(pval)) break
      if (pval >= alpha) current <- i
    }
    return(labels[current])
  }

  run_delta_step <- function(idx_name, cutoff) {
    current <- 1L
    for (i in seq_len(K - 1)) {
      v_simple <- get_fit_index(fits[[i]], idx_name)
      v_complex <- get_fit_index(fits[[i + 1]], idx_name)
      if (is.na(v_simple) || is.na(v_complex)) break
      delta <- v_complex - v_simple
      if (delta >= cutoff) current <- i + 1L
    }
    labels[current]
  }

  if (criterion == "delta_cfi") {
    return(run_delta_step("cfi", delta_cfi_cutoff))
  }
  if (criterion == "delta_rmsea") {
    return(run_delta_step("rmsea", delta_rmsea_cutoff))
  }
  if (criterion == "delta_srmr") {
    return(run_delta_step("srmr", delta_srmr_cutoff))
  }

  if (criterion == "delta_cfi_or_delta_rmsea") {
    current <- 1L
    for (i in seq_len(K - 1)) {
      d_cfi <- get_fit_index(fits[[i + 1]], "cfi") - get_fit_index(fits[[i]], "cfi")
      d_rmsea <- get_fit_index(fits[[i + 1]], "rmsea") - get_fit_index(fits[[i]], "rmsea")
      if (is.na(d_cfi) && is.na(d_rmsea)) break
      if ((!is.na(d_cfi) && d_cfi >= delta_cfi_cutoff) ||
          (!is.na(d_rmsea) && d_rmsea >= delta_rmsea_cutoff)) current <- i + 1L
    }
    return(labels[current])
  }

  if (criterion == "delta_cfi_and_delta_rmsea") {
    current <- 1L
    for (i in seq_len(K - 1)) {
      d_cfi <- get_fit_index(fits[[i + 1]], "cfi") - get_fit_index(fits[[i]], "cfi")
      d_rmsea <- get_fit_index(fits[[i + 1]], "rmsea") - get_fit_index(fits[[i]], "rmsea")
      if (is.na(d_cfi) || is.na(d_rmsea)) break
      if (d_cfi >= delta_cfi_cutoff && d_rmsea >= delta_rmsea_cutoff) current <- i + 1L
    }
    return(labels[current])
  }

  if (criterion == "majority_vote") {
    if (is.null(vote_criteria) || length(vote_criteria) == 0) {
      return(NA_character_)
    }
    vote_criteria <- vote_criteria[vote_criteria != "majority_vote"]
    if (length(vote_criteria) == 0) return(labels[1L])
    votes <- character(length(vote_criteria))
    for (j in seq_along(vote_criteria)) {
      votes[j] <- apply_selection_criterion(
        vote_criteria[j], fits, labels, alpha,
        delta_cfi_cutoff = delta_cfi_cutoff, delta_rmsea_cutoff = delta_rmsea_cutoff,
        delta_srmr_cutoff = delta_srmr_cutoff, vote_criteria = NULL
      )
    }
    votes <- votes[!is.na(votes) & nzchar(votes)]
    if (length(votes) == 0) return(labels[1L])
    tbl <- table(votes)
    mx <- max(tbl)
    winners <- names(tbl)[tbl == mx]
    if (length(winners) == 1) return(winners)
    idx_win <- which(labels %in% winners)[1]
    labels[idx_win]
  }

  NA_character_
}

#' Run Model Choice Analysis
#'
#' Fit competing models to each generated dataset, apply selection criteria, and
#' compute sensitivity/specificity with respect to a correct-model mapping.
#'
#' @details
#' **Model order:** Design \code{analysis_models} must be ordered simplest first
#' (see \code{\link{create_modelchoice_design}}). **Low power:** With few items
#' (e.g., 3), the chi2-difference test for loading invariance has low power; BIC
#' can prefer the wrong model because it heavily penalizes extra parameters.
#' Expect sensitivity to improve with more items or larger n.
#'
#' @param design Design from \code{create_modelchoice_design()}.
#' @param correct_model Named vector or list: names = \code{pop_model_x} levels,
#'   values = analysis model labels (from \code{analysis_model_labels}).
#' @param selection_criteria Character vector: \code{"chi2_diff_startsimple"},
#'   \code{"chi2_diff_startcomplex"}, \code{"bic_diff"}, \code{"aic_diff"},
#'   \code{"sabic_diff"}, \code{"delta_cfi"}, \code{"delta_rmsea"}, \code{"delta_srmr"},
#'   \code{"delta_cfi_or_delta_rmsea"}, \code{"delta_cfi_and_delta_rmsea"},
#'   \code{"majority_vote"}.
#' @param alpha Alpha for chi2 difference tests.
#' @param reps Number of replications per scenario.
#' @param parallel Use \pkg{future} for parallel execution.
#' @param workers Number of parallel workers.
#' @param delta_cfi_cutoff,delta_rmsea_cutoff,delta_srmr_cutoff Cutoffs for delta
#'   criteria (defaults: 0.01, 0.015, 0.03).
#' @param vote_criteria Character vector of criteria for \code{"majority_vote"}
#'   (required when \code{"majority_vote"} is in \code{selection_criteria}).
#' @param fit_options Named list passed to \code{lavaan::cfa()}.
#' @return List with \code{raw} (full results) and \code{summary} (sensitivity,
#'   specificity by scenario, DGP, criterion).
#' @family model-choice
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
#'   n_dyads = 100,
#'   n_items_x = 3,
#'   .seed = 12345
#' )
#' res <- run_modelchoice_analysis(design, correct_model = c(Uni = "uni", Cor = "cor"),
#'   reps = 25)
run_modelchoice_analysis <- function(design,
                                     correct_model,
                                     selection_criteria =
                                       c("chi2_diff_startsimple", "chi2_diff_startcomplex", "bic_diff"),
                                     alpha = 0.05,
                                     reps = 100,
                                     parallel = FALSE,
                                     workers = NULL,
                                     delta_cfi_cutoff = 0.01,
                                     delta_rmsea_cutoff = 0.015,
                                     delta_srmr_cutoff = 0.03,
                                     vote_criteria = NULL,
                                     fit_options = NULL) {
  if (!"analysis_models" %in% names(design)) {
    stop("Design must have analysis_models list-column from create_modelchoice_design.")
  }
  if (!"pop_model_x" %in% names(design) || !"pop_values_x" %in% names(design)) {
    stop("Design must have pop_model_x and pop_values_x.")
  }
  if (!"pop_generator" %in% names(design)) {
    stop("Design must have pop_generator (from create_modelchoice_design).")
  }

  am <- design$analysis_models[[1]]
  if (!is.list(am) || length(am) < 2) {
    stop("Each scenario must have analysis_models list of length >= 2.")
  }
  labels <- names(am)
  if (is.null(labels) || any(!nzchar(labels))) {
    stop("analysis_models must have names.")
  }

  pmx_levels <- unique(design$pop_model_x)
  cm_names <- if (is.vector(correct_model) && !is.list(correct_model)) {
    names(correct_model)
  } else {
    names(correct_model)
  }
  missing_cm <- setdiff(pmx_levels, cm_names)
  if (length(missing_cm) > 0) {
    stop("correct_model missing entries for pop_model_x: ", paste(missing_cm, collapse = ", "))
  }
  cm_vals <- unlist(correct_model)
  invalid_cm <- setdiff(cm_vals, labels)
  if (length(invalid_cm) > 0) {
    stop("correct_model values must match analysis_model_labels. Invalid: ",
      paste(invalid_cm, collapse = ", "))
  }

  if (length(pmx_levels) < 2 || length(cm_names) < 2) {
    warning("Only one DGP type or one correct_model entry. Specificity requires at least two DGP types.")
  }

  if ("majority_vote" %in% selection_criteria && (is.null(vote_criteria) || length(vote_criteria) == 0)) {
    stop("vote_criteria is required when \"majority_vote\" is in selection_criteria.")
  }

  n_scenarios <- nrow(design)

  # Pre-extract scenario args for parallel (avoids serializing full design)
  extract_row <- function(i) {
    am <- design$analysis_models[[i]]
    pv <- design$pop_values_x[[i]]
    pmx <- design$pop_model_x[i]
    if (is.list(pmx) || length(pmx) > 1) pmx <- pmx[[1]]
    pg <- design$pop_generator[i]
    if (is.list(pg) || length(pg) > 1) pg <- pg[[1]]
    nd <- design$n_dyads[i]
    if (is.list(nd) || length(nd) > 1) nd <- nd[[1]]
    ni <- if ("n_items_x" %in% names(design)) {
      design$n_items_x[i]
    } else 3
    if (is.list(ni) || length(ni) > 1) ni <- ni[[1]]
    sd <- if ("seed" %in% names(design)) design$seed[i] else NULL
    if (is.list(sd) || length(sd) > 1) sd <- sd[[1]]
    list(
      pop_model_x = pmx,
      pop_values_x = pv,
      pop_generator = pg,
      analysis_models = am,
      analysis_model_labels = names(am),
      n_items_x = ni,
      n_dyads = nd,
      lvname = "X",
      dvn = NULL,
      correct_label = correct_model[[pmx]],
      reps = reps,
      seed = sd,
      selection_criteria = selection_criteria,
      alpha = alpha,
      delta_cfi_cutoff = delta_cfi_cutoff,
      delta_rmsea_cutoff = delta_rmsea_cutoff,
      delta_srmr_cutoff = delta_srmr_cutoff,
      vote_criteria = vote_criteria,
      fit_options = fit_options,
      scenario_id = i
    )
  }
  scenario_args <- lapply(seq_len(n_scenarios), extract_row)

  run_one <- function(args) {
    args_out <- args[setdiff(names(args), "scenario_id")]
    out <- do.call(run_one_modelchoice_scenario, args_out)
    out$scenario_id <- args$scenario_id
    meta <- design[args$scenario_id, setdiff(names(design), c("analysis_models", "pop_values_x")), drop = FALSE]
    meta_extra <- meta[, setdiff(names(meta), names(out)), drop = FALSE]
    dplyr::bind_cols(out, meta_extra[rep(1L, nrow(out)), ])
  }

  if (parallel && requireNamespace("future", quietly = TRUE)) {
    workers <- workers %||% max(1, parallel::detectCores() - 1)
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
    futures <- lapply(seq_len(n_scenarios), function(i) {
      args_i <- scenario_args[[i]]
      future::future(
        {
          out <- do.call(run_one_modelchoice_scenario, args_i[setdiff(names(args_i), "scenario_id")])
          out$scenario_id <- args_i$scenario_id
          design_local <- design
          meta <- design_local[args_i$scenario_id, setdiff(names(design_local), c("analysis_models", "pop_values_x")), drop = FALSE]
          meta_extra <- meta[, setdiff(names(meta), names(out)), drop = FALSE]
          dplyr::bind_cols(out, meta_extra[rep(1L, nrow(out)), ])
        },
        globals = list(args_i = args_i, design = design, run_one_modelchoice_scenario = run_one_modelchoice_scenario),
        packages = c("dySim", "dySEM", "lavaan", "dplyr", "tibble"),
        seed = TRUE
      )
    })
    res_list <- lapply(futures, future::value)
    raw <- dplyr::bind_rows(res_list)
  } else {
    raw <- dplyr::bind_rows(lapply(scenario_args, run_one))
  }

  summary <- aggregate_modelchoice_summary(raw, selection_criteria)

  list(raw = raw, summary = summary)
}

#' Aggregate raw model choice results to sensitivity/specificity summary
#' Preserves scenario-level design factors (n_dyads, etc.) when present.
#' @param raw Raw results from run_modelchoice_analysis
#' @param selection_criteria Character vector of criteria
#' @return Tibble with pop_model_x, criterion, sensitivity, specificity, n_reps, n_converged,
#'   plus design columns (n_dyads, etc.) when present in raw
#' @keywords internal
aggregate_modelchoice_summary <- function(raw, selection_criteria) {
  conv_col <- "all_converged"
  if (!conv_col %in% names(raw)) return(tibble::tibble())

  pmx_levels <- unique(raw$pop_model_x)
  n_scen <- length(unique(raw$scenario_id[!is.na(raw$scenario_id)]))

  rows <- list()
  if ("scenario_id" %in% names(raw) && n_scen > 1 && "n_dyads" %in% names(raw)) {
    # Flatten n_dyads if stored as list (e.g. from expand_grid with list input)
    raw_flat <- raw
    if (is.list(raw_flat$n_dyads) && !is.data.frame(raw_flat$n_dyads)) {
      raw_flat$n_dyads <- vapply(raw_flat$n_dyads, function(x) as.numeric(if (is.list(x)) x[[1]] else x)[1], numeric(1))
    }
    # Scenario-level: preserve n_dyads for faceting
    sens_scenarios <- unique(raw_flat[, c("scenario_id", "pop_model_x", "n_dyads")])
    for (i in seq_len(nrow(sens_scenarios))) {
      sid <- sens_scenarios$scenario_id[i]
      pmx <- sens_scenarios$pop_model_x[i]
      n_dyads_val <- sens_scenarios$n_dyads[i]
      correct_lbl <- unique(raw_flat$correct_label[raw_flat$pop_model_x == pmx])[1]

      for (crit in selection_criteria) {
        corr_col <- paste0("correct_", crit)
        chosen_col <- paste0("chosen_", crit)

        sub_sens <- raw_flat[raw_flat$scenario_id == sid & raw_flat[[conv_col]], ]
        sub_spec <- raw_flat[raw_flat$pop_model_x != pmx & raw_flat$n_dyads == n_dyads_val & raw_flat[[conv_col]], ]

        rows[[length(rows) + 1]] <- tibble::tibble(
          pop_model_x = pmx,
          criterion = crit,
          sensitivity = if (nrow(sub_sens) > 0) mean(sub_sens[[corr_col]], na.rm = TRUE) else NA_real_,
          specificity = if (nrow(sub_spec) > 0) mean(sub_spec[[chosen_col]] != correct_lbl, na.rm = TRUE) else NA_real_,
          n_reps = nrow(raw_flat[raw_flat$scenario_id == sid, ]),
          n_converged = nrow(sub_sens),
          n_dyads = n_dyads_val
        )
      }
    }
  } else {
    # Flat aggregation (single scenario or no scenario_id)
    for (pmx in pmx_levels) {
      correct_lbl <- unique(raw$correct_label[raw$pop_model_x == pmx])[1]
      for (crit in selection_criteria) {
        corr_col <- paste0("correct_", crit)
        chosen_col <- paste0("chosen_", crit)
        sub_sens <- raw[raw$pop_model_x == pmx & raw[[conv_col]], ]
        sub_spec <- raw[raw$pop_model_x != pmx & raw[[conv_col]], ]
        rows[[length(rows) + 1]] <- tibble::tibble(
          pop_model_x = pmx,
          criterion = crit,
          sensitivity = if (nrow(sub_sens) > 0) mean(sub_sens[[corr_col]], na.rm = TRUE) else NA_real_,
          specificity = if (nrow(sub_spec) > 0) mean(sub_spec[[chosen_col]] != correct_lbl, na.rm = TRUE) else NA_real_,
          n_reps = sum(raw$pop_model_x == pmx),
          n_converged = nrow(sub_sens)
        )
      }
    }
  }

  dplyr::bind_rows(rows)
}
