#' Run One Simulation Scenario
#'
#' Generate data, fit models, and extract results for a single scenario.
#' Supports model mismatch (DGP model can differ from analysis model).
#' Use \code{analysis_model = "ICC_LMM"} to fit an ICC model via dyLMM (requires
#' non-negative \code{lv_cov} in population values).
#'
#' @param pop_model_x \code{"Uni"} or \code{"Cor"} — model that generates data.
#' @param analysis_model Character; lavaan model syntax (from dySEM or hand-written),
#'   or \code{"ICC_LMM"} for ICC via dyLMM.
#' @param n_items_x Number of indicators per partner.
#' @param n_dyads Number of dyads.
#' @param pop_values_x Named list of population values (Uni or Cor structure).
#' @param x_order,x_stem,x_delim1,x_delim2,distinguish_1,distinguish_2 Variable naming.
#' @param lvname Latent variable name.
#' @param reps Number of replications.
#' @param seed Base seed; each rep gets seed + rep.
#' @param extract Character vector: \code{"estimates"}, \code{"fit"}, \code{"convergence"}, \code{"coverage"}.
#' @param true_values Optional named list of true values for coverage.
#' @param fit_options Named list passed to \code{lavaan::cfa()} (e.g. \code{list(estimator = "mlr")}).
#' @return Tibble with one row per rep and columns for requested outputs.
#' @family simulation
#' @export
run_one_scenario <- function(pop_model_x = c("Uni", "Cor"),
                             analysis_model,
                             n_items_x,
                             n_dyads,
                             pop_values_x,
                             x_order = "sip",
                             x_stem = "x",
                             x_delim1 = "",
                             x_delim2 = "_",
                             distinguish_1 = "A",
                             distinguish_2 = "B",
                             lvname = "X",
                             reps = 100,
                             seed = NULL,
                             extract = c("estimates", "convergence"),
                             true_values = NULL,
                             fit_options = NULL) {
  pop_model_x <- match.arg(pop_model_x)
  if (!is.character(analysis_model) || length(analysis_model) != 1 || nchar(analysis_model) == 0) {
    stop("`analysis_model` must be a non-empty character string (lavaan script).")
  }
  base_seed <- if (is.null(seed)) sample.int(1e6, 1) else seed

  dvn <- dySEM::build_dvn(
    n_items_x = n_items_x,
    x_order = x_order,
    x_stem = x_stem,
    x_delim1 = x_delim1,
    x_delim2 = x_delim2,
    distinguish_1 = distinguish_1,
    distinguish_2 = distinguish_2
  )

  want_estimates <- "estimates" %in% extract
  want_fit <- "fit" %in% extract
  want_coverage <- "coverage" %in% extract

  is_icc_lmm <- identical(trimws(as.character(analysis_model)), "ICC_LMM")
  if (is_icc_lmm && !is.null(pop_values_x$lv_cov)) {
    lv_cov_vals <- unlist(pop_values_x$lv_cov, use.names = FALSE)
    lv_cov_vals <- lv_cov_vals[!is.na(lv_cov_vals)]
    if (length(lv_cov_vals) > 0 && any(lv_cov_vals < 0)) {
      stop("ICC_LMM requires non-negative lv_cov; LMM cannot model negatively correlated dyads.")
    }
  }

  out <- vector("list", reps)
  for (r in seq_len(reps)) {
    rep_seed <- base_seed + r
    pop_syntax <- if (pop_model_x == "Uni") {
      dySEM::scriptPopUni(dvn, lvname, pop_values_x)
    } else {
      dySEM::scriptPopCor(dvn, lvname, pop_values_x)
    }
    dat <- generate_dyadic_data(pop_syntax, n_dyads, seed = rep_seed)
    fit <- fit_dyadic_model(dat, model = analysis_model, dvn = dvn, fit_options = fit_options)

    row <- tibble::tibble(rep = r, converged = NA)

    if (is.null(fit)) {
      row$converged <- FALSE
      if (want_estimates) row <- tibble::add_column(row, estimates = list(NULL), .after = "converged")
      if (want_fit) row <- tibble::add_column(row, fit = list(NULL), .after = ncol(row))
    } else if (inherits(fit, "lmerMod")) {
      row$converged <- !isTRUE(lme4::isSingular(fit))
      if (isTRUE(row$converged)) {
        opt <- fit@optinfo$conv
        if (!is.null(opt) && !is.null(opt$opt) && !is.na(opt$opt)) {
          row$converged <- (opt$opt == 0L)
        }
      }
      row$converged <- isTRUE(row$converged)
      if (want_estimates) {
        pe <- extract_estimates(fit, model_type = "ICC_LMM", want_coverage = want_coverage)
        row <- tibble::add_column(row, estimates = list(pe), .after = "converged")
      }
      if (want_fit) row <- tibble::add_column(row, fit = list(NULL), .after = ncol(row))
      if (want_coverage && !is.null(true_values) && want_estimates) {
        pe <- extract_estimates(fit, model_type = "ICC_LMM", want_coverage = TRUE)
        cov_vec <- rep(NA_real_, length(true_values))
        names(cov_vec) <- names(true_values)
        for (i in seq_along(true_values)) {
          lbl <- names(true_values)[i]
          tv <- true_values[[i]]
          idx <- which(pe$label == lbl)
          if (length(idx) == 0 && grepl("~~", lbl, fixed = TRUE)) {
            parts <- strsplit(lbl, "~~", fixed = TRUE)[[1]]
            if (length(parts) == 2) {
              idx <- which((pe$lhs == parts[1] & pe$rhs == parts[2]) |
                          (pe$lhs == parts[2] & pe$rhs == parts[1]))
            }
          }
          if (length(idx) > 0) {
            cil <- pe[["ci.lower"]]
            ciu <- pe[["ci.upper"]]
            if (!is.null(cil) && !is.null(ciu) && !is.na(cil[idx[1]]) && !is.na(ciu[idx[1]])) {
              cov_vec[i] <- as.double(tv >= cil[idx[1]] & tv <= ciu[idx[1]])
            } else {
              cov_vec[i] <- NA_real_
            }
          }
        }
        row <- tibble::add_column(row, coverage = list(cov_vec), .after = ncol(row))
      }
    } else {
      row$converged <- lavaan::lavInspect(fit, "converged")
      if (want_estimates) {
        pe <- extract_estimates(fit, model_type = "lavaan", want_coverage = want_coverage)
        row <- tibble::add_column(row, estimates = list(pe), .after = "converged")
      }
      if (want_fit) {
        fm <- lavaan::fitmeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
        row <- tibble::add_column(row, fit = list(fm), .after = ncol(row))
      }
      if (want_coverage && !is.null(true_values) && want_estimates) {
        pe <- extract_estimates(fit, model_type = "lavaan", want_coverage = TRUE)
        cov_vec <- rep(NA_real_, length(true_values))
        names(cov_vec) <- names(true_values)
        for (i in seq_along(true_values)) {
          lbl <- names(true_values)[i]
          tv <- true_values[[i]]
          idx <- which(pe$label == lbl)
          if (length(idx) == 0 && grepl("~~", lbl, fixed = TRUE)) {
            parts <- strsplit(lbl, "~~", fixed = TRUE)[[1]]
            if (length(parts) == 2) {
              idx <- which((pe$lhs == parts[1] & pe$rhs == parts[2]) |
                          (pe$lhs == parts[2] & pe$rhs == parts[1]))
            }
          }
          if (length(idx) > 0) {
            cil <- pe[["ci.lower"]]
            ciu <- pe[["ci.upper"]]
            if (!is.null(cil) && !is.null(ciu) && !is.na(cil[idx[1]]) && !is.na(ciu[idx[1]])) {
              cov_vec[i] <- as.double(tv >= cil[idx[1]] & tv <= ciu[idx[1]])
            } else {
              cov_vec[i] <- NA_real_
            }
          }
        }
        row <- tibble::add_column(row, coverage = list(cov_vec), .after = ncol(row))
      }
    }
    out[[r]] <- row
  }

  dplyr::bind_rows(out)
}

#' Run simulation over design grid.
#'
#' @param design Tibble (from \code{create_design()}) or named list for single scenario.
#' @param reps Number of replications per scenario.
#' @param parallel Use \pkg{future} for parallel execution (one future per scenario).
#' @param workers Number of parallel workers.
#' @param save_dir If set, save each scenario to \code{scenario_<i>.rds} and optionally combine.
#' @param extract Elements to extract per rep.
#' @return Tibble of raw results.
#' @export
run_simulation <- function(design,
                          reps = 100,
                          parallel = FALSE,
                          workers = NULL,
                          save_dir = NULL,
                          extract = c("estimates", "convergence")) {
  if (is.list(design) && !inherits(design, "data.frame")) {
    design <- tibble::as_tibble(design)
  }
  design$reps <- reps
  design$extract <- rep(list(extract), nrow(design))
  n_scenarios <- nrow(design)
  if (n_scenarios == 0) stop("Empty design.")

  design_cols <- setdiff(names(design), c("reps", "extract"))
  scenario_args_names <- c("pop_model_x", "analysis_model", "n_items_x", "n_dyads",
    "pop_values_x", "x_order", "x_stem", "x_delim1", "x_delim2",
    "distinguish_1", "distinguish_2", "lvname", "reps", "seed", "extract",
    "true_values", "fit_options")

  # Build scenario args list (extract rows in main process to avoid serialization
  # issues with tibble list-cols when running in parallel)
  extract_row <- function(i) {
    full_row <- as.list(design[i, , drop = FALSE])
    full_row <- lapply(full_row, function(x) {
      if (is.list(x) && length(x) == 1 && !inherits(x, "data.frame")) {
        x[[1]]
      } else {
        x
      }
    })
    row <- full_row[intersect(names(full_row), scenario_args_names)]
    if ("analysis_model" %in% names(row) && is.character(row$analysis_model)) {
      row$analysis_model <- trimws(as.character(row$analysis_model))
    }
    meta <- design[i, design_cols, drop = FALSE]
    list(row = row, scenario_id = i, meta = meta)
  }
  scenario_args <- lapply(seq_len(n_scenarios), extract_row)

  run_one <- function(args) {
    out <- do.call(run_one_scenario, args$row)
    out$scenario_id <- args$scenario_id
    out <- dplyr::bind_cols(args$meta[rep(1L, nrow(out)), ], out)
    out
  }

  if (!is.null(save_dir)) {
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    for (i in seq_len(n_scenarios)) {
      res <- run_one(scenario_args[[i]])
      saveRDS(res, file.path(save_dir, paste0("scenario_", i, ".rds")))
    }
    files <- file.path(save_dir, paste0("scenario_", seq_len(n_scenarios), ".rds"))
    return(dplyr::bind_rows(lapply(files, readRDS)))
  }

  if (parallel && requireNamespace("future", quietly = TRUE)) {
    workers <- workers %||% max(1, parallel::detectCores() - 1)
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
    # Use one future per scenario. Bypass furrr to avoid its globals handling
    # mis-expanding nested list columns. Pass args_i explicitly so workers
    # receive pre-extracted data. Workers load dySim/dySEM so run_one_scenario
    # and its dependencies are available; parallel requires dySim to be
    # installed (not just load_all) so workers get the current API.
    futures <- lapply(seq_len(n_scenarios), function(i) {
      args_i <- scenario_args[[i]]
      future::future(
        {
          out <- do.call(run_one_scenario, args_i$row)
          out$scenario_id <- args_i$scenario_id
          dplyr::bind_cols(args_i$meta[rep(1L, nrow(out)), ], out)
        },
        globals = list(args_i = args_i, run_one_scenario = run_one_scenario),
        packages = c("dySEM", "dyLMM", "dySim", "lavaan", "lme4", "dplyr", "tibble", "tidyr"),
        seed = TRUE
      )
    })
    res_list <- lapply(futures, future::value)
    res <- dplyr::bind_rows(res_list)
  } else {
    res <- dplyr::bind_rows(lapply(scenario_args, run_one))
  }
  res
}

# Simple %||% for default
`%||%` <- function(x, y) if (is.null(x)) y else x
