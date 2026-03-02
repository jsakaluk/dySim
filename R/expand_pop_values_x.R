#' Expand pop_values_x Into Scenarios
#'
#' Converts a tiered structure into a tibble where each row is one scenario's
#' population values. Supports: (1) fixed params supplied once and recycled,
#' (2) condition bundles for mathematically dependent params (e.g. loadings +
#' residuals), and (3) cross factors that vary independently. When \code{conditions}
#' or cross factors are named lists, names become label columns for grouping.
#'
#' @param pop_values_x Named list with three optional tiers:
#'   \itemize{
#'     \item \strong{Fixed}: Elements that are a single value or vector (supply
#'       once, recycled into every scenario).
#'     \item \strong{Bundles} (\code{conditions}): A list of named lists; each
#'       sublist is a coherent condition (e.g. loadings + residuals that must
#'       stay paired). Use names for labels: \code{list(weak = ..., strong = ...)}.
#'     \item \strong{Cross factors}: Elements that are lists of scalars/vectors
#'       (e.g. \code{lv_cov = list(low = 0.3, med = 0.5, high = 0.8)}). Names
#'       become label columns.
#'   }
#' @param labels Optional named list of label vectors for conditions and cross
#'   factors when they are unnamed (e.g. \code{list(conditions = c("weak","strong"),
#'   lv_cov = c("low","med","high"))}). Used as fallback when names are missing.
#' @return Tibble with \code{pop_values_x} list-column plus \code{condition_label},
#'   scalar columns for cross factor values, and \code{<factor>_label} columns
#'   when names or labels are provided.
#' @family design
#' @export
#' @examples
#' # Fixed + cross factor only (no bundles)
#' expand_pop_values_x(list(
#'   loadings_p1 = c(.75, .80, .55),
#'   loadings_p2 = c(.75, .80, .55),
#'   residuals_p1 = c(.44, .36, .70),
#'   residuals_p2 = c(.44, .36, .70),
#'   coresids = c(.1, .12, .08),
#'   lv_cov = list(0.3, 0.5, 0.8)
#' ))
#'
#' # Fixed + bundles + cross factor (dependent measurement conditions)
#' expand_pop_values_x(list(
#'   intercepts = rep(0, 3),
#'   coresids = c(.1, .12, .08),
#'   conditions = list(
#'     list(loadings_p1 = c(.35,.4,.37), loadings_p2 = c(.35,.4,.37),
#'          residuals_p1 = c(.8775,.84,.8631), residuals_p2 = c(.8775,.84,.8631)),
#'     list(loadings_p1 = c(.7,.83,.87), loadings_p2 = c(.7,.83,.87),
#'          residuals_p1 = c(.49,.311,.243), residuals_p2 = c(.49,.311,.243))
#'   ),
#'   lv_cov = list(0.3, 0.5, 0.8)
#' ))
#'
#' # Full bundle (conditions only; everything in each sublist)
#' expand_pop_values_x(list(
#'   conditions = list(
#'     list(loadings_p1 = c(.75,.8,.55), residuals_p1 = c(.44,.36,.7),
#'          coresids = c(.1,.12,.08), lv_cov = 0.3),
#'     list(loadings_p1 = c(.7,.83,.87), residuals_p1 = c(.49,.31,.24),
#'          coresids = c(.1,.12,.08), lv_cov = 0.5)
#'   )
#' ))
#'
#' # Named conditions and cross factors yield label columns for grouping
#' expand_pop_values_x(list(
#'   conditions = list(weak = list(loadings_p1 = c(.35,.4), loadings_p2 = c(.35,.4)),
#'                    strong = list(loadings_p1 = c(.7,.8), loadings_p2 = c(.7,.8))),
#'   lv_cov = list(low = 0.3, med = 0.5, high = 0.8)
#' ))
expand_pop_values_x <- function(pop_values_x, labels = NULL) {
  if (!is.list(pop_values_x) || is.null(names(pop_values_x)) || any(names(pop_values_x) == "")) {
    stop("`pop_values_x` must be a named list.")
  }
  if (is.null(labels)) labels <- list()

  # Classify each element
  is_bundle_list <- function(x) {
    is.list(x) && length(x) > 0 &&
      all(vapply(x, function(e) is.list(e) && !is.null(names(e)) && !inherits(e, "data.frame"),
        logical(1)))
  }
  is_cross_factor <- function(x) {
    is.list(x) && length(x) > 1 && !inherits(x, "data.frame") &&
      !all(vapply(x, function(e) is.list(e) && !is.null(names(e)), logical(1)))
  }
  is_fixed <- function(x) {
    !is.list(x) || inherits(x, "data.frame") || length(x) <= 1
  }

  # Extract conditions (bundles) if present
  conditions <- NULL
  if ("conditions" %in% names(pop_values_x)) {
    cond_val <- pop_values_x$conditions
    if (!is_bundle_list(cond_val)) {
      stop("`pop_values_x$conditions` must be a list of named lists (bundles).")
    }
    conditions <- cond_val
  }

  # Separate fixed vs cross factors (excluding conditions)
  other_names <- setdiff(names(pop_values_x), "conditions")
  fixed <- list()
  cross_factors <- list()

  for (nm in other_names) {
    val <- pop_values_x[[nm]]
    if (is_fixed(val)) {
      fixed[[nm]] <- if (is.list(val) && length(val) == 1) val[[1]] else val
    } else if (is_cross_factor(val)) {
      cross_factors[[nm]] <- val
    } else if (is_bundle_list(val)) {
      stop("Use `conditions` for bundled parameters. Found list-of-bundles in `", nm, "`.")
    } else {
      stop("Element `", nm, "` is not clearly fixed (single value/vector), ",
        "cross factor (list of values), or part of `conditions`.")
    }
  }

  # Build expansion grid
  if (length(conditions) == 0 && length(cross_factors) == 0) {
    return(tibble::tibble(pop_values_x = list(as.list(fixed))))
  }

  grid_parts <- list()
  if (length(conditions) > 0) {
    grid_parts[["_cond_idx"]] <- seq_len(length(conditions))
  }
  for (nm in names(cross_factors)) {
    grid_parts[[paste0("_cross_", nm)]] <- seq_len(length(cross_factors[[nm]]))
  }

  grid <- do.call(tidyr::expand_grid, grid_parts)

  # Build each scenario's pop_values and label columns
  build_row <- function(i) {
    row_vals <- as.list(fixed)
    if (length(conditions) > 0) {
      cond_idx <- grid[["_cond_idx"]][i]
      row_vals <- modifyList(row_vals, conditions[[cond_idx]])
    }
    for (nm in names(cross_factors)) {
      idx <- grid[[paste0("_cross_", nm)]][i]
      row_vals[[nm]] <- cross_factors[[nm]][[idx]]
    }
    row_vals
  }

  pop_list <- lapply(seq_len(nrow(grid)), build_row)
  out <- tibble::tibble(pop_values_x = pop_list)

  # Add condition_label (names win, else labels, else auto)
  if (length(conditions) > 0) {
    cond_names <- names(conditions)
    if (!is.null(cond_names) && any(nzchar(cond_names))) {
      out$condition_label <- cond_names[grid[["_cond_idx"]]]
    } else if (!is.null(labels$conditions)) {
      lab <- labels$conditions
      if (length(lab) != length(conditions)) {
        stop("`labels$conditions` length must match number of conditions.")
      }
      out$condition_label <- lab[grid[["_cond_idx"]]]
    } else {
      out$condition_label <- paste0("condition_", grid[["_cond_idx"]])
    }
  }

  # Add scalar value + optional label for each cross factor
  for (nm in names(cross_factors)) {
    cf <- cross_factors[[nm]]
    vals <- unname(cf)
    idx <- grid[[paste0("_cross_", nm)]]
    selected <- vals[idx]
    # Flatten to atomic vector when all elements are scalar
    if (all(vapply(selected, function(x) length(x) == 1L && is.atomic(x), logical(1)))) {
      out[[nm]] <- unlist(selected, recursive = FALSE)
    } else {
      out[[nm]] <- selected
    }
    # Label column
    cf_names <- names(cf)
    label_nm <- paste0(nm, "_label")
    if (!is.null(cf_names) && any(nzchar(cf_names))) {
      out[[label_nm]] <- cf_names[grid[[paste0("_cross_", nm)]]]
    } else if (!is.null(labels[[nm]])) {
      lab <- labels[[nm]]
      if (length(lab) != length(cf)) {
        stop("`labels$", nm, "` length must match number of ", nm, " levels.")
      }
      out[[label_nm]] <- lab[grid[[paste0("_cross_", nm)]]]
    }
    # If no names and no labels, omit _label column (value column is enough)
  }

  out
}
