#' Plot Simulation Results
#'
#' Visualization helpers for raw simulation output.
#'
#' @param data Tibble of raw results (from \code{run_simulation} or \code{run_one_scenario}).
#' @param x Variable for x-axis (e.g. \code{n_dyads}).
#' @param y Variable for y-axis (e.g. \code{bias}, \code{coverage}).
#' @param param Parameter to filter (from estimates).
#' @return A \code{ggplot2} object (if \pkg{ggplot2} installed).
#' @family visualization
#' @export
plot_bias <- function(data, x = "n_dyads", y = "bias", param = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2 for plotting.")
  est <- data$estimates
  if (is.null(est)) stop("No estimates column.")
  est_df <- dplyr::bind_rows(est, .id = "rep")
  if (nrow(est_df) == 0) return(NULL)
  est_df$rep <- as.integer(est_df$rep)
  meta <- data[, setdiff(names(data), "estimates"), drop = FALSE]
  meta$rep <- seq_len(nrow(meta))
  merged <- dplyr::left_join(est_df, meta, by = "rep")
  if (!is.null(param)) {
    match_lhs_rhs <- nchar(param) > 0 && grepl("~~", param, fixed = TRUE)
    if (match_lhs_rhs) {
      parts <- strsplit(param, "~~", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        merged <- merged[merged$lhs == parts[1] & merged$rhs == parts[2], ]
      }
    } else {
      merged <- merged[merged$label == param | merged$lhs == param, ]
    }
  }
  if (!x %in% names(merged)) x <- "rep"
  ggplot2::ggplot(merged, ggplot2::aes(.data[[x]], .data[["est"]])) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::stat_summary(ggplot2::aes(y = .data[["est"]]), fun = mean, geom = "line", colour = "blue")
}

#' @rdname plot_bias
#' @export
plot_coverage <- function(data, x = "n_dyads", param = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2 for plotting.")
  cov_col <- data$coverage
  if (is.null(cov_col)) stop("No coverage column. Run with extract = c(\"estimates\", \"coverage\") and true_values.")
  cov_list <- lapply(seq_along(cov_col), function(i) {
    z <- cov_col[[i]]
    if (is.null(z)) return(NULL)
    out <- tibble::enframe(z, "param", "covered")
    out$rep <- i
    out
  })
  cov_df <- dplyr::bind_rows(cov_list)
  if (is.null(cov_df) || nrow(cov_df) == 0) return(NULL)
  meta <- data[, setdiff(names(data), "coverage"), drop = FALSE]
  meta$rep <- seq_len(nrow(meta))
  merged <- dplyr::left_join(cov_df, meta, by = "rep")
  if (!is.null(param)) merged <- merged[merged$param == param, ]
  ggplot2::ggplot(merged, ggplot2::aes(.data[[x]], .data[["covered"]])) +
    ggplot2::stat_summary(fun = mean, geom = "col") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0.95), linetype = 2)
}

#' @rdname plot_bias
#' @export
plot_power <- function(data, x = "n_dyads", alpha = 0.05, param = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2 for plotting.")
  est <- data$estimates
  if (is.null(est)) stop("No estimates column.")
  est_df <- dplyr::bind_rows(est, .id = "rep")
  if (nrow(est_df) == 0) return(NULL)
  est_df$rep <- as.integer(est_df$rep)
  meta <- data[, setdiff(names(data), "estimates"), drop = FALSE]
  meta$rep <- seq_len(nrow(meta))
  merged <- dplyr::left_join(est_df, meta, by = "rep")
  if (!is.null(param)) merged <- merged[merged$label == param | paste0(merged$lhs, "~~", merged$rhs) == param, ]
  grp_cols <- setdiff(names(merged), c("est", "se", "z", "pvalue", "ci.lower", "ci.upper"))
  rej <- dplyr::summarise(
    dplyr::group_by(merged, dplyr::across(dplyr::any_of(grp_cols))),
    rejection_rate = mean(.data$pvalue < alpha, na.rm = TRUE),
    .groups = "drop"
  )
  ggplot2::ggplot(rej, ggplot2::aes(.data[[x]], .data[["rejection_rate"]])) +
    ggplot2::geom_col()
}

#' Plot Model Choice Sensitivity and Specificity
#'
#' Plot sensitivity and specificity from \code{run_modelchoice_analysis} output.
#'
#' @param data Output from \code{run_modelchoice_analysis}, either \code{raw} or \code{summary}.
#' @param x Variable for x-axis (e.g. \code{"n_dyads"}, \code{"pop_model_x"}).
#' @param facets Character vector of variables to facet by (e.g. \code{"criterion"} or
#'   \code{c("criterion", "n_dyads")}). Pass \code{NULL} for no faceting.
#' @return A \code{ggplot2} object.
#' @family visualization
#' @export
#' @examples
#' # Single facet
#' plot_modelchoice(results, x = "pop_model_x", facets = "criterion")
#'
#' # Two facets (criterion and n_dyads)
#' plot_modelchoice(results, x = "pop_model_x", facets = c("criterion", "n_dyads"))
plot_modelchoice <- function(data, x = "n_dyads", facets = "criterion") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2 for plotting.")

  if ("raw" %in% names(data) && "summary" %in% names(data)) {
    data <- data$summary
  }

  if (!"sensitivity" %in% names(data)) {
    warning("Data does not have sensitivity column. Use output from run_modelchoice_analysis.")
    return(NULL)
  }

  # Reshape to long for sensitivity and specificity
  has_spec <- "specificity" %in% names(data)
  if (has_spec) {
    plot_dat <- tidyr::pivot_longer(data,
      cols = c("sensitivity", "specificity"),
      names_to = "metric", values_to = "value"
    )
  } else {
    plot_dat <- data
    plot_dat$metric <- "sensitivity"
    plot_dat$value <- plot_dat$sensitivity
  }

  x_avail <- x %in% names(plot_dat)
  if (!x_avail) x <- "pop_model_x"

  p <- ggplot2::ggplot(plot_dat, ggplot2::aes(.data[[x]], .data[["value"]], fill = .data[["metric"]])) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9), width = 0.8)

  if (!is.null(facets) && length(facets) > 0) {
    facet_vars <- intersect(facets, names(plot_dat))
    if (length(facet_vars) > 0) {
      p <- p + ggplot2::facet_wrap(
        ggplot2::vars(!!!rlang::syms(facet_vars)),
        ncol = NULL
      )
    }
  }

  p + ggplot2::labs(y = "Proportion", fill = "") +
    ggplot2::theme(legend.position = "bottom")
}
