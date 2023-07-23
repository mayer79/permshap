#' Shapley Weights
#'
#' Weights used in Shapley's formula. Vectorized over `p` and/or `ell`.
#'
#' @noRd
#' @keywords internal
#'
#' @param p Number of features.
#' @param ell Size of subset (i.e., sum of on-off vector z).
#' @returns Shapley weights.
shapley_weights <- function(p, ell) {
  1 / choose(p, ell) / (p - ell)
}

#' All on-off Vectors
#'
#' Creates matrix of all on-off vectors of length `p`. Adapted from {kernelshap}.
#'
#' @noRd
#' @keywords internal
#'
#' @param p Number of features.
#' @param feature_names Feature names.
#' @returns An integer (2^p x p) matrix of all on-off vectors of length `p`.
exact_Z <- function(p, feature_names) {
  Z <- as.matrix(do.call(expand.grid, replicate(p, 0:1, simplify = FALSE)))
  colnames(Z) <- feature_names
  Z
}

#' SHAP values for one row
#'
#' Calculates permutation SHAP values for a single row.
#'
#' @noRd
#' @keywords internal
#'
#' @inheritParams permshap
#' @param x A single row to be explained.
#' @param precalc A list with precalculated values that are identical for all rows.
#' @return A (p x K) matrix of SHAP values.
permshap_one <- function(x, object, pred_fun, bg_w, precalc, ...) {
  vz <- get_vz(                                                          #  (m_ex x K)
    X = x[rep(1L, times = nrow(precalc[["bg_X_rep"]])), , drop = FALSE], #  (m_ex*n_bg x p)
    bg = precalc[["bg_X_rep"]],                                          #  (m_ex*n_bg x p)
    Z = precalc[["Z"]],                                                  #  (m_ex x p)
    object = object,
    pred_fun = pred_fun,
    bg_w = bg_w,
    ...
  )
  rownames(vz) <- precalc[["Z_code"]]
  shapley_formula(precalc[["Z"]], vz)
}

#' Shapley's formula
#'
#' Evaluates Shapley's formula for each feature.
#'
#' @noRd
#' @keywords internal
#'
#' @param Z Matrix of on-off row vectors.
#' @param vz Named vector of vz values.
#' @returns SHAP values organized as (p x K) matrix.
shapley_formula <- function(Z, vz) {
  out <- matrix(
    nrow = ncol(Z), ncol = ncol(vz), dimnames = list(colnames(Z), colnames(vz))
  )
  for (v in colnames(Z)) {
    s1 <- Z[, v] == 1L
    vz1 <- vz[s1, , drop = FALSE]
    Z0 <- Z[s1, , drop = FALSE]
    Z0[, v] <- 0L
    s0 <- rowpaste(Z0)
    vz0 <- vz[s0, , drop = FALSE]
    w <- shapley_weights(ncol(Z), rowSums(Z0))
    out[v, ] <- wcolMeans(vz1 - vz0, w = w)
  }
  out
}

#' Masker
#'
#' For each on-off vector (rows in `Z`), the (weighted) average prediction is returned.
#' Modified from {kernelshap}.
#'
#' @noRd
#' @keywords internal
#'
#' @inheritParams permshap
#' @param X Row to be explained stacked m*n_bg times.
#' @param bg Background data stacked m times.
#' @param Z A (m x p) matrix with on-off values.
#' @returns A (m x K) matrix with vz values.
get_vz <- function(X, bg, Z, object, pred_fun, bg_w, ...) {
  m <- nrow(Z)
  not_Z <- !Z
  n_bg <- nrow(bg) / m   # because bg was replicated m times

  # Replicate not_Z, so that X, bg, not_Z are all of dimension (m*n_bg x p)
  g <- rep(seq_len(m), each = n_bg)
  not_Z <- not_Z[g, , drop = FALSE]

  if (is.matrix(X)) {
    # Remember that columns of X and bg are perfectly aligned in this case
    X[not_Z] <- bg[not_Z]
  } else {
    for (v in colnames(Z)) {
      s <- not_Z[, v]
      X[[v]][s] <- bg[[v]][s]
    }
  }
  preds <- align_pred(pred_fun(object, X, ...))

  # Aggregate
  if (is.null(bg_w)) {
    out <- rowsum(preds, group = g, reorder = FALSE) / n_bg
  } else {
    # w is recycled over rows and columns
    out <- rowsum(preds * bg_w, group = g, reorder = FALSE) / sum(bg_w)
  }
  out
}

#' Weighted colMeans
#'
#' Weighted version of [colMeans()]. Originally implemented in {kernelshap}.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A matrix.
#' @param w Optional vector of case weights.
#' @param ... Further arguments passed to [colMeans()].
#' @returns Column means of `x` (organized as matrix with one row).
wcolMeans <- function(x, w = NULL, ...) {
  if (!is.matrix(x)) {
    stop("x must be a matrix")
  }
  if (is.null(w)) {
    out <- colMeans(x, ...)
  } else {
    if (nrow(x) != length(w)) {
      stop("Weights w not compatible with matrix x")
    }
    out <- colSums(x * w, ...) / sum(w)
  }
  matrix(out, nrow = 1L)
}

#' Combine Matrices
#'
#' Binds list of matrices along new first axis. Originally implemented in {kernelshap}.
#'
#' @noRd
#' @keywords internal
#'
#' @param a List of n (p x K) matrices.
#' @returns A (n x p x K) array.
abind1 <- function(a) {
  out <- array(
    dim = c(length(a), dim(a[[1L]])),
    dimnames = c(list(NULL), dimnames(a[[1L]]))
  )
  for (i in seq_along(a)) {
    out[i, , ] <- a[[i]]
  }
  out
}

#' Reorganize List
#'
#' Turns list of n (p x K) matrices into list of K (n x p) matrices. Reduce if K = 1.
#' Originally implemented in {kernelshap}.
#'
#' @noRd
#' @keywords internal
#'
#' @param alist List of n (p x K) matrices.
#' @returns List of K (n x p) matrices.
reorganize_list <- function(alist) {
  if (!is.list(alist)) {
    stop("alist must be a list")
  }
  out <- abind1(alist)
  out <- asplit(out, MARGIN = 3L)
  if (length(out) == 1L) {
    return(as.matrix(out[[1L]]))
  }
  lapply(out, as.matrix)
}

#' Aligns Predictions
#'
#' Turns predictions into matrix. Originally implemented in {hstats}.
#'
#' @noRd
#' @keywords internal
#'
#' @param x Object representing model predictions.
#' @returns Like `x`, but converted to matrix.
align_pred <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
    stop("Predictions must be numeric")
  }
  x
}

#' Head of List Elements
#'
#' Returns top n rows of each element in the input list.
#' Originally implemented in {kernelshap}.
#'
#' @noRd
#' @keywords internal
#'
#' @param x A list or a matrix-like.
#' @param n Number of rows to show.
#' @returns List of first rows of each element in the input.
head_list <- function(x, n = 6L) {
  if (!is.list(x)) utils::head(x, n) else lapply(x, utils::head, n)
}

#' Rowwise Paste
#'
#' Fast version of `apply(Z, 1L, FUN = paste0, collapse = "")`.
#'
#' @noRd
#' @keywords internal
#'
#' @param Z A (n x p) matrix.
#' @returns A length n vector.
rowpaste <- function(Z) {
  do.call(paste0, asplit(Z, 2L))
}

