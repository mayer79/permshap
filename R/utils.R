shapley_weights <- function(p, ell) {
  1 / choose(p, ell) / (p - ell)
}

exact_Z <- function(p, feature_names) {
  Z <- as.matrix(do.call(expand.grid, replicate(p, 0:1, simplify = FALSE)))
  colnames(Z) <- feature_names
  Z
}

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

shapley_formula <- function(Z, vz) {
  out <- matrix(
    nrow = ncol(Z), ncol = ncol(vz), dimnames = list(colnames(Z), colnames(vz))
  )
  for (v in colnames(Z)) {
    s1 <- Z[, v] == 1L
    vz1 <- vz[s1, , drop = FALSE]
    Z0 <- Z[s1, , drop = FALSE]
    Z0[, v] <- 0L
    s0 <- apply(Z0, 1L, FUN = paste0, collapse = "")
    vz0 <- vz[s0, , drop = FALSE]
    w <- shapley_weights(ncol(Z), rowSums(Z0))
    out[v, ] <- wcolMeans(vz1 - vz0, w = w)
  }
  out
}

# Calculates all vz of an iteration by a single call to predict()
get_vz <- function(X, bg, Z, object, pred_fun, bg_w, ...) {
  m <- nrow(Z)
  not_Z <- !Z
  rownames(not_Z) <- NULL
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
  rownames(out) <- rownames(Z)
  out
}

# Weighted colMeans(). Always returns a (1 x ncol(x)) matrix
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

# Binds list of matrices along new first axis
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

# Turn list of n (p x K) matrices into list of K (n x p) matrices. Reduce if K = 1.
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

# Turns predictions into matrix
align_pred <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
    stop("Predictions must be numeric")
  }
  x
}

# Helper function in print() and summary()
# x is either a matrix or a list of matrices
head_list <- function(x, n = 6L) {
  if (!is.list(x)) utils::head(x, n) else lapply(x, utils::head, n)
}
