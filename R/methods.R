#' Print Method
#'
#' Prints the first `n` rows of the matrix (or matrices) of SHAP values.
#'
#' @param x An object of class "permshap".
#' @param n Maximum number of rows of SHAP values to print.
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' s <- permshap(fit, iris[1:3, -1], bg_X = iris[-1])
#' s
#' @seealso [permshap()]
print.permshap <- function(x, n = 2L, ...) {
  cat("SHAP values of first", n, "observations:\n")
  print(head_list(x[["S"]], n = n))
  invisible(x)
}

#' Check for permshap
#'
#' Is object of class "permshap"?
#'
#' @param object An R object.
#' @returns `TRUE` if `object` is of class "permshap", and `FALSE` otherwise.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' s <- permshap(fit, iris[1:2, -1], bg_X = iris[-1])
#' is.permshap(s)
#' is.permshap("a")
#' @seealso [permshap()]
is.permshap <- function(object){
  inherits(object, "permshap")
}
