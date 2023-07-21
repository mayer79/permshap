
# Helper functions
test_that("head_list(x) = head(x) for matrix x", {
  x <- cbind(1:10, 2:11)
  expect_equal(head_list(x), utils::head(x))
})

test_that("head_list(x)[[1L]] = head(x[[1L]]) for list of matries x", {
  x1 <- cbind(1:10, 2:11)
  x2 <- cbind(1:7, 2:8)
  x <- list(x1, x2)
  expect_equal(head_list(x)[[1L]], utils::head(x[[1L]]))
})

test_that("regoranize_list() fails for non-list inputs", {
  expect_error(reorganize_list(alist = 1:10, nms = NULL))
})

test_that("wcolMeans() fails for non-matrix inputs", {
  expect_error(wcolMeans(1))
})

test_that("wcolMeans() fails if weights are not long enough", {
  X <- cbind(1:3, 2:4)
  expect_error(wcolMeans(X, w = 1))
  expect_error(wcolMeans(X, w = 1:2))
})

test_that("wcolMeans() gives the same as colMeans() in unweighted case", {
  X <- cbind(1:3, 2:4)
  expect_equal(c(wcolMeans(X)), colMeans(X))
  expect_equal(c(wcolMeans(X, w = c(1, 1, 1))), colMeans(X))
  expect_equal(c(wcolMeans(X, w = c(2, 2, 2))), colMeans(X))
})

