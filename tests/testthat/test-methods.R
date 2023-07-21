fit <- lm(Sepal.Length ~ ., data = iris)
s <- permshap(fit, iris[1:2, -1L], bg_X = iris[-1L], verbose = FALSE)

test_that("is_permshap() works", {
  expect_true(is.permshap(s))
  expect_false(is.permshap(1))
})

test_that("print() does not give an error", {
  capture_output(expect_no_error(print(s)))
})

