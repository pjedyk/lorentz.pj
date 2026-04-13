test_that("lorentz_predict() input (types)", {
  set.seed(100)
  x_test <- seq(-10, 10, length.out = 200)
  y_test <- .lorentz_fn(x_test, f0 = 2, gamma = 3, a = 5)
  y_noisy <- y_test + rnorm(length(y_test), sd = 0.2)
  fit <- lorentz_fit(x_test, y_noisy)

  expect_error(lorentz_predict(list(a = 1), c(1, 2, 3)))
  expect_error(lorentz_predict(fit, c("1", "2", "3")))
})

test_that("lorentz_predict() output (types)", {
  set.seed(100)
  x_test <- seq(-10, 10, length.out = 200)
  y_test <- .lorentz_fn(x_test, f0 = 2, gamma = 3, a = 5)
  y_noisy <- y_test + rnorm(length(y_test), sd = 0.2)
  fit <- lorentz_fit(x_test, y_noisy)
  model <- lorentz_predict(fit, x_test)

  expect_true(is.numeric(model))
  expect_length(model, length(x_test))
})

test_that("lorentz_fit()/lorentz_predict() round-trip", {
  set.seed(100)
  x_test <- seq(-10, 10, length.out = 200)
  y_test <- .lorentz_fn(x_test, f0 = 2, gamma = 3, a = 5)
  y_noisy <- y_test + rnorm(length(y_test), sd = 0.2)
  fit <- lorentz_fit(x_test, y_noisy)
  model <- lorentz_predict(fit, x_test)

  expect_equal(model, y_test, tolerance = 0.1)
})

test_that("lorentz_predict() shape", {
  set.seed(100)
  x_test <- seq(-10, 10, length.out = 200)
  y_test <- .lorentz_fn(x_test, f0 = 2, gamma = 3, a = 5)
  y_noisy <- y_test + rnorm(length(y_test), sd = 0.2)
  fit <- lorentz_fit(x_test, y_noisy)

  expect_equal(lorentz_predict(fit, fit$f0), fit$a, tolerance = 1e-4)
  expect_equal(
    lorentz_predict(fit, fit$f0 + 1.5),
    lorentz_predict(fit, fit$f0 - 1.5),
    tolerance = 1e-4
  )
})

test_that("lorentz_predict() fwhm test", {
  set.seed(100)
  x_test <- seq(-10, 10, length.out = 200)
  y_test <- .lorentz_fn(x_test, f0 = 2, gamma = 3, a = 5)
  y_noisy <- y_test + rnorm(length(y_test), sd = 0.2)
  fit <- lorentz_fit(x_test, y_noisy)

  expect_equal(
    lorentz_predict(fit, fit$f0 + fit$gamma / 2),
    fit$a / 2,
    tolerance = 1e-4
  )
})
