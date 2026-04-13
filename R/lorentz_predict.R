#' Predict from a Lorentzian fit
#'
#' Computes predicted Lorentzian values at new positions using parameters
#' from a fitted \code{"LorentzFit"} object. NA/NaN values in x propagate
#' to output.
#'
#' @param fit An S3 object of class \code{"LorentzFit"} (output of
#'   \code{\link{lorentz_fit}}).
#' @param x Numeric vector of positions/frequencies at which to predict.
#' @return Numeric vector of predicted values, same length as x.
#' @export
#' @examples
#' set.seed(100)
#' x <- seq(-10, 10, length.out = 200)
#' y <- 5 * 1.5^2 / ((x - 2)^2 + 1.5^2) + rnorm(200, sd = 0.1)
#' fit <- lorentz_fit(x, y)
#' lorentz_predict(fit, seq(-5, 5, by = 0.5))
lorentz_predict <- function(fit, x) {
  stopifnot(
    inherits(fit, "LorentzFit"),
    is.numeric(x) # NOTE: Not-finites are OK here.
  )

  .lorentz_fn(x, fit$f0, fit$gamma, fit$a)
}
