#' Fit a Lorentzian curve
#'
#' Fits a Lorentzian curve to observed (x, y) data using nonlinear least
#' squares. If initial parameter guesses are not provided, they are estimated
#' automatically from the data.
#'
#' @param x Numeric vector of positions/frequencies. Must be finite.
#' @param y Numeric vector of observed values. Same length as x, must be finite.
#' @param f0_init Initial guess for peak center (scalar), or NULL to estimate.
#' @param gamma_init Initial guess for FWHM (scalar), or NULL to estimate.
#' @param a_init Initial guess for amplitude (scalar), or NULL to estimate.
#' @return An S3 object of class \code{"LorentzFit"} containing:
#'   \describe{
#'     \item{f0}{Fitted peak center.}
#'     \item{gamma}{Fitted FWHM.}
#'     \item{a}{Fitted amplitude.}
#'     \item{residuals}{Residuals (observed - fitted).}
#'     \item{r2}{Coefficient of determination.}
#'     \item{original_x}{Input x vector.}
#'     \item{original_y}{Input y vector.}
#'   }
#' @export
#' @importFrom stats nls coef residuals
#' @examples
#' set.seed(100)
#' x <- seq(-10, 10, length.out = 200)
#' y <- 5 * 1.5^2 / ((x - 2)^2 + 1.5^2) + rnorm(200, sd = 0.1)
#' fit <- lorentz_fit(x, y)
#' fit$f0
#' fit$r2
lorentz_fit <- function(x, y, f0_init = NULL, gamma_init = NULL, a_init = NULL) {
  stopifnot(
    is.numeric(x), all(is.finite(x)),
    is.numeric(y), all(is.finite(y)),
    length(x) == length(y), # NOTE: Recycling is not applicable here.
    length(x) > 0
  )

  if (is.null(f0_init)) {
    f0_init <- x[which.max(y)]
  }
  if (is.null(gamma_init)) {
    gamma_init <- (max(x) - min(x)) / 3
  }
  if (is.null(a_init)) {
    a_init <- max(y) - min(y)
  }

  stopifnot(
    is.numeric(f0_init), all(is.finite(f0_init)), length(f0_init) == 1,
    is.numeric(gamma_init), all(is.finite(gamma_init)), length(gamma_init) == 1,
    is.numeric(a_init), all(is.finite(a_init)), length(a_init) == 1
  )

  # NOTE: The nls() function already throws an error if it fails to converge;
  # however, we use tryCatch() here to modify the message for clarity.
  fit <- tryCatch(
    nls(
      y ~ .lorentz_fn(x, f0, gamma, a),
      start = list(f0 = f0_init, gamma = gamma_init, a = a_init)
    ),
    error = function(e) {
      stop("Lorentz fit failed to converge: ", e$message)
    }
  )

  result <- list(
    f0 = coef(fit)[["f0"]],
    gamma = coef(fit)[["gamma"]],
    a = coef(fit)[["a"]],
    residuals = residuals(fit),
    r2 = 1 - sum(residuals(fit)^2) / sum((y - mean(y))^2),
    original_x = x,
    original_y = y
  )
  attr(result, "nobs") <- length(x)
  class(result) <- "LorentzFit"

  result
}
