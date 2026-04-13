#' Lorentzian profile value
#'
#' Internal helper. Computes Lorentzian profile values using the formula:
#' \eqn{a \cdot (\gamma/2)^2 / ((x - f_0)^2 + (\gamma/2)^2)}.
#'
#' @param x Numeric vector of positions/frequencies.
#' @param f0 Center position of the peak.
#' @param gamma Full width at half maximum (FWHM).
#' @param a Peak amplitude (value at x = f0).
#' @return Numeric vector of the same length as x.
#' @keywords internal
.lorentz_fn <- function(x, f0, gamma, a) {
  a * (gamma / 2)^2 / ((x - f0)^2 + (gamma / 2)^2)
}
