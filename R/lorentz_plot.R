#' Plot a Lorentzian fit
#'
#' Plots original data points and the fitted Lorentzian curve using ggplot2.
#'
#' @param fit An object of class \code{"LorentzFit"} (output of
#'   \code{\link{lorentz_fit}}).
#' @return A \code{ggplot} object.
#' @export
#' @importFrom rlang .data
#' @examples
#' set.seed(100)
#' x <- seq(-10, 10, length.out = 200)
#' y <- 5 * 1.5^2 / ((x - 2)^2 + 1.5^2) + rnorm(200, sd = 0.1)
#' fit <- lorentz_fit(x, y)
#' lorentz_plot(fit)
lorentz_plot <- function(fit) {
  stopifnot(inherits(fit, "LorentzFit"))

  x_smooth <- seq(min(fit$original_x), max(fit$original_x), length.out = 500)
  y_smooth <- lorentz_predict(fit, x_smooth)

  df_data <- data.frame(x = fit$original_x, y = fit$original_y)
  df_fit <- data.frame(x = x_smooth, y = y_smooth)

  ggplot2::ggplot() +
    ggplot2::geom_point(data = df_data, ggplot2::aes(x = .data$x, y = .data$y),
                        color = "grey40", size = 1) +
    ggplot2::geom_line(data = df_fit, ggplot2::aes(x = .data$x, y = .data$y),
                       color = "red", linewidth = 0.8) +
    ggplot2::labs(x = "Frequency", y = "Intensity",
                  title = "Lorentzian fit") +
    ggplot2::theme_minimal()
}
