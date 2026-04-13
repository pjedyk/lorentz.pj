#' Spectroscopic scan data
#'
#' A single spectroscopic scan from a Paul trap experiment.
#'
#' @docType data
#' @usage data(spectra)
#' @format A data frame with columns:
#' \describe{
#'   \item{freq}{Scan frequencies in Hz.}
#'   \item{readout}{Fluorescence readout values.}
#' }
#' @source Experimental data from a Paul trap spectroscopy setup.
#' Collected on 2025-05-23.
#' @keywords datasets
#' @examples
#' data(spectra)
#' plot(spectra$freq, spectra$readout)
"spectra"
