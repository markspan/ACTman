#' ews_metrics
#'
#' Calculates a set of distributional and time-series "early warning signal"
#' (EWS) statistics on an activity series that has already been windowed to
#' the period of interest (see \code{\link{nparcalc}}). Pure function: no
#' file I/O, no printing, no global state.
#'
#' ## Background
#'
#' Early warning signals are statistical indicators, originally developed in
#' the study of complex dynamical systems (e.g. ecosystem collapse), that can
#' precede a critical transition -- rising variance, rising autocorrelation,
#' and slower recovery from perturbation ("critical slowing down") are
#' classic examples. They have been explored as possible early indicators of
#' impending state transitions in mood or health (e.g. an approaching
#' depressive episode); Helmich et al. (2024) provide a critical
#' perspective on this literature, arguing that current empirical and
#' theoretical support for using early-warning-signal-based critical
#' slowing down to predict changes in mental health symptoms is limited,
#' and that these limitations should be kept in mind when interpreting the
#' EWS statistics below.
#'
#' @references
#' Helmich, M. A., Schreuder, M. J., Bringmann, L. F., Riese, H., Snippe,
#' E., & Smit, A. C. (2024). Slow down and be critical before using early
#' warning signals in psychopathology. \emph{Nature Reviews Psychology},
#' 3, 767-780.
#'
#' @param CRV.data A data frame with (at least) an \code{Activity} column,
#'   already windowed to the period of interest.
#'
#' @return A named list with elements \code{Mean}, \code{Variance}, \code{SD},
#'   \code{CoV} (coefficient of variation, \%), \code{Skewness},
#'   \code{Kurtosis}, \code{Autocorr} (lag-1) / \code{Autocorr_lag2} /
#'   \code{Autocorr_lag3} / \code{Autocorr_lag60} / \code{Autocorr_lag120},
#'   and \code{Time_to_Recovery} (the first lag, in minutes, at which the
#'   autocorrelation function drops below 0.2; capped at
#'   \code{MAX_AUTOCORR_LAG} if it never does within the computed range).
#'
#' @importFrom stats na.pass sd var acf
#' @importFrom moments skewness kurtosis
#' @export
ews_metrics <- function(CRV.data) {
  result <- list()
  activity <- CRV.data[, "Activity"]

  result$Mean <- round(mean(activity, na.rm = TRUE), 2)
  result$Variance <- round(var(activity, na.rm = TRUE), 2)
  result$SD <- round(sd(activity, na.rm = TRUE), 2)
  result$CoV <- round((sd(activity, na.rm = TRUE) / mean(activity, na.rm = TRUE)) * 100, 2)
  result$Skewness <- round(moments::skewness(activity, na.rm = TRUE), 2)
  result$Kurtosis <- round(moments::kurtosis(activity, na.rm = TRUE), 2)

  Autocorr <- acf(x = activity, lag.max = MAX_AUTOCORR_LAG, na.action = na.pass, plot = FALSE)
  result$Autocorr <- round(Autocorr$acf[2], 2)
  result$Autocorr_lag2 <- round(Autocorr$acf[3], 2)
  result$Autocorr_lag3 <- round(Autocorr$acf[4], 2)
  result$Autocorr_lag60 <- round(Autocorr$acf[61], 2)
  result$Autocorr_lag120 <- round(Autocorr$acf[121], 2)

  if (length(which(Autocorr$acf < 0.2)) != 0) {
    result$Time_to_Recovery <- which(Autocorr$acf < 0.2)[1]
  } else {
    result$Time_to_Recovery <- MAX_AUTOCORR_LAG
  }

  result
}
