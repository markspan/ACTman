#' circadian_metrics
#'
#' Calculates the non-parametric circadian rhythm variables IS, IV, RA, L5
#' (with its start time), and M10 (with its start time) from an activity
#' series that has already been windowed to the period of interest (see
#' \code{\link{nparcalc}}, which handles device-specific windowing and calls
#' this function). This is a pure function: it does not read files, print
#' progress, or depend on global state.
#'
#' ## Background
#'
#' These are standard non-parametric measures of rest-activity rhythms used
#' in chronobiology and circadian rhythm research:
#'
#' - **IS (Interdaily Stability)**: quantifies how similar the 24-hour
#'   activity pattern is from day to day. Ranges from 0 (no stable pattern)
#'   towards 1 (highly stable/repeated pattern). Computed as the ratio of the
#'   variance of the average 24-hour pattern to the overall variance.
#' - **IV (Interdaily Variability)**: quantifies the fragmentation of the
#'   rhythm, i.e. how much activity changes from one hour to the next
#'   relative to the day-to-day variance. Higher values indicate a more
#'   fragmented rhythm.
#' - **L5**: the average activity level during the 5 (clock-)hour period of
#'   lowest activity in the average 24-hour profile, with \code{L5_starttime}
#'   marking when that period begins. Represents the "trough" of the rhythm
#'   (typically nocturnal sleep).
#' - **M10**: the average activity level during the 10 (clock-)hour period of
#'   highest activity, with \code{M10_starttime} marking when it begins.
#'   Represents the "peak" of the daytime activity period.
#' - **RA (Relative Amplitude)**: \code{(M10 - L5) / (M10 + L5)}, a
#'   normalized measure of the day/night activity contrast. Ranges from 0
#'   (no contrast) to 1 (maximal contrast).
#'
#' @references
#' Van Someren, E. J. W., Swaab, D. F., Colenda, C. C., Cohen, W., McCall,
#' W. V., & Rosenquist, P. B. (1999). Bright light therapy: improved
#' sensitivity to its effects on rest-activity rhythms in Alzheimer patients
#' by application of nonparametric methods. \emph{Chronobiology
#' International}, 16(4), 505-518.
#'
#' Witting, W., Kwa, I. H., Eikelenboom, P., Mirmiran, M., & Swaab, D. F.
#' (1990). Alterations in the circadian rest-activity rhythm in aging and
#' Alzheimer's disease. \emph{Biological Psychiatry}, 27(6), 563-572.
#'
#' @param CRV.data A data frame with (at least) \code{Date} and
#'   \code{Activity} columns, already windowed to the period of interest and
#'   ordered chronologically at 1-minute resolution.
#' @param movingwindow Boolean. Whether this call is part of a moving-window
#'   analysis; affects whether the final aggregated hour is trimmed (kept for
#'   exact behavioral parity with the original combined implementation).
#'
#' @return A named list with elements \code{IS}, \code{IV}, \code{RA},
#'   \code{L5}, \code{L5_starttime}, \code{M10}, \code{M10_starttime}.
#'
#' @importFrom stats na.pass aggregate
#' @importFrom utils tail
#' @export
circadian_metrics <- function(CRV.data, movingwindow = FALSE) {
  result <- list()

  ## IS: Interdaily Stability -------------------------------------------
  xi <- aggregate(CRV.data[, "Activity"],
    list(hour = cut(as.POSIXct(CRV.data[, "Date"]), breaks = "hour")),
    mean, na.action = na.pass, na.rm = TRUE)

  ## Exception for moving window to retain last observation for correct number of observations:
  if (!movingwindow) {
    xi <- xi[seq_len(nrow(xi) - 1), ]
  } else {
    xi <- xi[seq_len(nrow(xi)), ]
  }
  xi <- xi$x

  X <- mean(xi, na.rm = TRUE)

  xi_X <- xi - X
  sq.xi_X <- xi_X^2
  sum.sq.xi_X <- sum(sq.xi_X, na.rm = TRUE)
  n <- sum(!is.na(xi))
  sum.sq.xi_X.perhour <- sum.sq.xi_X / n

  xi_sub <- xi[1:(24 * floor(length(xi) / 24))]
  Xh <- rowMeans(matrix(xi_sub, nrow = 24), na.rm = TRUE)
  Xh_X <- Xh - X
  sum.sq.Xh_X <- sum(Xh_X^2, na.rm = TRUE)
  sum.sq.Xh_X.perhour <- sum.sq.Xh_X / 24

  result$IS <- round(sum.sq.Xh_X.perhour / sum.sq.xi_X.perhour, 2)

  ## IV: Interdaily Variability ------------------------------------------
  Xi_diffXi <- diff(xi)
  sum.sq.Xi_diffXi <- sum(Xi_diffXi^2, na.rm = TRUE)
  sum.sq.Xi_diffXi.perhour <- sum.sq.Xi_diffXi / (n - 1)

  Xi_X <- xi - X
  sum.sq.Xi_X <- sum(Xi_X^2, na.rm = TRUE)
  sum.sq.Xi_X.perhour <- sum.sq.Xi_X / n

  result$IV <- round(sum.sq.Xi_diffXi.perhour / sum.sq.Xi_X.perhour, 2)

  ## Average-day profile, used by both L5 and M10 ------------------------
  averageday <- matrix(c(substr(CRV.data[1:MINUTES_PER_DAY, "Date"], 14, 22), rep(NA, MINUTES_PER_DAY)),
    nrow = MINUTES_PER_DAY, ncol = 2)
  selection_mat <- matrix(FALSE, MINUTES_PER_DAY)
  for (aa in 1:MINUTES_PER_DAY) {
    selection_mat[aa, ] <- TRUE
    averageday[aa, 2] <- mean(CRV.data[selection_mat, "Activity"], na.rm = TRUE)
    selection_mat <- matrix(FALSE, MINUTES_PER_DAY)
  }
  averageday[, 2] <- as.numeric(averageday[, 2])

  ## Duplicated so a window can cross the midnight point when scanning for
  ## the lowest/highest contiguous span (see L5/M10 loops below).
  averageday <- rbind(averageday, averageday)

  ## L5: Average of the 5 Lowest Hourly Means ----------------------------
  averageday_loc_L5 <- matrix(NA, nrow(averageday))
  for (gg in 1:(nrow(averageday) - L5_WINDOW_MINUTES)) {
    averageday_loc_L5[gg] <- mean(as.numeric(averageday[c(gg:(L5_WINDOW_MINUTES - 1 + gg)), 2]))
  }
  result$L5_starttime <- averageday[which.min(averageday_loc_L5), 1]
  result$L5 <- round(averageday_loc_L5[which.min(averageday_loc_L5)], 2)

  ## M10: Average of the 10 Highest Hourly Means -------------------------
  averageday_loc_M10 <- matrix(NA, nrow(averageday))
  for (hh in 1:(nrow(averageday) - M10_WINDOW_MINUTES)) {
    averageday_loc_M10[hh] <- mean(as.numeric(averageday[c(hh:(M10_WINDOW_MINUTES - 1 + hh)), 2]), na.rm = TRUE)
  }
  result$M10_starttime <- averageday[which.max(averageday_loc_M10), 1]
  result$M10 <- round(averageday_loc_M10[which.max(averageday_loc_M10)], 2)

  ## RA: Relative Amplitude ----------------------------------------------
  Amp <- result$M10 - result$L5
  result$RA <- Amp / (result$L5 + result$M10)

  result
}
