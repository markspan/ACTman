#' score_epochs
#'
#' Computes epoch-by-epoch wake/sleep and mobile/immobile classifications
#' for a night's activity data, plus the "sleep chance" / "wakeup chance"
#' rolling indicators used to locate sleep onset and sleep offset. Pure
#' function: takes a data frame in, returns it with additional columns, with
#' no file I/O or side effects.
#'
#' ## Background
#'
#' The wake/sleep \code{score} is a neighbor-weighted combination of the
#' current epoch's activity count and its immediate neighbors (2 minutes on
#' either side, with the closer neighbor weighted more heavily), thresholded
#' at 20 ("Medium Sensitivity" in the convention used by several actigraphy
#' scoring algorithms of this family; 40 would be "Low Sensitivity", 20 "High
#' Sensitivity"). This is the same general family of approach as the
#' Cole-Kripke algorithm for actigraphic sleep scoring.
#'
#' \code{sleep.chance} and \code{wakeup.chance} are simple rolling sums of a
#' binarized "was activity above threshold" indicator over the next/previous
#' 9 and 4 epochs respectively, used downstream to locate the first
#' sustained quiet period after bedtime (sleep onset) and the first
#' sustained active period before/after the scheduled wake time (sleep
#' offset).
#'
#' @references
#' Cole, R. J., Kripke, D. F., Gruen, W., Mullaney, D. J., & Gillin, J. C.
#' (1992). Automatic sleep/wake identification from wrist activity.
#' \emph{Sleep}, 15(5), 461-469.
#'
#' @param aaa A data frame for one night's data, with an
#'   \code{Activity..MW.counts.} column (numeric activity counts).
#'
#' @return The same data frame with added columns: \code{score},
#'   \code{WakeSleep} (1 = awake, 0 = asleep), \code{MobileImmobile} (1 =
#'   mobile, 0 = immobile), \code{epoch.sleep.chance}, \code{sleep.chance},
#'   and \code{wakeup.chance}.
#'
#' @importFrom dplyr mutate lag lead
#' @export
score_epochs <- function(aaa) {
  aaa <- dplyr::mutate(aaa, score = (
    (dplyr::lag(Activity..MW.counts., n = 1L, default = 0) / 5) +
      (dplyr::lag(Activity..MW.counts., n = 2L, default = 0) / 25) +
      (dplyr::lead(Activity..MW.counts., n = 1L, default = 0) / 5) +
      (dplyr::lead(Activity..MW.counts., n = 2L, default = 0) / 25) +
      Activity..MW.counts.))

  ## NOTE: 40 is 'Medium Sensitivity', 20 is 'High Sensitivity'
  aaa$WakeSleep <- ifelse(aaa$score > 20, 1, 0) # 1 is awake, 0 is asleep
  aaa$MobileImmobile <- ifelse(aaa$Activity..MW.counts. > 3, 1, 0) # 1 is mobile, 0 is immobile

  ## Calculation should indicate the moment of sleep start: 10 consecutive
  ## non-active epochs, allowing 1 active epoch within that span.
  aaa$epoch.sleep.chance <- ifelse(aaa$Activity..MW.counts. > 6, 1, 0) # 1 is above threshold, 0 is below threshold
  aaa$sleep.chance <- (dplyr::lead(aaa$epoch.sleep.chance, n = 1L) +
                        dplyr::lead(aaa$epoch.sleep.chance, n = 2L) +
                        dplyr::lead(aaa$epoch.sleep.chance, n = 3L) +
                        dplyr::lead(aaa$epoch.sleep.chance, n = 4L) +
                        dplyr::lead(aaa$epoch.sleep.chance, n = 5L) +
                        dplyr::lead(aaa$epoch.sleep.chance, n = 6L) +
                        dplyr::lead(aaa$epoch.sleep.chance, n = 7L) +
                        dplyr::lead(aaa$epoch.sleep.chance, n = 8L) +
                        dplyr::lead(aaa$epoch.sleep.chance, n = 9L) +
                        aaa$epoch.sleep.chance)

  aaa$wakeup.chance <- (dplyr::lag(aaa$epoch.sleep.chance, n = 1L) +
                         dplyr::lag(aaa$epoch.sleep.chance, n = 2L) +
                         dplyr::lag(aaa$epoch.sleep.chance, n = 3L) +
                         dplyr::lag(aaa$epoch.sleep.chance, n = 4L) +
                         aaa$epoch.sleep.chance)

  aaa
}
