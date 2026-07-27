#############################################################################
### Shared constants for ACTman                                          ###
#############################################################################

#' Minutes in a day, for a 1-minute-epoch actigraphy recording.
#' @keywords internal
MINUTES_PER_DAY <- 1440L

#' Window length (in minutes) used for the L5 statistic: average activity
#' over the least active 5-hour span of the average day.
#' @keywords internal
L5_WINDOW_MINUTES <- 300L

#' Window length (in minutes) used for the M10 statistic: average activity
#' over the most active 10-hour span of the average day.
#' @keywords internal
M10_WINDOW_MINUTES <- 600L

#' Autocorrelation lag (in minutes) beyond which "recovery" (autocorrelation
#' dropping below the 0.2 threshold) is capped, if it never actually drops
#' below threshold within the computed lags.
#' @keywords internal
MAX_AUTOCORR_LAG <- 120L

#' roundup_power_10
#'
#' Rounds a value up to the nearest power of 10 (e.g. 342 -> 1000, 8 -> 10).
#' Used to pick a "clean" y-axis upper limit for actogram/EWS plots.
#'
#' @param x A positive numeric value.
#' @return The smallest power of 10 that is >= x.
#' @examples
#' roundup_power_10(342)  # 1000
#' roundup_power_10(8)    # 10
roundup_power_10 <- function(x) 10 ^ ceiling(log10(x))
