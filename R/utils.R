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
