#' actman_config
#'
#' Builds and validates a single configuration object for an `ACTman()`
#' run. Consolidates the parameter validation that used to be scattered
#' across the top of `ACTman()` and (for `myACTdevice`) re-checked inside
#' its per-file loop on every iteration, into one place that fails fast,
#' before any file is processed.
#'
#' `ACTman()` calls this internally as its first step and keeps its own
#' argument list unchanged for backward compatibility, so existing calls
#' to `ACTman(workdir = ..., myACTdevice = ..., ...)` continue to work
#' exactly as before; `actman_config()` is exported so it can also be built
#' and inspected independently, e.g. for validating a set of parameters
#' before a long batch job, or for building one config and reusing it
#' across multiple similar `ACTman()`-style calls.
#'
#' @inheritParams ACTman
#'
#' @return A list (S3 class `actman_config`) with one element per
#'   validated parameter.
#'
#' @examples
#' \dontrun{
#' cfg <- actman_config(workdir = "~/actigraphy/study1", myACTdevice = "Actiwatch2")
#' }
#'
#' @export
actman_config <- function(workdir, sleepdatadir = workdir, myACTdevice = "Actiwatch2",
                          iwantsleepanalysis = FALSE, plotactogram = FALSE,
                          selectperiod = FALSE, startperiod = NULL, daysperiod = FALSE, endperiod = NULL,
                          movingwindow = FALSE, movingwindow.size = 14, movingwindow.jump = 1,
                          circadian_analysis = TRUE, nparACT_compare = FALSE, na_omit = FALSE, na_impute = FALSE,
                          missings_report = TRUE, lengthcheck = TRUE, i_want_EWS = FALSE,
                          on_high_missings = c("continue", "abort"),
                          on_missing_markers = c("median", "manual", "abort")) {

  ## Validate the non-interactive decision parameters and device name up
  ## front (fail fast, rather than partway through a long-running batch
  ## job, or -- for myACTdevice -- on the very last file after already
  ## processing every earlier one).
  on_high_missings <- match.arg(on_high_missings)
  on_missing_markers <- match.arg(on_missing_markers)
  if (!(myACTdevice %in% c("Actiwatch2", "MW8"))) {
    stop(paste("Unknown value for myACTdevice (should be MW8, Actiwatch2):", myACTdevice))
  }

  if (!is.logical(plotactogram) && !(plotactogram %in% c("24h", "48h"))) {
    stop("plotactogram must be FALSE, \"24h\", or \"48h\".")
  }

  if (i_want_EWS && !movingwindow) {
    stop("i_want_EWS = TRUE requires movingwindow = TRUE (EWS is plotted against rolling-window results).")
  }

  config <- list(
    workdir = workdir, sleepdatadir = sleepdatadir, myACTdevice = myACTdevice,
    iwantsleepanalysis = iwantsleepanalysis, plotactogram = plotactogram,
    selectperiod = selectperiod, startperiod = startperiod, daysperiod = daysperiod, endperiod = endperiod,
    movingwindow = movingwindow, movingwindow.size = movingwindow.size, movingwindow.jump = movingwindow.jump,
    circadian_analysis = circadian_analysis, nparACT_compare = nparACT_compare,
    na_omit = na_omit, na_impute = na_impute, missings_report = missings_report, lengthcheck = lengthcheck,
    i_want_EWS = i_want_EWS, on_high_missings = on_high_missings, on_missing_markers = on_missing_markers
  )
  class(config) <- "actman_config"
  config
}
