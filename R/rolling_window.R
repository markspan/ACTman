#' run_rolling_window
#'
#' Runs a moving-window circadian/EWS analysis: repeatedly windows the data
#' into overlapping (or adjacent) spans of \code{window} minutes, shifting
#' by \code{jump} minutes each step, and calls \code{\link{nparcalc}} on
#' each window. Extracted from \code{ACTman()}'s previously inline
#' \code{rollingwindow()} closure with no behavior change.
#'
#' @param x The full activity data (Date/Time/Activity or Date/Activity).
#' @param window Window length in minutes (e.g. \code{1440 * movingwindow.size}).
#' @param jump Step size in minutes between successive windows (e.g.
#'   \code{1440 * movingwindow.jump}).
#' @param myACTdevice Name of the input device used ("Actiwatch2" or "MW8"),
#'   passed through to \code{\link{nparcalc}}.
#' @param ACTdata.1.sub The full managed dataset for this file, used by
#'   \code{\link{nparcalc}} for device-specific end-of-data detection.
#' @param verbose Whether to print per-window progress and results to the
#'   console (as the original implementation always did). Default TRUE for
#'   backward-compatible console output.
#'
#' @return A data frame with one row per window and columns: \code{starttime},
#'   \code{endtime}, \code{IS}, \code{IV}, \code{RA}, \code{L5},
#'   \code{L5_starttime}, \code{M10}, \code{M10_starttime}, \code{Mean},
#'   \code{Variance}, \code{SD}, \code{Coeff_of_Var}, \code{Skewness},
#'   \code{Kurtosis}, \code{Autocorr_lag1/2/3/60/120}, \code{Time_to_Recovery}.
run_rolling_window <- function(x, window, jump, myACTdevice, ACTdata.1.sub, verbose = TRUE) {
  out <- data.frame()
  n <- nrow(x)
  rollingwindow.results <- as.data.frame(matrix(nrow = (floor(((n - window) / jump))), ncol = 21))

  ## Set number of iterations at number of rows of (data - windowsize) / jump
  for (i in 1:((floor(((n - window) / jump))) + 1)) {
    if (i == 1) {
      out <- x[i:window, ]
    } else {
      out <- x[((i - 1) * jump):(((i - 1) * jump) + window), ]
    }

    CRV.data <- out
    if (ncol(CRV.data) > 2) {
      colnames(CRV.data) <- c("Date", "Time", "Activity")
    } else if (ncol(CRV.data) == 2) {
      colnames(CRV.data) <- c("Date", "Activity")
    }

    r2 <- nparcalc(myACTdevice = myACTdevice, movingwindow = TRUE, CRV.data = CRV.data,
                   ACTdata.1.sub = ACTdata.1.sub, out = out)

    rollingwindow.results[i, 1] <- as.character(strftime(CRV.data[1, "Date"], format = "%Y-%m-%d %H:%M:%S"))
    rollingwindow.results[i, 2] <- as.character(strftime(CRV.data[nrow(CRV.data), "Date"], format = "%Y-%m-%d %H:%M:%S"))
    rollingwindow.results[i, 3] <- r2$IS
    rollingwindow.results[i, 4] <- r2$IV
    rollingwindow.results[i, 5] <- round(r2$RA, 2)
    rollingwindow.results[i, 6] <- round(r2$L5, 2)
    rollingwindow.results[i, 7] <- r2$L5_starttime
    rollingwindow.results[i, 8] <- round(r2$M10, 2)
    rollingwindow.results[i, 9] <- r2$M10_starttime
    rollingwindow.results[i, 10] <- r2$Mean
    rollingwindow.results[i, 11] <- r2$Variance
    rollingwindow.results[i, 12] <- r2$SD
    rollingwindow.results[i, 13] <- r2$CoV
    rollingwindow.results[i, 14] <- r2$Skewness
    rollingwindow.results[i, 15] <- r2$Kurtosis
    rollingwindow.results[i, 16] <- r2$Autocorr
    rollingwindow.results[i, 17] <- r2$Autocorr_lag2
    rollingwindow.results[i, 18] <- r2$Autocorr_lag3
    rollingwindow.results[i, 19] <- r2$Autocorr_lag60
    rollingwindow.results[i, 20] <- r2$Autocorr_lag120
    rollingwindow.results[i, 21] <- r2$Time_to_Recovery
    colnames(rollingwindow.results) <- c("starttime", "endtime", "IS", "IV", "RA", "L5", "L5_starttime",
                                         "M10", "M10_starttime", "Mean", "Variance", "SD",
                                         "Coeff_of_Var", "Skewness", "Kurtosis", "Autocorr_lag1",
                                         "Autocorr_lag2", "Autocorr_lag3", "Autocorr_lag60",
                                         "Autocorr_lag120", "Time_to_Recovery")

    if (verbose) {
      print("---------------------------------------------------------------------------------")
      print(paste("Rolling window CRV analysis output - Window step:", (i - 1)))
      print(paste("Begin time:", CRV.data[1, "Date"]))
      print(paste("End time:", CRV.data[nrow(CRV.data), "Date"]))
      print(paste("nOBS:", nrow(CRV.data)))
      print("")
      print("Circadian Rhythm Variables")
      print(paste("IS: ", r2$IS))
      print(paste("IV: ", r2$IV))
      print(paste("RA: ", round(r2$RA, 2)))
      print(paste("L5: ", round(r2$L5, 2)))
      print(paste("L5_starttime: ", r2$L5_starttime))
      print(paste("M10: ", round(r2$M10, 2)))
      print(paste("M10_starttime: ", r2$M10_starttime))
      print("")
      print("Early-Warning Signals")
      print(paste("Mean: ", r2$Mean))
      print(paste("Variance: ", r2$Variance))
      print(paste("SD: ", r2$SD))
      print(paste("Coefficient of Variation: ", r2$CoV))
      print(paste("Skewness: ", r2$Skewness))
      print(paste("Kurtosis: ", r2$Kurtosis))
      print(paste("Autocorr at-lag-1: ", r2$Autocorr))
      print(paste("Autocorr at-lag-2: ", r2$Autocorr_lag2))
      print(paste("Autocorr at-lag-3: ", r2$Autocorr_lag3))
      print(paste("Autocorr at-lag-60: ", r2$Autocorr_lag60))
      print(paste("Autocorr at-lag-120: ", r2$Autocorr_lag120))
      print(paste("Time_to_Recovery: ", r2$Time_to_Recovery))
      print("---------------------------------------------------------------------------------")
    }
  }

  rollingwindow.results
}
