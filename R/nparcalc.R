#' nparcalc
#'
#' Device- and moving-window-aware entry point for circadian rhythm and
#' early-warning-signal analysis. Handles column normalization and windowing
#' (which portion of the recording to analyze, depending on device and
#' whether a moving window is in use), then delegates the actual statistics
#' to \code{\link{circadian_metrics}} (IS, IV, RA, L5, M10) and
#' \code{\link{ews_metrics}} (Mean, Variance, SD, CoV, Skewness, Kurtosis,
#' autocorrelation, time to recovery). Kept as a single entry point for
#' backward compatibility with existing callers/scripts.
#'
#' @param myACTdevice Name of the input device used. Should be either Actiwatch2 or MW8.
#' @param movingwindow A boolean indicating whether moving window is used.
#' @param CRV.data CRV data
#' @param ACTdata.1.sub Managed data set
#' @param out Optional. When movingwindow is TRUE, this is the current window of data.
#'
#' @return A named list combining the outputs of \code{\link{circadian_metrics}}
#'   and \code{\link{ews_metrics}}, plus \code{CRV_data} (the windowed data
#'   actually used for the calculations).
#'
#' @seealso \code{\link{circadian_metrics}}, \code{\link{ews_metrics}}
#'
#' @importFrom utils tail
#' @export
nparcalc <- function(myACTdevice, movingwindow, CRV.data, ACTdata.1.sub, out = NULL) {
  ## Step 1: Normalize columns and window the data to the period of interest ---------------

  if (ncol(CRV.data) > 2) {
    CRV.data$Date <- paste(CRV.data$Date, " ", CRV.data$Time)
    CRV.data <- CRV.data[, -2]
  } else {
    colnames(CRV.data) <- c("Date", "Activity")
  }

  CRV.data.wholehours <- CRV.data[grep("00:00", CRV.data[, "Date"]), ]
  CRV.data.start <- which(CRV.data$Date == CRV.data.wholehours[1, "Date"])

  ## Device- and functionality-specific identification of dataset end:
  if (myACTdevice == "MW8") {
    CRV.data.end <- tail(grep("00:00:00", ACTdata.1.sub$Date), 2)[1]
  } else {
    if (movingwindow) {
      CRV.data.end <- which(out == "00:00:00")[length(which(out == "00:00:00"))]
    } else {
      CRV.data.end <- tail(grep("00:00:00", ACTdata.1.sub$Date), 2)[1]
    }
  }

  CRV.data <- CRV.data[CRV.data.start:CRV.data.end, ]

  ## Step 2: Delegate to the pure metric functions -----------------------------------------

  result <- c(circadian_metrics(CRV.data, movingwindow = movingwindow),
    ews_metrics(CRV.data))

  ## Step 3: Attach the windowed data (some callers/plots use this) -----------------------
  result$CRV_data <- CRV.data

  result
}
