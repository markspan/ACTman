#' nparcalc
#'
#' This function calculates non-parametric circadian rhythm variables based on activity data.
#' The variables calculated are:
#' - IS: Interdaily Stability
#' - IV: Interdaily Variability
#' - RA: Relative Amplitude
#' - L5: Average of the 5 lowest hourly means within each day
#' - L5_starttime: Time at which L5 occurs
#' - M10: Average of the 10 highest hourly means within each day
#' - M10_starttime: Time at which M10 occurs
#' 
#' This method is typically used for analyzing rest-activity rhythms, particularly in research related to circadian rhythms.
#'
#' @param myACTdevice Name of the input device used. Should be either "Actiwatch2" or "MW8". This parameter determines how the dataset is handled, particularly how the end of the dataset is identified.
#' @param movingwindow A boolean indicating whether a moving window analysis is being used. If TRUE, the function processes a subset of data based on the current window; otherwise, the entire dataset is analyzed.
#' @param CRV.data A data frame containing circadian rhythm (CRV) data. It must have a "Date" column (containing date and time) and an "Activity" column (containing activity data).
#' @param ACTdata.1.sub A subset of the original activity data used for filtering and identifying the dataset's end point.
#' @param out Optional. If movingwindow is TRUE, this argument is the current window of data being analyzed.
#'
#' @return A list with calculated circadian rhythm variables:
#'   - IS: Interdaily Stability
#'   - IV: Interdaily Variability
#'   - RA: Relative Amplitude
#'   - L5: Average of the 5 lowest hourly means within each day
#'   - L5_starttime: Time at which L5 occurs
#'   - M10: Average of the 10 highest hourly means within each day
#'   - M10_starttime: Time at which M10 occurs
#'   - CRV_data: The input CRV data after filtering based on the start and end times.
#'
#' @importFrom stats na.pass
#' @importFrom utils tail
#' @importFrom stats aggregate
#' @importFrom moments skewness
#' @importFrom moments kurtosis
nparcalc <- function(myACTdevice, movingwindow, CRV.data, ACTdata.1.sub, out = NULL) {

  ## Step 1: Basic Operations -------------------------------------------------------------------

  ## Initialise results list to store calculated variables.
  result <- list()

  ## Check if the CRV data contains more than two columns (Date and Time), 
  ## and combine the separate "Date" and "Time" columns into a single "Date" column. 
  ## If there's only two columns, it assumes the columns are already named "Date" and "Activity".
  if (ncol(CRV.data) > 2) {
    CRV.data$Date <- paste(CRV.data$Date, " ", CRV.data$Time)
    CRV.data <- CRV.data[, -2]
  } else {
    colnames(CRV.data) <- c("Date", "Activity")
  }

  ## Filter the data to keep only full 24-hour periods, starting from "00:00"
  CRV.data.wholehours <- CRV.data[grep("00:00", CRV.data[, "Date"]), ]
  CRV.data.start <- which(CRV.data$Date == CRV.data.wholehours[1, "Date"])

  ## Identify the end of the dataset based on the device type and whether a moving window is used.
  if (myACTdevice == "MW8") {
    CRV.data.end <- tail(grep("00:00:00", ACTdata.1.sub$Date), 2)[1]
  } else {
    if (movingwindow) {
      CRV.data.end <- which(out == "00:00:00")[length(which(out == "00:00:00"))]
    } else {
      CRV.data.end <- tail(grep("00:00:00", ACTdata.1.sub$Date), 2)[1]
    }
  }

  ## Filter the CRV data based on the start and end times for circadian rhythm analysis.
  CRV.data <- CRV.data[CRV.data.start:CRV.data.end, ]


  ## Step 2: Calculate Circadian Rhythm Variables -------------------------------------------------

  ### 1. IS: Interdaily Stability ---------------------------------------------------------------
  # Interdaily stability quantifies the consistency of an individual's activity pattern across days.
  # A higher IS means more consistency, while a lower IS indicates variability.
  # Source: Van Someren et al. (1999) - Chronobiology International, 16(4), pp.505-518.

  ## Aggregating activity data by hour, calculating the mean activity for each hour.
  xi <- aggregate(CRV.data[, "Activity"],
                  list(hour = cut(as.POSIXct(CRV.data[, "Date"]), breaks = "hour")),
                  mean, na.action = na.pass, na.rm = TRUE)

  ## If moving window is not used, discard the last data point in xi.
  if (!movingwindow) {
    xi <- xi[1:(nrow(xi) - 1), ]
  } else {
    xi <- xi[1:(nrow(xi)), ]
  }

  ## Extract the activity data as a vector of hourly means.
  xi <- xi$x

  ## Calculate the overall mean of activity for all hours.
  X <- mean(xi, na.rm = TRUE)

  ## Calculate the numerator and denominator for IS formula.
  xi_X <- xi - X # Differences between hourly means and the overall mean.
  sq.xi_X <- xi_X ^ 2 # Squared differences.
  sum.sq.xi_X <- sum(sq.xi_X, na.rm = TRUE) # Sum of squares.
  n <- sum(!is.na(xi)) # Number of hours (should be 168 for a 7-day interval).
  sum.sq.xi_X.perhour <- sum.sq.xi_X / n # Sum of squares per hour.

  ## Subset xi to ensure the length is a multiple of 24 (one full day).
  xi_sub <- xi[1:(24 * floor(length(xi) / 24))]
  Xh <- rowMeans(matrix(xi_sub, nrow = 24), na.rm = TRUE) # Calculate hourly means.
  Xh_X <- Xh - X # Difference between 24-hour means and the overall mean.
  sum.sq.Xh_X <- sum(Xh_X ^ 2, na.rm = TRUE) # Sum of squared differences.
  sum.sq.Xh_X.perhour <- sum.sq.Xh_X / 24 # Sum of squares per hour.

  # Final IS value calculation
  result$IS <- round(sum.sq.Xh_X.perhour / sum.sq.xi_X.perhour,2)

  ### 2. IV: Interdaily Variability ------------------------------------------------------------
  # Interdaily variability measures how much the activity pattern changes from day to day.
  # A higher IV indicates more variability.
  # Source: Van Someren et al. (1999) - Chronobiology International, 16(4), pp.505-518.

  Xi_diffXi <- diff(xi) # Differences between consecutive hourly means.
  sum.sq.Xi_diffXi <- sum(Xi_diffXi ^ 2, na.rm = TRUE) # Sum of squared differences.
  sum.sq.Xi_diffXi.perhour <- sum.sq.Xi_diffXi / (n - 1) # Sum of squares per hour.

  Xi_X <- xi - X # Difference between xi and the overall mean.
  sum.sq.Xi_X <- sum(Xi_X ^ 2, na.rm = TRUE) # Sum of squared differences.
  sum.sq.Xi_X.perhour <- sum.sq.Xi_X / n # Sum of squares per hour (not n-1).

  result$IV <- round(sum.sq.Xi_diffXi.perhour / sum.sq.Xi_X.perhour,2)

  ### 3. L5: Average of the 5 Lowest Hourly Means --------------------------------------------
  # L5 is the average of the 5 lowest hourly activity means within each day.
  # It reflects the low-activity periods of the circadian rhythm.
  # Source: Witting et al. (1990) - Biological Psychiatry, 27(6), pp.563-572.

  # Calculate the average activity per minute for the entire period.
  averageday <- matrix(c(substr(CRV.data[1:1440, "Date"], 14, 22), rep(NA, 1440)), nrow = 1440, ncol = 2)

  # Calculate the mean activity for each minute over the period.
  selection_mat <- matrix(FALSE, 1440)
  for (aa in 1:1440) {
    selection_mat[aa, ] <- TRUE
    averageday[aa, 2] <- mean(CRV.data[selection_mat, "Activity"], na.rm = TRUE)
    selection_mat <- matrix(FALSE, 1440)
  }
  averageday[, 2] <- as.numeric(averageday[, 2])

  # Calculate the 5-hour intervals of activity.
  averageday_loc_L5 <- matrix(NA, nrow(averageday))
  for (gg in 1:(nrow(averageday) - 300)) {
    averageday_loc_L5[gg] <- mean(as.numeric(averageday[c(gg:(299 + gg)), 2]))
  }

  # Find the time and value of L5 (lowest 5-hour mean).
  result$L5_starttime <- averageday[which.min(averageday_loc_L5), 1]
  result$L5 <- round(averageday_loc_L5[which.min(averageday_loc_L5)], 2)

  ### 4. M10: Average of the 10 Highest Hourly Means -----------------------------------------
  # M10 is the average of the 10 highest hourly activity means within each day.
  # It represents the high-activity periods of the circadian rhythm.
  # Source: Witting et al. (1990) - Biological Psychiatry, 27(6), pp.563-572.

  averageday_loc_M10 <- matrix(NA, nrow(averageday))
  for (hh in 1:(nrow(averageday) - 600)) {
    averageday_loc_M10[hh] <- mean(as.numeric(averageday[c(hh:(599 + hh)), 2]), na.rm = TRUE)
  }

  result$M10_starttime <- averageday[which.max(averageday_loc_M10), 1]
  result$M10 <- round(averageday_loc_M10[which.max(averageday_loc_M10)], 2)

  ### 5. RA: Relative Amplitude -----------------------------------------------------------------
  # RA quantifies the difference between M10 and L5 relative to their sum.
  # A higher RA indicates a more pronounced circadian rhythm.
  # Source: Van Someren et al. (1999) - Chronobiology International, 16(4), pp.505-518.

  Amp <- M10 - L5
  result$RA <- Amp / (L5 + M10)
  ### 6. EWS Part: Statistical Moments ---------------------------------------------------------
  # The following variables provide additional statistical measures of the activity data:
  # - Mean: The average activity level.
  # - Variance: The variance of the activity data.
  # - Standard Deviation: The spread of the activity data.
  # - Coefficient of Variation: The ratio of the standard deviation to the mean activity.
  # - Skewness: The asymmetry of the data distribution.
  # - Kurtosis: The "tailedness" of the data distribution.
  # - Autocorrelation: The correlation of activity data with itself at various time lags.
  # - Time to Recovery: The time it takes for autocorrelation to drop below a threshold (0.2).

  result$Mean <- round(mean(CRV.data[, "Activity"], na.rm = TRUE), 2)
  result$Variance <- round(var(CRV.data[, "Activity"], na.rm = TRUE), 2)
  result$SD <- round(sd(CRV.data[, "Activity"], na.rm = TRUE), 2)
  result$CoV <- round(((sd(CRV.data[, "Activity"], na.rm = TRUE) / mean(CRV.data[, "Activity"], na.rm = TRUE)) * 100), 2)
  result$Skewness <- skewness(CRV.data[, "Activity"], na.rm = TRUE)
  result$Kurtosis <- kurtosis(CRV.data[, "Activity"], na.rm = TRUE)

  ## Step 3: Return Results --------------------------------------------------------------------
  return(result)
}
