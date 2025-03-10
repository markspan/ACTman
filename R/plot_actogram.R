#' plot_actogram
#'
#' Function to plot 48-hour or 24-hour actograms based on activity data.
#'
#' @param workdir The working directory where the results will be saved.
#' @param ACTdata.1.sub The subset of the actigraphy data to plot.
#' @param i The index for the current actogram plot (e.g., file number).
#' @param plotactogram A string indicating the type of plot to generate: 
#'                     '48h' for a 48-hour plot, '24h' for a 24-hour plot, or FALSE to skip plotting.
#' @param rollingwindow.results A matrix containing the rolling window results, used for EWS (Early Warning Signals) analysis.
#' @param i_want_EWS Logical flag indicating whether to include Early Warning Signals (EWS) in the plot.
#' 
#' @return NULL (creates and saves the plot as a PNG or PDF file).
#' 
#' @importFrom grDevices dev.off pdf png
#' @importFrom graphics axis barplot mtext
#' @importFrom stats na.omit
plot_actogram <- function(workdir, ACTdata.1.sub, i, plotactogram, rollingwindow.results, i_want_EWS) {
  
  # Part 1: Basic Setup ----------------------------------------------------------------------------
  
  # Save and set the working directory
  workdir.save <- getwd()
  setwd(workdir)

  # Prepare activity data
  act_data <- ACTdata.1.sub
  act_data$Date <- as.character(act_data$Date)  # Convert Date to character for easy handling
  ndays.plot <- round(abs(as.numeric(difftime(as.POSIXct(act_data$Date[1]), as.POSIXct(act_data$Date[nrow(act_data)]), units = "days"))))
  
  # Part 2: Split Data for Days ----------------------------------------------------------------------------
  
  # Detect midnight (start of the second day)
  day2start <- which(substr(act_data$Date, 12, 19) == "00:00:00")[1] - 1

  # Split data into days
  day1 <- act_data[1:day2start, ]
  day1.rest <- 1440 - day2start  # Time remaining in the first day

  # Create empty matrix for the missing period before the first day
  day1.rest.mat <- matrix(0, nrow = day1.rest, ncol = 2)
  colnames(day1.rest.mat) <- colnames(day1)
  day1 <- rbind(day1.rest.mat, day1)  # Add missing period to the start of day1

  # Assign the subsequent days (up to 14 days max for the plot)
  days_list <- list(day1)
  for (i.plot in 2:ndays.plot) {
    start_idx <- day2start + (1440 * (i.plot - 2))
    end_idx <- day2start + (1440 * (i.plot - 1))
    days_list[[i.plot]] <- act_data[start_idx:end_idx, ]
  }

  # Determine y-axis limits based on activity range
  activity_values <- unlist(lapply(days_list, function(x) x$Activity))
  ylimit <- range(na.omit(activity_values))

  # Part 3: Handle EWS (Early Warning Signals) Plotting -------------------------------------------

  if (i_want_EWS && is.null(rollingwindow.results)) {
    stop("EWS cannot be created without rolling window results. Ensure 'i_want_EWS' and 'movingwindow' are both TRUE.")
  }

  if (i_want_EWS) {
    LOLkat <- matrix(NA, nrow = 1440 * ndays.plot, ncol = 2)

    # Fill the LOLkat matrix with timestamps and activity counts
    for (k.plot in 1:ndays.plot) {
      start_idx <- (k.plot - 1) * 1440 + 1
      end_idx <- k.plot * 1440
      LOLkat[start_idx:end_idx, 1] <- days_list[[k.plot]]$Date
      LOLkat[start_idx:end_idx, 2] <- days_list[[k.plot]]$Activity
    }

    # Create EWS plots (one for each metric in the rolling window results)
    plotme <- colnames(rollingwindow.results)[10:21]
    for (EWS_count in 1:length(plotme)) {
      plot_EWS(LOLkat, rollingwindow.results, plotme[EWS_count], i, ylimit)
    }
  }

  # Part 4: Generate Actogram Plots ----------------------------------------------------------------

  # 48-hour plot
  if (plotactogram == "48h") {
    generate_actogram_plot(48, days_list, i, ylimit)
  }

  # 24-hour plot
  if (plotactogram == "24h") {
    generate_actogram_plot(24, days_list, i, ylimit)
  }

  # Reset the working directory to the original
  setwd(workdir.save)
}

#' generate_actogram_plot
#'
#' Helper function to generate and save an actogram plot (24-hour or 48-hour).
#'
#' @param hours The number of hours for the plot (24 or 48).
#' @param days_list A list of day data frames to plot.
#' @param i The index for the current plot.
#' @param ylimit The range of the activity values for the y-axis.
#' 
#' @return NULL (creates and saves the plot as a PDF file).
generate_actogram_plot <- function(hours, days_list, i, ylimit) {
  filename <- paste("Actigraphy Data - ", hours, "h Plot", i, ".pdf", sep = "")
  pdf(filename, width = 11.7, height = 8.3)

  # Set plot parameters
  par(mfrow = c(14, 1))  # 14 subplots vertically
  par(mar = c(0.5, 4, 0.5, 4))  # Margins

  # Plot each day
  for (k.plot in 1:length(days_list)) {
    day_data <- days_list[[k.plot]]
    bp <- barplot(day_data$Activity, ylim = ylimit, ylab = paste("Day", k.plot), plot = FALSE)
    barplot(day_data$Activity, ylim = ylimit, ylab = paste("Day", k.plot))
    
    # Set axis labels
    x_labels <- substr(day_data$Date, nchar(day_data$Date) - 8 + 1, nchar(day_data$Date))
    x_labels_pos <- grep("00:00", x_labels)
    x_labels <- x_labels[x_labels_pos]
    x_labels <- substr(x_labels, 1, 5)
    axis(side = 1, at = bp[1 + x_labels_pos[ c(TRUE, FALSE)]], labels = x_labels[ c(TRUE, FALSE)])
    axis(side = 1, at = bp[1 + x_labels_pos[ c(FALSE, TRUE)]], labels = FALSE, col.ticks = "red")
  }

  dev.off()
}

#' plot_EWS
#'
#' Helper function to generate Early Warning Signals (EWS) plot on top of the actogram.
#'
#' @param LOLkat Matrix containing the activity data with timestamps.
#' @param rollingwindow.results The rolling window results for EWS analysis.
#' @param plotme The specific EWS measure to plot.
#' @param i The index for the current plot.
#' @param ylimit The y-axis limits for the plot.
#' 
#' @return NULL (creates and saves the plot as a PNG file).
plot_EWS <- function(LOLkat, rollingwindow.results, plotme, i, ylimit) {
  # Set up the PNG output for the plot
  png(paste("Actigraphy EWS Plot - ", plotme, ".png"), width = 842, height = 595, units = "px")
  
  # Create the initial bar plot
  bp <- barplot(as.numeric(LOLkat[, 2]), plot = FALSE)
  bp_ylim <- range(na.omit(as.numeric(LOLkat[, 2])))
  bp_ylim_upper <- 10 ^ ceiling(log10(max(bp_ylim)))
  barplot(as.numeric(LOLkat[, 2]), ylim = c(0, bp_ylim_upper))

  # Add vertical day lines and labels
  x_labels2 <- substr(LOLkat[, 1], 1, 10)
  x_labels_pos2_start <- which(x_labels2 != "0")

  axis(side = 1, at = bp[1 + x_labels_pos2_start], labels = x_labels2[x_labels_pos2_start], las = 2, cex.axis = 0.8)
  abline(v = bp[1 + x_labels_pos2_start], col = "blue")

  # Plot the EWS on top of the actogram
  scaling_var <- (bp_ylim_upper / max(rollingwindow.results[, plotme], na.rm = TRUE))
  matched_dates <- paste(x_labels2[x_labels_pos2_start], "00:00:00") %in% rollingwindow.results$endtime
  plot_points_x <- bp[1 + x_labels_pos2_start][matched_dates]

  points(x = plot_points_x, type = "b", pch = 19, col = "red", cex = 0.5)
  dev.off()
}
