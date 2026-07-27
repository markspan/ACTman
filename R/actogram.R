##############################################################################
### ACTman{plot_actogram}                                                  ###
### Script authors: Yoram Kunkels, Stefan Knapen, & Ando Emerencia         ###
### Most recent Update: 21-12-2017                                         ###
### Goal: plot activity counts over time, with 24 hour lines (or 48 hours) ###
###                                                                        ###
### To add: Grey part before start, Title,                                 ###
### Export to PDF function, Days more clearly written                      ###
###========================================================================###
### Revision History:                                                      ###
### 16-04-2018: Added x-axis hours & 24h Plotting                          ###
###~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@###

#' plot_actogram
#'
#' Function to plot 48 hour Actograms.
#'
#' @param workdir the working directory as supplied to ACTman.
#' @param ACTdata.1.sub The managed data set
#' @param i The index of the current file in ACTdata.files
#' @param plotactogram Value indicating if and what kind of actogram has to be plotted. Can be either '48h', '24h', or FALSE
#'
#'
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom graphics axis
#' @importFrom graphics barplot
#' @importFrom graphics par
#' @importFrom stats na.omit
plot_actogram <- function(workdir, ACTdata.1.sub, i, plotactogram, rollingwindow.results, i_want_EWS) {

### Part 1: Basic Operations ----------------------------------------------------------------------------

  paths <- actman_paths(workdir)
  ensure_dir(paths$actogram_dir)



  act_data <- ACTdata.1.sub # Copy data for editing required for plotting
  ## Use format() with an explicit format string rather than as.character():
  ## as.character() on a POSIXct silently drops the time-of-day when it is
  ## exactly midnight (e.g. "2020-01-01 00:00:00" prints as "2020-01-01"),
  ## which breaks the substr(Date, 12, 19) == "00:00:00" day-boundary checks
  ## below whenever the very first recorded minute happens to be midnight.
  act_data <- within(act_data, Date <- format(act_data$Date, "%Y-%m-%d %H:%M:%S"))
  ndays.plot <- round(abs(as.numeric(round(as.POSIXct(act_data$Date[1]) - as.POSIXct(act_data$Date[nrow(act_data)]), 2))))


### Part 2: 1st day Selection & Midnight Detection 2nd day  ---------------------------------------------------

  ## Midnight Detection 2nd day
  day2start <- ((which(substr(act_data$Date, start = 12, stop = 19) == "00:00:00")[1]) - 1) # Start of day2 at midnight.

  ## 1st Day selection
  day1 <- act_data[1:day2start, ]
  day1.rest <- 1440 - day2start

  ## Filling in period before 1st day selection to get full 24h in 1st day
  day1.rest.mat <-  matrix(data = 0, nrow = day1.rest, ncol = 2) # Create empty matrix
  colnames(day1.rest.mat) <- colnames(day1) # Equalise colnames
  day1 <- rbind(day1.rest.mat, day1) # Combine day1 data with empty pre-day1 matrix


### Part 3: Loop for assigning subsequent days (14 days max for plot) & setting ylim's ----------------------

  ## Assign other days
  for (i.plot in 2:(ndays.plot)) {
    if (i.plot == 2) {
      assign(paste("day", i.plot, sep = ""), act_data[((day2start + (1440 ^ (i.plot - 2))):(day2start + (1440 * (i.plot - 1)))), ])
    } else {
      assign(paste("day", i.plot, sep = ""), act_data[((day2start + ((1440 * (i.plot - 2)) + 1)):(day2start + (1440 * (i.plot - 1)))), ])
    }
  }

  ## Define the range so all plots will be equal height (ylim)
  ylimit_TEMP <- ls()[setdiff(grep("day", ls()), c(grep("rest", ls()), grep("start", ls()), grep("plot", ls())))]
  ylimit <- range(na.omit(eval(parse(text = ylimit_TEMP))[, "Activity"]))


  ### Part 4: Combining Days & 48 hour Doubleplot -----------------------------------------------------------

  ## Combine days
  for (j.plot in 1:(ndays.plot - 1)) {
    assign(paste("day.", j.plot, ".", (j.plot + 1), sep = ""),
           rbind(eval(parse(text = paste("day", j.plot, sep = ""))), eval(parse(text =  paste("day", (j.plot + 1), sep = "")))))
  }


if (i_want_EWS == TRUE && is.na(rollingwindow.results)) {

  stop("Cannot create EWS plot without rolling window results. Ensure 'i_want_EWS' and 'movingwindow' are both TRUE.")

}


if (i_want_EWS == TRUE) {  # ## Initialise empty matrix for timestamps and activity counts

  ews_timeseries <- matrix(NA, nrow = (1440 * ndays.plot), ncol = 2)

  ## Assign timestamps and activity counts to matrix
  for (k.plot in 1:ndays.plot) {

     if (k.plot == 1) {
      ews_timeseries[k.plot:1440, 1] <- eval(parse(text = paste("day", k.plot, "[ , 1]", sep = "")))
      ews_timeseries[k.plot:1440, 2] <- eval(parse(text = paste("day", k.plot, "[ , 2]", sep = "")))
    } else {
      ews_timeseries[((((k.plot - 1) * 1440) + 1):(k.plot * 1440)), 1] <- eval(parse(text = paste("day", k.plot, "[ , 1]", sep = "")))
      ews_timeseries[((((k.plot - 1) * 1440) + 1):(k.plot * 1440)), 2] <- eval(parse(text = paste("day", k.plot, "[ , 2]", sep = "")))
    }

  }


  plotme <- colnames(rollingwindow.results)[10:21]

  for (EWS_count in 1:length(plotme)) {


  ## Initialise .PDF plot in A4 size (11.7 x 8.3 inches)
  png(file.path(paths$actogram_dir, paste("Actigraphy EWS Plot - ", plotme[EWS_count], ".png")), width = 842, height = 595, units = "px")

  ## Create barplot
  bp2 <- barplot(as.numeric(ews_timeseries[, 2]), plot = FALSE)

  ## Obtain barplot range
  bp2_ylim <- range(na.omit(as.numeric(ews_timeseries[, 2])))
  bp2_ylim_upper <- roundup_power_10(max(bp2_ylim))

  ## Plot barplot
  barplot(as.numeric(ews_timeseries[, 2]), ylim = c(0, bp2_ylim_upper))


  ## Create labels
  x_labels2 <- substr(ews_timeseries[, 1], 1, 10)
  l.plot_n <- length(unique(substr(ews_timeseries[, 1], 1, 10)[!substr(ews_timeseries[, 1], 1, 10) == "0"]))
  x_labels_pos2_start <- matrix(NA, nrow = l.plot_n, ncol = 1)


  l.plot <- 0
  ## Obtain label positions
  for (l.plot in 1:l.plot_n) {

    ## Assure l.plot is not NA, otherwise assign last non-NA value
    if (is.na(unique(substr(ews_timeseries[, 1], 1, 10)[!substr(ews_timeseries[, 1], 1, 10) == "0"])[l.plot])) {
      l.plot <- max(which(is.na(unique(substr(ews_timeseries[, 1], 1, 10)[!substr(ews_timeseries[, 1], 1, 10) == "0"])) == FALSE))
    }

    x_labels_pos2_start[l.plot, ] <- which.max(x_labels2 == unique(substr(ews_timeseries[, 1], 1, 10)[!substr(ews_timeseries[, 1], 1, 10) == "0"])[l.plot])

  }


  ## Assign start and end dates
  ews_timeseries_startdate <- x_labels2[x_labels_pos2_start][1] # Activity data start date
  ews_timeseries_enddate <- x_labels2[x_labels_pos2_start][length(x_labels2[x_labels_pos2_start])] # Activity data end date

  ## Create axis and vertical day lines
  axis(side = 1, at = bp2[1 + x_labels_pos2_start], labels = x_labels2[x_labels_pos2_start], las = 2, cex.axis = 0.8)
  abline(v = bp2[1 + x_labels_pos2_start], col = "blue")

  ## Assign moving window results (remove first obs to account for non-24h day)
  rollingwindow.results <- rollingwindow.results[2:nrow(rollingwindow.results), ]

  ## Asssign occurences when days from plotted actigraphy data co-occur with days from the moving window results
  matched_dates <- paste(x_labels2[x_labels_pos2_start], "00:00:00") %in% rollingwindow.results$endtime

  ## Assign x-coordinates for moving window results
  plot_points_x <- bp2[1 + x_labels_pos2_start][matched_dates]


  #
  # for(EWS_count in 1:length(plotme)) {

  ## Plot results for moving window on existing barplot at designated x-coordinates
  #! N.b. Scaling should be made dynamic!
  scaling_var <- (bp2_ylim_upper / max(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))

  ## Plotting "plotme" EWS over actogram
    ## Create right axis for EWS measure

  ## Exception for when length(at) != length(labels)
  #! Added na.omit() functions for when "to is not finite" error.
  if (length(seq(0, round(max(na.omit(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))),
                by = (round(max(na.omit(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))) / 2)))
     == length(c(0, (bp2_ylim_upper / 2), bp2_ylim_upper))) {

    axis(side = 4,
         labels = seq(0, round(max(na.omit(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))),
                      by = (round(max(na.omit(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))) / 2)),
         at = c(0, (bp2_ylim_upper / 2), bp2_ylim_upper))

  } else {

    axis(side = 4,
         labels = c(0, (bp2_ylim_upper / 2), bp2_ylim_upper),
         at = c(0, (bp2_ylim_upper / 2), bp2_ylim_upper))

  }

    ## Plot EWS over actogram (exceptions for when length of x- and y-values differ)
    if (length(plot_points_x) < length((rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]]
                                       * scaling_var))) {

        # print("DEBUG--------------------------------------------------------------------------")
        # print(length(plot_points_x))
        # print(length((rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme] * scaling_var)))

        points(x = plot_points_x, type = "l", col = "red",
               y = (rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]]
                    * scaling_var)[1:length(plot_points_x)])

    } else if (length(plot_points_x) > length((rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme] * scaling_var))) {

      points(x = plot_points_x[1:length(rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]])],
             type = "l", col = "red", y = (rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]]
                                           * scaling_var))

    } else if (length(plot_points_x) == length((rollingwindow.results[(1:(nrow(rollingwindow.results))),
                                                                     plotme[EWS_count]] * scaling_var))) {

      points(x = plot_points_x, type = "l", col = "red",
             y = (rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]] * scaling_var))
     }

  ## Create Title
  mtext(paste("Total Activity data with", plotme[EWS_count], "moving window"), line = 1, cex = 1.0)
  mtext(paste("start date:", ews_timeseries_startdate, "end date:", ews_timeseries_enddate), line = 0, cex = 0.8)

  dev.off()
  }

  # dev.off()


}


  ### /Work in Progress ------------------------------------------------------------------------------


  if (plotactogram ==  "48h") {

    ## Initialise .PDF plot in A4 size (11.7 x 8.3 inches)
    pdf(file.path(paths$actogram_dir, paste("Actigraphy Data - 48h Plot", i, ".pdf")), width = 11.7, height = 8.3)


     ## Plot initialisation & parameters
      par(mfrow = c(14, 1)) # Set plot parameters
      par(mar = c(0.5, 4, 0.5, 4)) # Set margins
      # par(mai = c(1, 1, 1, 1))
      bp <- barplot(day.1.2$Activity, ylim = ylimit, ylab = "Day 1", plot = FALSE)
      barplot(day.1.2$Activity, ylim = ylimit, ylab = "Day 1")
      # abline(v = day2start + 75, lty = 2) # Set start moment
      x_labels <- substr(day.1.2$Date, nchar(day.1.2$Date) - 8 + 1, nchar(day.1.2$Date))
      x_labels_pos <- grep("00:00", x_labels)
      x_labels <- x_labels[x_labels_pos]
      x_labels <- substr(x_labels, start = 1, stop = 5)
      axis(side = 1, at = bp[1 + x_labels_pos[ c(TRUE, FALSE)]], labels = x_labels[ c(TRUE, FALSE)])
      axis(side = 1, at = bp[1 + x_labels_pos[ c(FALSE, TRUE)]], labels = FALSE, col.ticks = "red")


      ## Filling in plot with loop for other days
      for (k.plot in 2:(ndays.plot - 1)) {

        barplot(eval(parse(text = paste("day.", k.plot, ".", (k.plot + 1), "$Activity", sep = ""))),
                ylim = ylimit, ylab = paste("Day", k.plot))

        x_labels <- substr(day.2.3$Date, nchar(day.2.3$Date) - 8 + 1, nchar(day.2.3$Date))
        x_labels_pos <- grep("00:00", x_labels)
        x_labels <- x_labels[x_labels_pos]
        x_labels <- substr(x_labels, start = 1, stop = 5)

        axis(side = 1, at = bp[1 + x_labels_pos[ c(TRUE, FALSE)]], labels = x_labels[ c(TRUE, FALSE)])
        axis(side = 1, at = bp[1 + x_labels_pos[ c(FALSE, TRUE)]], labels = FALSE, col.ticks = "red")
      }

      dev.off()

  }


  ### Part 5: 24 hour plot ------------------------------------------------------------------------

  ## 24 hour plot
  if (plotactogram == "24h") {

    ## Initialise .PDF plot in A4 size (11.7 x 8.3 inches)
    pdf(file.path(paths$actogram_dir, paste("Actigraphy Data - 24h Plot", i, ".pdf")), width = 11.7, height = 8.3)

      par(mfrow = c(14, 1)) # Set parameters for plots
      par(mar = c(0.5, 4, 0.5, 4)) # Set margins
      bp <- barplot(day1$Activity, ylim = ylimit,
                    ylab = substr(day1$Date[(which(day1$Date == "0")[length(which(day1$Date == "0"))] + 1)], 6, 10),
                    plot = F)
      barplot(day1$Activity, ylim = ylimit,
              ylab = substr(day1$Date[(which(day1$Date == "0")[length(which(day1$Date == "0"))] + 1)], 6, 10),
              plot = T)
      # abline(v = day2start+75, lty = 2) # Add start line (+75 = marge)

      x_labels <- substr(day2$Date, nchar(day2$Date) - 8 + 1, nchar(day2$Date))
      x_labels_pos <- grep("00:00", x_labels)
      x_labels <- x_labels[x_labels_pos]

      axis(side = 1, at = bp[1 + x_labels_pos], labels = x_labels)

      ## Filling in plot with loop for other days
      for (k.plot in 2:(ndays.plot - 1)) {

        barplot(eval(parse(text = paste("day", k.plot, "$Activity", sep = ""))),
                ylim = ylimit,
                ylab = substr((eval(parse(text = paste("day", k.plot, "$Date", sep = ""))))[1], 6, 10)
                )

        # axis(side = 1, at = (x_labels_pos), labels = x_labels)
        axis(side = 1, at = bp[1 + x_labels_pos], labels = x_labels)

      }

      dev.off()

}
  ### Part 6: Finishing Operations -------------------------------------------------------------------

}
