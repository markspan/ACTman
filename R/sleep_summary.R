#' sleepdata_overview
#'
#' Calculate sleep data
#'
#' @param workdir The directory where the sleep files are located.
#' @param actdata The activity data.
#' @param i The index of the current file in ACTdata.files
#' @param lengthcheck Boolean value. If TRUE, the dataset is shortened to the start date plus 14 days, and observations more than 14 days after the start date are removed.
#' @param ACTdata.files The current file in ACTdata.files
#' @param on_missing_markers What to do when the sleeplog generated from marker files has missing Bedtime/Gotup values. One of `"median"` (default), `"manual"`, or `"abort"`. See `?ACTman`. Only used when a sleeplog has to be generated from marker files.
#'
#' @return Returns a sleepdata overview.
#'
#' @importFrom stats na.omit
#' @importFrom utils read.csv
#' @importFrom utils write.csv
sleepdata_overview <- function(workdir, actdata, i, lengthcheck, ACTdata.files,
                               on_missing_markers = c("median", "manual", "abort")) {
  on_missing_markers <- match.arg(on_missing_markers)
  # TODO: add a fragmentation index (see the "planned metrics" note near the
  # end of the per-night loop below).

  ## Step 1: Basic Operations.----------------------------------------------------------------------------

  ## Absolute-path structure, replacing the previous approach of relying on
  ## whatever setwd() the caller (ACTman()) had done before calling this
  ## function.
  paths <- actman_paths(workdir)

  # Load data
  data <- actdata
  data.sleeplog <- NA

  data$Activity..MW.counts. <- as.numeric(as.character(data$Activity))

  ## Read sleeplog
  if (length(list.files(paths$workdir, pattern = "sleeplog.csv")) == 0 &&
     length(list.files(paths$workdir, pattern = "markers.csv")) == 0) { # Sanity Check
    message("No sleeplog or Event marker file found in working directory!")
    message("Please provide sleeplog or Event marker file!")
    message("Also please make sure that the name of the sleeplog file ends in 'sleeplog.csv'!")
    message("")
    message("Stopping sleep-analyses...")
    stop()
  }


  # When only sleeplog found
  if (length(list.files(paths$workdir, pattern = "sleeplog.csv")) >= 1 &&
     length(list.files(paths$workdir, pattern = "markers.csv")) == 0) { # Check for Marker file
    message("Sleeplog file found in working directory!")

    sleeplog_file <- list.files(paths$workdir, pattern = "sleeplog.csv")[i]
    data.sleeplog <- read.csv(file = file.path(paths$workdir, sleeplog_file), sep = "\t")

  }



  if (length(list.files(paths$workdir, pattern = "sleeplog.csv")) == 0 &&
     length(list.files(paths$workdir, pattern = "markers.csv")) == 1) { # Check for Marker file
    message("Only Event marker file found in working directory!")
    message("Generating sleeplog from marker file!")

    sleeplog_from_markers(workdir = paths$workdir, i = i, ACTdata.files = ACTdata.files, on_missing_markers = on_missing_markers)
  }


  if (length(list.files(paths$workdir, pattern = "sleeplog.csv")) == 0 &&
     length(list.files(paths$workdir, pattern = "markers.csv")) > 1) { # Check for Marker file
    message("Multiple marker files found in working directory!")
    message("Generating sleeplog from marker file!")
  }



  if (length(list.files(paths$workdir, pattern = "sleeplog.csv")) != 0 &&
     length(list.files(paths$workdir, pattern = "markers.csv")) != 0) { # Check for Marker file
    message("Both marker file AND Sleeplog found in working directory!")

    message("Checking for person-specific markers and/or sleeplog!")

    which_ppns_markers <- pmatch((paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 8)))),
                                                      list.files(paths$workdir, pattern = "markers.csv")) # markers of this ppn

    which_ppns_sleeplog <- pmatch((paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 8)))),
                                 list.files(paths$workdir, pattern = "sleeplog.csv")) #sleeplog of this ppn


    ## If there is a sleeplog AND this sleeplog belongs to this ppn
    if (length(list.files(paths$workdir, pattern = "sleeplog.csv")) != 0 && !is.na(which_ppns_sleeplog)) {

      sleeplog_file <- list.files(paths$workdir, pattern = "sleeplog.csv")[which_ppns_sleeplog]
      data.sleeplog <- read.csv(file = file.path(paths$workdir, sleeplog_file))

    } else {
      sleeplog_from_markers(workdir = paths$workdir, i = i, ACTdata.files = ACTdata.files, on_missing_markers = on_missing_markers)

      which_ppns_sleeplog <- pmatch((paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 8)))),
                                    list.files(paths$workdir, pattern = "sleeplog.csv")) #sleeplog of this ppn

      sleeplog_file <- list.files(paths$workdir, pattern = "sleeplog.csv")[which_ppns_sleeplog]
      data.sleeplog <- read.csv(file = file.path(paths$workdir, sleeplog_file))
    }


    message("Selecting sleeplog only!")

  }





  # Recreate data$Time var for this module
  data$Time <- strftime(data$Date, format = "%H:%M:%S")

  # Select different nights: 12:00 (noon) is used as the day boundary since
  # bedtimes cluster around midnight, so splitting there would cut most
  # nights in half.
  startdates.new.days <- which(strftime(data$Date, format = "%H:%M:%S") == "12:00:00")
  end.night.1 <- startdates.new.days[1]

  ## Initialise Output Overview
  sleepdata.overview <- data.frame("date" = NA, "sleep.start" = NA, "sleep.end" = NA,
                                   "sleep.efficiency" = NA, "sleep.latency" = NA)

  ## END OF Step 1: Basic Operations.

  ## Number of loop iterations depends on lengthcheck: either exactly 14
  ## nights (erroring if the sleeplog is shorter), or every row available.
  if (lengthcheck) {
    loop_steps <- 14
    if (nrow(data.sleeplog) < 14) {
      print("ERROR: we need at least 14 days of sleeplog. Terminating program.")
      stop('ERROR: we need at least 14 days of sleeplog. Terminating program.')
    }
  } else {
    loop_steps <- nrow(data.sleeplog)
  }

  ## LOOP for Calculating Sleep Variables
  ## Note: "aaa" (this night's data window) is extended by an extra 1440
  ## minutes for nights 2+, to support cases where the diary-reported
  ## Bedtime falls later than the naive one-day window would allow (a
  ## same-day Bedtime/Gotup situation -- see the sameday_loc handling below).
  for (a in seq_len(loop_steps)) {

    if (a == 1) {
      aaa <- data[1:(end.night.1), ]
    } else {
      aaa <- data[(end.night.1 + (1 + (1440 * (a - 2)))):((end.night.1 + (1440 * (a - 1))) + 1440), ]
    }

    aaa$Date <- as.POSIXct(aaa$Date)

    ## Epoch-level wake/sleep scoring (score, WakeSleep, MobileImmobile,
    ## epoch.sleep.chance, sleep.chance, wakeup.chance); see ?score_epochs.
    aaa <- score_epochs(aaa)

    ## Now calculate sleep start from a certain point in the data (based on sleep log)
    Bedtime <- paste((as.character(data.sleeplog$Bedtime[a])), ":00", sep = "")

    if (nchar(Bedtime) > 8) {
      Bedtime <- substr(Bedtime, 1, nchar(Bedtime) - 3)
    }

    rownr.Bedtime <- which(aaa$Time == Bedtime)[1]

    Gotup <- paste((as.character(data.sleeplog$Gotup[a])), ":00", sep = "")

    if (nchar(Gotup) > 8) {
      Gotup <- substr(Gotup, 1, nchar(Gotup) - 3)
    }

    ## Known limitation: taking the first ("[1]") match can be wrong if this
    ## Gotup time also happens to occur earlier in the window than Bedtime.
    rownr.Gotup <- which(aaa$Time == Gotup)[1]

    if (is.na(substr(aaa[rownr.Gotup, "Date"], 1, 10)) || is.na(substr(aaa[rownr.Bedtime, "Date"], 1, 10))) {

      message("NA in rownr. of Bedtime or Gotup!")
      message("Skip current day!")
      next()

    }

    ## If Bedtime and Gotup fall on the same calendar day (can happen when
    ## the window is offset in an unusual way), take the second occurrence
    ## of Gotup in the window instead of the first.
    sameday_loc <- 0
    if (substr(aaa[rownr.Gotup, "Date"], 1, 10) == substr(aaa[rownr.Bedtime, "Date"], 1, 10)) {

      sameday_loc <- which(data.sleeplog[, "Date"] == substr(aaa[rownr.Bedtime, "Date"], 1, 10))

      if (sameday_loc > 1 && length(sameday_loc) >= 1) {
        rownr.Gotup <- which(aaa$Time == Gotup)[2]
      }
    }

    ## Exception for when rownr.Gotup is NA
    if (is.na(rownr.Gotup)) {

      message("rownr.Gotup is NA!")
      message("Skipping current day!")
      next()

    }

    ## Exception for when rownr.Bedtime is NA
    if (is.na(rownr.Bedtime)) {

      message("rownr.Bedtime is NA!")
      message("Skipping current day!")
      next()

    }

    ## Known limitation: rownr.sleep.start below can end up NA if no epoch
    ## with sleep.chance < 2 falls within aaa.Bedtime (i.e. the subject never
    ## registers as "asleep" between Bedtime and Gotup by this heuristic).
    aaa.Bedtime <- aaa[rownr.Bedtime:rownr.Gotup, ] # only the window between lights-out and wake-up

    ## First epoch after Bedtime with a low enough sleep.chance marks sleep onset.
    sleep.start. <- aaa.Bedtime[which(aaa.Bedtime$sleep.chance < 2), ]
    sleep.start <- as.character(sleep.start.$Time[1])

    if (substr(aaa[rownr.Gotup, "Date"], 1, 10) == substr(aaa[rownr.Bedtime, "Date"], 1, 10)) {
      rownr.sleep.start <- which(aaa$Time == sleep.start)[2] # [1] would be wrong in the same-day case
    } else {
      rownr.sleep.start <- which(aaa$Time == sleep.start)[1]
    }

    ## Exception for when rownr.sleep.start is NA
    if (is.na(rownr.sleep.start)) {

      message("Warning: rownr.sleep.start is NA!")
      message("Cause: obs with sleep.chance < 2 is NOT in aaa.Bedtime")
      message("Action: Take Bedtime from sleeplog instead")
      rownr.sleep.start <- rownr.Bedtime

    }

    ## Known limitation: this sanity check has previously caught real data
    ## files (e.g. a case with an all-CSV export from participant 5016) where
    ## something upstream of this point resolves Bedtime/Gotup/sleep-start
    ## inconsistently, producing negative sleep/sleep-efficiency values for
    ## a run of nights. When it fires, the day is skipped rather than
    ## reporting a nonsensical result.
    if (rownr.Bedtime >= rownr.Gotup || rownr.sleep.start >= rownr.Gotup || rownr.Bedtime > rownr.Gotup) {
      message("Warning: sanity checks for Bedtime, Gotup, and/or sleep start failed")

      print(paste("Gotup:", aaa[rownr.Gotup, "Date"]))
      print(paste("Bedtime: ", aaa[rownr.Bedtime, "Date"]))
      print(paste("Sleep start: ", aaa[rownr.sleep.start, "Date"]))

      print(paste("Bedtime later than Gotup:", aaa[rownr.Bedtime, "Date"] >= aaa[rownr.Gotup, "Date"]))
      print(paste("Bedtime earlier than Gotup:", aaa[rownr.Bedtime, "Date"] <= aaa[rownr.Gotup, "Date"]))

      message("Skipping current day!")
      next()
    }

    ## Get sleeptime
    aaa.sleeptime <- aaa[rownr.sleep.start:(rownr.Gotup + (4 * 60)), ] # A (4 * 60) minute extra window is included, for when a subject filled the diary incorrectly (with a too early time). This makes sure that if sleep actually ended after the Gotup time the sleep end is somewhere near the Gotup, instead of in the middle of the night.

    ## First candidate approach: find sustained wake periods via wakeup.chance,
    ## then take the point where the gap between candidates exceeds 4 minutes
    ## (i.e. skip small blips, keep the first genuinely sustained wake period).
    sleep.end. <- aaa.sleeptime[which(aaa.sleeptime$wakeup.chance >= 2 & dplyr::lead(aaa.sleeptime$wakeup.chance > 2)), ]
    sleep.end.$diff[2:nrow(sleep.end.)] <- diff(as.numeric(row.names(sleep.end.)))
    sleep.end.new <- sleep.end.[which(sleep.end.$diff > 4), ] # Bigger than 4, as the difference should be at least 5 minutes, so a small difference with possible sleep is left out.

    if (sum(na.omit(sleep.end.$diff > 4)) == 0) {
      sleep.end.new <- sleep.end.[1, ]
    }

    sleep.end.row <- as.numeric(rownames(sleep.end.new[nrow(sleep.end.new), ]))
    sleep.end <- as.character(aaa$Time[ifelse(sleep.end.row > rownr.Gotup, rownr.Gotup, sleep.end.row)])
    rownr.sleep.end <- ifelse(sleep.end.row > rownr.Gotup, rownr.Gotup, sleep.end.row)

    ## Second (currently authoritative) approach: last epoch before Gotup
    ## with a low enough wakeup.chance marks sleep offset. Overwrites the
    ## first approach's result above.
    tempp <- aaa.sleeptime[which(head(aaa.sleeptime, n = (-4 * 60))$wakeup.chance <= 2), ]
    sleepend <- tail(tempp, n = 1) # sleepend = last element of temp
    if (nrow(sleepend) == 0) { # tail() of an empty data.frame returns 0 rows, not NULL; use sleeplog got up time instead
      sleepend <- aaa.sleeptime[rownr.Gotup, ]
    }

    sleep.end <- sleepend$Time
    rownr.sleep.end <- as.numeric(rownames(sleepend))

    ## Exception for when rownr.sleep.end is NA
    if (is.na(rownr.sleep.end) || (length(rownr.sleep.end) == 0)) {

      message("rownr.sleep.end is NA!")
      message("Action: Taking sleep end from sleeplog Gotup time instead.")
      rownr.sleep.end <- rownr.Gotup

    }

    ## Step 3: Calculate sleep analysis data.----------------------------------------------------------------------------

    aaa.assumedsleeptime <- aaa[rownr.sleep.start:(rownr.sleep.end - 1), ]

    if (nrow(aaa.assumedsleeptime) > 1440) {

      message("nrow assumedsleeptime > 1440!!!!!!!!")
      aaa.assumedsleeptime <- aaa[(which(aaa$Time == Bedtime)[1]:which(aaa$Time == sleepend$Time)[1]), ]

    }

    TimeInBed <- (nrow(aaa.assumedsleeptime) / 60) # The total elapsed time between the "Lights Out" and "Got Up" times
    TimeInBed_false <- (rownr.Gotup - rownr.Bedtime) / 60 # The total elapsed time between the "Lights Out" and "Got Up" times

    AssumedSleep_false <- (rownr.sleep.end - rownr.sleep.start) / 60 # The total elapsed time between the "Fell Asleep" and "Woke Up" times.
    AssumedSleep <- (TimeInBed - (TimeInBed_false - AssumedSleep_false))

    WakeEpochs <- sum(aaa.assumedsleeptime$WakeSleep == 1) # Number of epochs scored as "awake"
    ActualSleep <- length(which(aaa.assumedsleeptime$WakeSleep == 0)) / 60.0

    ActualSleepPerc <- (ActualSleep / AssumedSleep) * 100 # Actual sleep time expressed as a percentage of the assumed sleep time
    ActualWakeTime <- WakeEpochs / 60 # Total time spent in wake according to the epoch-by-epoch wake/sleep scores.
    ActualWakePerc <- 100 - ActualSleepPerc # Actual sleep time expressed as a percentage of the assumed sleep time.
    SleepEfficiency <- (ActualSleep/TimeInBed) * 100 # Actual sleep time expressed as a percentage of time in bed.
    SleepLatency <- (rownr.sleep.start - rownr.Bedtime) / 60 # The time between "Lights Out" and "Fell Asleep"

    ## Planned but not yet implemented metrics (would need bout-detection
    ## logic over WakeSleep/MobileImmobile): SleepBouts, WakeBouts,
    ## MeanSleepBout, MeanWakeBout, ImmobileMins/ImmobileTime,
    ## MobileMins/MobileTime, ImmobileBouts (and the <=1-minute-bout
    ## variants), TotalActivityScore, MeanNonZero, and a FragmentationIndex
    ## combining MobileTime and the short-immobile-bout percentage.

    ## Step 4: Fill in the Sleep Overview

    # Attach Sleep Analysis output to overview
    sleepdata.overview[a, "date"] <- as.character(data.sleeplog[a, "Date"])
    sleepdata.overview[a, "Bedtime_sleeplog"] <- Bedtime
    sleepdata.overview[a, "Gotup_sleeplog"] <- Gotup
    sleepdata.overview[a, "sleep.start"] <- sleep.start
    sleepdata.overview[a, "sleep.end"] <- sleep.end
    sleepdata.overview[a, "timeinbed"] <- round(TimeInBed, 2)
    sleepdata.overview[a, "assumed_sleep"] <- round(AssumedSleep, 2)
    sleepdata.overview[a, "wakepochs_duration"] <- WakeEpochs
    sleepdata.overview[a, "actual_sleep_duration"] <- round(ActualSleep, 2)
    sleepdata.overview[a, "actual_sleep_perc"] <- round(ActualSleepPerc, 2)
    sleepdata.overview[a, "actual_wake_duration"] <- round(ActualWakeTime, 2)
    sleepdata.overview[a, "actual_wake_perc"] <- round(ActualWakePerc, 2)
    sleepdata.overview[a, "sleep.efficiency"] <- round(SleepEfficiency, 2)
    sleepdata.overview[a, "sleep.latency"] <- round(SleepLatency, 2)


  }

  ## Write sleepdata output as .CSV into "Results" directory:
  ensure_dir(paths$results_dir)
  write.csv(sleepdata.overview, file = file.path(paths$results_dir, paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 4)),
                                             "-sleep-results.csv", sep = "")))

  # Return a result
  sleepdata.overview
}
