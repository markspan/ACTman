
######################################
#### Sleeplog from Marker buttons ####
#### YKK                          ####
####~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*####


#' sleeplog_from_markers
#'
#' Calculate sleeplog from markers
#'
#' @param workdir The directory where the sleep files are located.
#' @param i The index of the current file in ACTdata.files
#' @param ACTdata.files The current file in ACTdata.files
#' @param on_missing_markers What to do when derived Bedtime/Gotup markers are missing. One of `"median"` (default; impute with the median time), `"manual"` (open an interactive editor via `fix()`; only usable in an interactive session), or `"abort"`.
#'
#' @return Returns a sleeplog
#'
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#'
sleeplog_from_markers <- function(workdir, i, ACTdata.files,
                                  on_missing_markers = c("median", "manual", "abort")) {
  on_missing_markers <- match.arg(on_missing_markers)

  paths <- actman_paths(workdir)

  ## List marker button files and read data
  mb_files <- list.files(paths$workdir, pattern = "markers.csv")
  mb_files <- mb_files[pmatch(substr(ACTdata.files[i], 1, 4), mb_files)] # only take marker file from this ppn
  mb_files <- file.path(paths$workdir, mb_files)

  mb_data <- read.csv(mb_files)

  ## Known limitation: if the file turns out to be tab-separated instead of
  ## comma-separated, the comma-separated read above collapses everything
  ## into a single column; re-read as tab-separated in that case.
  if (ncol(mb_data) == 1) {
    mb_data <- read.delim(mb_files)
  }

  colnames(mb_data) <- c("Name/Type", "Date", "Time")

  ## Remove header row if present
  if (length(grep("^Name/Type", mb_data[, 1])) != 0) {
    mb_data <- mb_data[((grep("^Name/Type", mb_data[, 1]) + 1):nrow(mb_data)), ]
  }

  ## Frequencies of marker presses per day
  mb_data_datefreq <- as.data.frame(table(unlist(mb_data$Date)))
  mb_data_datefreq <- mb_data_datefreq[!(mb_data_datefreq$Freq == 0), ]

  ## Temporary working columns
  mb_data$Remove <- 0               # rows to drop as duplicate/invalid markers
  mb_data$Morning_evening <- 0      # classified as "Bedtime" or "Gotup"
  mb_data$Freq <- 0                 # marker presses that day
  mb_data$sleep_after_midnight <- 0 # bedtime marker pressed after midnight

  ## Classify each marker press as "Gotup" (04:00-14:00) or "Bedtime"
  ## (14:00-04:00 next day), and record that day's press frequency.
  for (a in seq_len(nrow(mb_data))) {

    if (as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") >  as.POSIXct("04:00:00", format = "%H:%M:%S") &&
       as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") <  as.POSIXct("14:00:00", format = "%H:%M:%S")) {

      mb_data[a, "Morning_evening"] <- "Gotup"

    } else if (as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") >  as.POSIXct("14:00:00", format = "%H:%M:%S") &&
              as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") <  (as.POSIXct("04:00:00", format = "%H:%M:%S") + (1440 * 60))) {

      mb_data[a, "Morning_evening"] <- "Bedtime"

    }

    mb_data[a, "Freq"] <- mb_data_datefreq[which(mb_data[a, "Date"] == mb_data_datefreq[, "Var1"]), "Freq"]
  }

  ## A "Gotup" press before 05:00, sandwiched between two other "Gotup"
  ## presses, is really a bedtime press that happened after midnight.
  for (a in seq_len(nrow(mb_data))) {

    if ((mb_data[a, "Morning_evening"] == 0) && (mb_data[(a - 1), "Morning_evening"] == "Gotup") &&
       (mb_data[(a + 1), "Morning_evening"] == "Gotup") && (as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") <
                                                            as.POSIXct("05:00:00", format = "%H:%M:%S"))) {

      mb_data[a, "Morning_evening"] <- "Bedtime"
      mb_data[a, "sleep_after_midnight"] <- 1

    }
  }

  ## Two consecutive same-type, same-day markers: the earlier one is a
  ## duplicate/accidental press, mark it for removal.
  for (a in seq_len(nrow(mb_data) - 1)) {

    if ((mb_data[a, "Morning_evening"] == "Gotup") &&
       identical(mb_data[a, "Morning_evening"], mb_data[(a + 1), "Morning_evening"]) &&
       identical(mb_data[a, "Date"], mb_data[(a + 1), "Date"])) {

      mb_data[a, "Remove"] <- 1
    }

    if ((mb_data[a, "Morning_evening"] == "Bedtime") &&
       identical(mb_data[a, "Morning_evening"], mb_data[(a + 1), "Morning_evening"]) &&
       identical(mb_data[a, "Date"], mb_data[(a + 1), "Date"])) {

      mb_data[a, "Remove"] <- 1
    }
  }

  ## Reassign "sleep_after_midnight" markers to the previous calendar day
  ## (the night they actually belong to).
  for (a in seq_len(nrow(mb_data))) {
    if (mb_data[a, "sleep_after_midnight"] == 1) {
      mb_data[a, "Date"] <- mb_data[(a - 1), "Date"]
    }
  }

  ## Remove markers flagged above, and drop the now-unneeded temp columns.
  mb_data <- mb_data[(which(mb_data$Remove == 0)), ]
  mb_data <- mb_data[, c("Date", "Time", "Morning_evening", "sleep_after_midnight")]

  ## Initialise sleeplog: one row per day with marker presses.
  sleeplog_nrow <- nrow(mb_data_datefreq)
  sleeplog <- matrix(nrow = sleeplog_nrow, ncol = 3)
  colnames(sleeplog) <- c("Date", "Gotup", "Bedtime")

  ## Known edge case: if removing duplicate/invalid markers above changed
  ## the number of distinct days, align the shorter "Date" list to the end
  ## of the sleeplog matrix rather than erroring on a length mismatch.
  if (length(sleeplog[, "Date"]) != length(as.character(sort(rep(unique(mb_data[, "Date"]), 1))))) {

    if (length(sleeplog[, "Date"]) > length(as.character(sort(rep(unique(mb_data[, "Date"]), 1))))) {

      sleeplog_diff <- abs(length(sleeplog[, "Date"]) - length(as.character(sort(rep(unique(mb_data[, "Date"]), 1)))))
      sleeplog[((1 + sleeplog_diff):sleeplog_nrow), "Date"] <- as.character(sort(rep(unique(mb_data[, "Date"]), 1)))
    }

  } else {
    sleeplog[, "Date"] <- as.character(sort(rep(unique(mb_data[, "Date"]), 1)))
  }

  ## Fill in each day's Gotup/Bedtime time from the (cleaned) marker data.
  for (b in seq_len(nrow(sleeplog))) {

    if (sleeplog[b, "Date"] %in% mb_data[, "Date"]) {

      person_day_markers <- mb_data[which(mb_data[, "Date"] == sleeplog[b, "Date"]), ]

      if ("Gotup" %in% person_day_markers[, "Morning_evening"]) {
        ## If multiple Gotup presses were recorded that day, take the earliest.
        if (length(as.character(person_day_markers[which(person_day_markers[, "Morning_evening"] == "Gotup"), "Time"])) > 1) {
          sleeplog[b, "Gotup"] <- min(as.character(person_day_markers[which(person_day_markers[, "Morning_evening"] == "Gotup"), "Time"]))
        } else {
          sleeplog[b, "Gotup"] <- as.character(person_day_markers[which(person_day_markers[, "Morning_evening"] == "Gotup"), "Time"])
        }
      } else {
        message(paste("Gotup time is missing at day", sleeplog[b, "Date"], "!!"))
      }

      if ("Bedtime" %in% person_day_markers[, "Morning_evening"]) {
        ## If multiple Bedtime presses were recorded that day, take the earliest.
        if (length(as.character(person_day_markers[which(person_day_markers[, "Morning_evening"] == "Bedtime"), "Time"])) > 1) {
          sleeplog[b, "Bedtime"] <- min(as.character(person_day_markers[which(person_day_markers[, "Morning_evening"] == "Bedtime"), "Time"]))
        } else {
          sleeplog[b, "Bedtime"] <- as.character(person_day_markers[which(person_day_markers[, "Morning_evening"] == "Bedtime"), "Time"])
        }
      } else {
        message(paste("Bedtime time is missing at day", sleeplog[b, "Date"], "!!"))
      }
    }

    rm(person_day_markers)
  }

  ## Handle missing Bedtime/Gotup values per on_missing_markers.
  if (sum(is.na(sleeplog)) != 0) {

    message(paste("Warning:", sum(is.na(sleeplog)), "markers are missing!"))
    message(paste("Resolving via on_missing_markers =", shQuote(on_missing_markers)))

    if (on_missing_markers == "abort") {
      stop("on_missing_markers = 'abort': stopping due to missing markers.")
    }

    if (on_missing_markers == "median") {
      message("Imputing missing markers using median!")

      ## Median (not mean) is used because Bedtime crossing midnight makes a
      ## plain mean of clock times unreliable.
      sleeplog_bedtime_posix <- as.POSIXct(sleeplog[, "Bedtime"], format = "%H:%M:%S")

      if (TRUE %in% is.na(sleeplog[, "Gotup"])) {
        sleeplog[is.na(sleeplog[, "Gotup"]), "Gotup"] <- substr(median(as.POSIXct(sleeplog[, "Gotup"], format = "%H:%M:%S"), na.rm = TRUE), start = 12, stop = 19)
      }

      if (TRUE %in% is.na(sleeplog[, "Bedtime"])) {
        sleeplog[is.na(sleeplog[, "Bedtime"]), "Bedtime"] <- substr(median(as.POSIXct(sleeplog_bedtime_posix, format = "%H:%M:%S"), na.rm = TRUE), start = 12, stop = 19)
      }
    }

    if (on_missing_markers == "manual") {
      if (!interactive()) {
        stop("on_missing_markers = 'manual' requires an interactive R session (fix() cannot run in a batch/CI job).")
      }
      message("Please fill in the missing markers")
      message("Thereafter, click 'File > Close' to continu")
      fix(sleeplog)
    }

    ## Round to full minutes if any time isn't already on a ":00" boundary
    ## (otherwise downstream Time-matching in sleepdata_overview() fails).
    if (length(grep(pattern = ":00", x = sleeplog)) < nrow(sleeplog)) {
      message("Non-full minutes detected in sleeplog times!")
      message("Rounding sleeplog times to full minutes (':00')")
      message("")

      substr(sleeplog[, 2:3], start = 6, stop = 8) <- ":00"
    }
  }

  ## Write sleeplog to .csv
  write.csv(x = sleeplog,
            file = file.path(paths$workdir, paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 4)),
                                       "-sleeplog.csv", sep = "")),
            row.names = FALSE)
}
