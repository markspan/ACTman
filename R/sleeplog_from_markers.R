#' sleeplog_from_markers
#'
#' Calculates a sleep log from marker button data.
#'
#' @param workdir The directory where the sleep files are located.
#' @param i The index of the current file in ACTdata.files.
#' @param ACTdata.files A list of file names containing the sleep data.
#'
#' @return Returns a sleeplog as a data frame.
#' 
#' @importFrom utils read.csv write.csv
#'
sleeplog_from_markers <- function(workdir, i, ACTdata.files) {

  # Set working directory
  setwd(workdir)

  # List all marker button files and filter by participant ID
  mb_files <- list.files(pattern = "markers.csv")
  mb_files <- mb_files[pmatch(substr(ACTdata.files[i], 1, 4), mb_files)]  # Match participant ID

  # Read the marker button data
  mb_data <- read.csv(mb_files)

  # If file has 1 column, assume tab-separated format and reload
  if (ncol(mb_data) == 1) {
    mb_data <- read.delim(mb_files)
  }

  # Set column names
  colnames(mb_data) <- c("Name/Type", "Date", "Time")

  # Remove header if present
  if (length(grep("^Name/Type", mb_data[, 1])) != 0) {
    mb_data <- mb_data[((grep("^Name/Type", mb_data[, 1]) + 1):nrow(mb_data)), ]
  }

  # Calculate frequency of marker presses per day
  mb_data_datefreq <- as.data.frame(table(unlist(mb_data$Date)))
  mb_data_datefreq <- mb_data_datefreq[!(mb_data_datefreq$Freq == 0), ]

  # Create new temporary columns for further processing
  mb_data$Remove <- 0
  mb_data$Morning_evening <- 0
  mb_data$Freq <- 0
  mb_data$sleep_after_midnight <- 0

  # Fill in initial Bedtimes and Gotups based on time of day
  for (a in 1:nrow(mb_data)) {
    time <- as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S")
    
    if (time > as.POSIXct("04:00:00", format = "%H:%M:%S") && time < as.POSIXct("14:00:00", format = "%H:%M:%S")) {
      mb_data[a, "Morning_evening"] <- "Gotup"
    } else if (time > as.POSIXct("14:00:00", format = "%H:%M:%S") && time < (as.POSIXct("04:00:00", format = "%H:%M:%S") + (1440 * 60))) {
      mb_data[a, "Morning_evening"] <- "Bedtime"
    }

    mb_data[a, "Freq"] <- mb_data_datefreq[which(mb_data[a, "Date"] == mb_data_datefreq[, "Var1"]), "Freq"]
  }

  # Handle special cases for 'sleep_after_midnight' and 'Morning_evening'
  for (a in 1:nrow(mb_data)) {
    if ((mb_data[a, "Morning_evening"] == 0) && (mb_data[(a - 1), "Morning_evening"] == "Gotup") &&
        (mb_data[(a + 1), "Morning_evening"] == "Gotup") && (as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") < as.POSIXct("05:00:00", format = "%H:%M:%S"))) {
      mb_data[a, "Morning_evening"] <- "Bedtime"
      mb_data[a, "sleep_after_midnight"] <- 1
    }
  }

  # Remove incorrect markers
  for (a in 1:(nrow(mb_data) - 1)) {
    if ((mb_data[a, "Morning_evening"] == "Gotup") && identical(mb_data[a, "Morning_evening"], mb_data[(a + 1), "Morning_evening"]) &&
        identical(mb_data[a, "Date"], mb_data[(a + 1), "Date"])) {
      mb_data[a, "Remove"] <- 1
    }

    if ((mb_data[a, "Morning_evening"] == "Bedtime") && identical(mb_data[a, "Morning_evening"], mb_data[(a + 1), "Morning_evening"]) &&
        identical(mb_data[a, "Date"], mb_data[(a + 1), "Date"])) {
      mb_data[a, "Remove"] <- 1
    }
  }

  # Adjust dates for 'sleep_after_midnight' occurrences
  for (a in 1:(nrow(mb_data))) {
    if (mb_data[a, "sleep_after_midnight"] == 1) {
      mb_data[a, "Date"] <- mb_data[(a - 1), "Date"]
    }
  }

  # Remove incorrect markers and drop unnecessary columns
  mb_data <- mb_data[(which(mb_data$Remove == 0)), ]
  mb_data <- mb_data[, c("Date", "Time", "Morning_evening", "sleep_after_midnight")]

  # Initialize the sleeplog
  sleeplog_nrow <- nrow(mb_data_datefreq)
  sleeplog <- matrix(nrow = sleeplog_nrow, ncol = 3)
  colnames(sleeplog) <- c("Date", "Gotup", "Bedtime")

  # Fill in the sleeplog data with 'Gotup' and 'Bedtime' times
  for (b in 1:nrow(sleeplog)) {
    if (sleeplog[b, "Date"] %in% mb_data[, "Date"]) {
      mb_TEMP <- mb_data[which(mb_data[, "Date"] == sleeplog[b, "Date"]), ]

      # Get 'Gotup' time (if exists)
      if ("Gotup" %in% mb_TEMP[, "Morning_evening"]) {
        if (length(as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Gotup"), "Time"])) > 1) {
          sleeplog[b, "Gotup"] <- min(as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Gotup"), "Time"]))
        } else {
          sleeplog[b, "Gotup"] <- as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Gotup"), "Time"])
        }
      } else {
        message(paste("Gotup time is missing for day", sleeplog[b, "Date"], "!!"))
      }

      # Get 'Bedtime' time (if exists)
      if ("Bedtime" %in% mb_TEMP[, "Morning_evening"]) {
        if (length(as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Bedtime"), "Time"])) > 1) {
          sleeplog[b, "Bedtime"] <- min(as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Bedtime"), "Time"]))
        } else {
          sleeplog[b, "Bedtime"] <- as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Bedtime"), "Time"])
        }
      } else {
        message(paste("Bedtime time is missing for day", sleeplog[b, "Date"], "!!"))
      }
    }
    rm(mb_TEMP)
  }

  # Handle missing data and prompt user for action
  if (sum(is.na(sleeplog)) != 0) {
    message(paste("Warning:", sum(is.na(sleeplog)), "markers are missing!"))
    missings_prompt_answer <- readline(prompt = "Enter 'm' to impute missing markers with median values, 'f' to fill in the missings by hand, or 'q' to Abort:")

    if (missings_prompt_answer == "m") {
      message("Imputing missing markers using median!")
      sleeplog_TEMP <- as.POSIXct(sleeplog[, "Bedtime"], format = "%H:%M:%S")

      if (TRUE %in% is.na(sleeplog[, "Gotup"])) {
        sleeplog[is.na(sleeplog[, "Gotup"]), "Gotup"] <- substr(median(as.POSIXct(sleeplog[, "Gotup"], format = "%H:%M:%S"), na.rm = TRUE), start = 12, stop = 19)
      }

      if (TRUE %in% is.na(sleeplog[, "Bedtime"])) {
        sleeplog[is.na(sleeplog[, "Bedtime"]), "Bedtime"] <- substr(median(as.POSIXct(sleeplog_TEMP, format = "%H:%M:%S"), na.rm = TRUE), start = 12, stop = 19)
      }
    }

    if (missings_prompt_answer == "f") {
      message("Please fill in the missing markers and click 'File > Close' to continue")
      fix(sleeplog)
    }
    if (missings_prompt_answer == "q") {
      stop("User aborted the program.")
    }

    # Check if sleeplog times are in full minutes
    if (length(grep(pattern = ":00", x = sleeplog)) < nrow(sleeplog)) {
      message("Non-full minutes detected in sleeplog times! Rounding to full minutes.")
      substr(sleeplog[, 2:3], start = 6, stop = 8) <- ":00"
    }
  }

  # Write the sleeplog to a CSV file
  write.csv(x = sleeplog,
            file = paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 4)),
                                       "-sleeplog.csv", sep = ""),
            row.names = FALSE)
}
