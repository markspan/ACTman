#' Overview of Sleep Data Analysis
#'
#' This function processes activity data, calculates sleep-related metrics, 
#' and generates a summary overview of sleep data. It handles cases where a
#' sleeplog or event marker file may be missing or needs to be generated from
#' available data. The function calculates various sleep parameters such as sleep 
#' duration, sleep efficiency, and sleep latency based on activity data.
#'
#' @param workdir A string representing the working directory where the necessary 
#'   files (e.g., sleeplog.csv, markers.csv) are located.
#' @param actdata A data frame containing activity data, including the time and 
#'   activity counts.
#' @param i An integer indicating the index of the file in the `ACTdata.files` 
#'   list for which the analysis will be performed.
#' @param lengthcheck A logical value indicating whether to check if the 
#'   sleeplog contains at least 14 days of data. Defaults to `TRUE`.
#' @param ACTdata.files A list of file names containing activity data.
#'
#' @return A data frame containing an overview of the sleep data, including 
#'   calculated metrics such as sleep start time, sleep end time, time in bed, 
#'   actual sleep duration, wake epochs, and sleep efficiency.
#'
#' @details The function reads activity data and uses a sleeplog or marker files 
#'   to extract the necessary sleep times (e.g., bedtime, wakeup time). It then 
#'   calculates sleep-related metrics and generates an overview.
#'   - **Sleep start**: The time when sleep was initiated based on activity data.
#'   - **Sleep end**: The time when sleep ended based on activity data.
#'   - **Time in bed**: Total duration between "lights out" and "wake up."
#'   - **Actual sleep duration**: Calculated based on the difference between wake 
#'     epochs and actual sleep time.
#'   - **Sleep efficiency**: Percentage of time in bed spent sleeping.
#'   - **Sleep latency**: Time between bedtime and the start of sleep.
#' 
#' @examples
#' # Example usage:
#' result <- sleepdata_overview(workdir = "/path/to/data", actdata = my_act_data,
#'                               i = 1, lengthcheck = TRUE, ACTdata.files = my_files)
#' print(result)
sleepdata_overview <- function(workdir, activity_data, day_index, length_check, act_data_files) {
  
  # Initialize variables
  sleep_log <- NA
  activity_data$Activity_MW_counts <- as.numeric(as.character(activity_data$Activity))
  
  # Sanity check: Ensure that at least one of 'sleeplog.csv' or 'markers.csv' is available
  if (length(list.files(pattern = "sleeplog.csv")) == 0 && length(list.files(pattern = "markers.csv")) == 0) {
    stop("No sleeplog or Event marker file found in working directory! Please provide sleeplog or Event marker file!")
  }
  
  # Check and load sleep log data
  if (length(list.files(pattern = "sleeplog.csv")) >= 1 && length(list.files(pattern = "markers.csv")) == 0) {
    message("Sleeplog file found in working directory!")
    sleep_log <- read.csv(file = list.files(pattern = "sleeplog.csv")[day_index], sep = "\t")
  }
  
  # Generate sleep log from marker file if only markers file is present
  if (length(list.files(pattern = "sleeplog.csv")) == 0 && length(list.files(pattern = "markers.csv")) == 1) {
    message("Only Event marker file found, generating sleeplog...")
    sleeplog_from_markers(workdir = workdir, day_index = day_index, act_data_files = act_data_files)
  }
  
  # Handle multiple marker files in the directory
  if (length(list.files(pattern = "sleeplog.csv")) == 0 && length(list.files(pattern = "markers.csv")) > 1) {
    message("Multiple marker files found, generating sleeplog...")
  }
  
  # If both marker and sleeplog files are found, select the appropriate ones
  if (length(list.files(pattern = "sleeplog.csv")) != 0 && length(list.files(pattern = "markers.csv")) != 0) {
    message("Both marker file and sleeplog found, checking for person-specific files...")
    person_markers_index <- pmatch(substr(act_data_files[day_index], 1, nchar(act_data_files[day_index]) - 8),
                                   list.files(pattern = "markers.csv"))
    person_sleeplog_index <- pmatch(substr(act_data_files[day_index], 1, nchar(act_data_files[day_index]) - 8),
                                    list.files(pattern = "sleeplog.csv"))
    
    # Load the specific sleeplog file if found
    if (length(list.files(pattern = "sleeplog.csv")) != 0 && !is.na(person_sleeplog_index)) {
      sleep_log <- read.csv(file = list.files(pattern = "sleeplog.csv")[person_sleeplog_index])
    } else {
      # If no specific sleeplog file is found, generate it from markers
      sleeplog_from_markers(workdir = workdir, day_index = day_index, act_data_files = act_data_files)
      person_sleeplog_index <- pmatch(substr(act_data_files[day_index], 1, nchar(act_data_files[day_index]) - 8),
                                      list.files(pattern = "sleeplog.csv"))
      sleep_log <- read.csv(file = list.files(pattern = "sleeplog.csv")[person_sleeplog_index])
    }
    message("Selecting sleeplog only!")
  }
  
  # Convert the time in the activity data for later processing
  activity_data$Time <- strftime(activity_data$Date, format = "%H:%M:%S")
  start_new_day_indices <- which(strftime(activity_data$Date, format = "%H:%M:%S") == "12:00:00")
  first_new_day_start <- start_new_day_indices[1]
  
  # Initialize overview dataframe for storing results
  sleepdata_overview <- data.frame(
    "date" = NA, "sleep.start" = NA, "sleep.end" = NA,
    "timeinbed" = NA, "assumed_sleep" = NA, "wakepochs_duration" = NA,
    "actual_sleep_duration" = NA, "actual_sleep_perc" = NA,
    "actual_wake_duration" = NA, "actual_wake_perc" = NA,
    "sleep.efficiency" = NA, "sleep.latency" = NA
  )
  
  # Check if sufficient days of sleep log data is available
  if (length_check) {
    if (nrow(sleep_log) < 14) {
      stop("ERROR: we need at least 14 days of sleeplog. Terminating program.")
    }
    loop_steps <- 14
  } else {
    loop_steps <- nrow(sleep_log)
  }
  
  # Process each day's data
  for (day_index in 1:loop_steps) {
    
    # Create a subset of the activity data for the current day
    if (day_index == 1) {
      day_data <- activity_data[1:first_new_day_start, ]
    } else {
      day_data <- activity_data[(first_new_day_start + (1 + (1440 * (day_index - 2)))):((first_new_day_start + (1440 * (day_index - 1))) + 1440), ]
    }
    
    # Convert Date column to POSIXct for easier manipulation
    day_data$Date <- as.POSIXct(day_data$Date)
    
    # Calculate scores based on previous and next activity counts
    day_data <- dplyr::mutate(day_data, score = (
      (dplyr::lag(Activity_MW_counts, n = 1L, default = 0) / 5) +
      (dplyr::lag(Activity_MW_counts, n = 2L, default = 0) / 25) +
      (dplyr::lead(Activity_MW_counts, n = 1L, default = 0) / 5) +
      (dplyr::lead(Activity_MW_counts, n = 2L, default = 0) / 25) +
      Activity_MW_counts))
    
    # Classify the subject as awake or asleep based on the score
    day_data$WakeSleep <- ifelse(day_data$score > 20, 1, 0)  # 1 is awake, 0 is asleep
    
    # Classify the subject as mobile or immobile based on activity counts
    day_data$MobileImmobile <- ifelse(day_data$Activity_MW_counts > 3, 1, 0)  # 1 is mobile, 0 is immobile
    
    # Bedtime and wake-up time from the sleep log
    bedtime <- paste(as.character(sleep_log$Bedtime[day_index]), ":00", sep = "")
    if (nchar(bedtime) > 8) {
      bedtime <- substr(bedtime, 1, nchar(bedtime) - 3)
    }
    wake_up_time <- paste(as.character(sleep_log$Gotup[day_index]), ":00", sep = "")
    if (nchar(wake_up_time) > 8) {
      wake_up_time <- substr(wake_up_time, 1, nchar(wake_up_time) - 3)
    }
    
    # Find indices for bedtime and wake-up time in the activity data
    bedtime_index <- which(day_data$Time == bedtime)[1]
    wake_up_index <- which(day_data$Time == wake_up_time)[1]
    
    if (is.na(bedtime_index) || is.na(wake_up_index)) {
      message("NA in row indices for Bedtime or Gotup! Skipping current day.")
      next()
    }
    
    # Filter data for the time spent in bed
    bedtime_data <- day_data[bedtime_index:wake_up_index, ]
    sleep_start_data <- bedtime_data[which(bedtime_data$sleep.chance < 2), ]
    sleep_start <- as.character(sleep_start_data$Time[1])
    
    # Find the index for the sleep start time
    sleep_start_index <- which(day_data$Time == sleep_start)[1]
    
    # Filter for sleep epochs (time spent asleep)
    assumed_sleep_data <- day_data[sleep_start_index:wake_up_index, ]
    time_in_bed <- (nrow(assumed_sleep_data) / 60)  # Convert minutes
    
    # Calculate various sleep metrics
    actual_sleep_duration <- length(which(assumed_sleep_data$WakeSleep == 0)) / 60  # Duration in hours
    wake_epochs_duration <- sum(assumed_sleep_data$WakeSleep == 1)  # Wake epochs duration in minutes
    sleep_efficiency <- (actual_sleep_duration / time_in_bed) * 100  # Sleep efficiency percentage
    sleep_latency <- (sleep_start_index - bedtime_index) / 60  # Sleep latency in minutes
    
    # Store the results for the current day
    sleepdata_overview[day_index, ] <- c(
      as.character(sleep_log$Date[day_index]),
      bedtime, wake_up_time, sleep_start, wake_up_time,
      round(time_in_bed, 2), round(actual_sleep_duration, 2),
      wake_epochs_duration, round(actual_sleep_duration, 2),
      round((actual_sleep_duration / time_in_bed) * 100, 2),
      round(wake_epochs_duration, 2), round(sleep_efficiency, 2), round(sleep_latency, 2)
    )
  }
  
  # Create results directory and write the sleep data overview to CSV
  temp_dir <- getwd()
  dir.create(file.path(temp_dir, "Results"), showWarnings = FALSE)
  setwd(file.path(temp_dir, "Results"))
  write.csv(sleepdata_overview, file = paste(substr(act_data_files[day_index], 1, nchar(act_data_files[day_index]) - 4), "-sleep-results.csv", sep = ""))
  setwd(temp_dir)
  
  # Return the processed sleep data overview
  sleepdata_overview
}
