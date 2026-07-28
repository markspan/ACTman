#############################################################################
### ACTman package                                                        ###
### Script authors: Yoram Kunkels, Stefan Knapen, & Ando Emerencia        ###
### Most recent Update: 16-04-2018                                        ###
### Supported devices: Actiwatch 2 Respironics & MW8                      ###
### =======================================================================###
### Revision History:                                                     ###
### 16-04-2018: Added Actogram Functionality.                             ###
### ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~###

#' ACTman - Actigraphy Manager
#'
#' ACTman manages actigraphy data whilst offering pre-processing and analyses options.
#' This initial version supports the 'Actiwatch 2' and 'MotionWatch 8' actigraphy devices,
#' whilst allowing for both sleep and circadian rhythm analyses.
#'
#' @param workdir The working directory of the script.
#' @param sleepdatadir An optional vector specifying the directory for actogram and sleep analysis data.
#' @param myACTdevice Name of the input device used. Should be either 'Actiwatch2' or 'MW8'.
#' @param iwantsleepanalysis Boolean value indicating whether sleep analysis should be performed.
#' @param plotactogram Value indicating if and what kind of actogram has to be plotted. Can be either '48h', '24h', or FALSE.
#' @param selectperiod Boolean value indicating whether a specific period has to be selected.
#' @param startperiod An optional vector specifying single or multiple period starts. Should be in the format "2016-10-03 00:00:00".
#' @param daysperiod An optional vector specifying the length in days of the period.
#' @param endperiod An optional argument that is a date string (format: "2016-10-03 00:00:00"), denoting the end of the data subset to be analyzed. Only used if daysperiod is not specified.
#' @param movingwindow Boolean value indicating whether a moving window should be utilised.
#' @param movingwindow.size An optional vector specifying the length in days of the moving window. Default is 14 days.
#' @param movingwindow.jump An optional vector specifying the length of the jumps with which the moving window is shifted each iteration. Default is 1 day.
#' @param circadian_analysis Boolean value indicating whether non-parametric circadian rhythm analysis should be performed.
#' @param nparACT_compare Boolean value indicating that comparison with another actigraphy R package should be performed. If TRUE, the values for IS, IV, RA, L5, L5_starttime, M10, and M10_starttime of the nparACT_base_loop function are recorded in the returned overview variable.
#' @param na_omit Boolean value indicating whether NA's should be omitted.
#' @param na_impute Boolean value indicating whether NA's should be imputed.
#' @param missings_report Boolean value indicating whether missings promt should appear.
#' @param lengthcheck Boolean value. If TRUE, the dataset is shortened to the start date plus 14 days, and observations more than 14 days after the start date are removed.
#' @param i_want_EWS Boolean value indicating whether early-warning-signal statistics (see `?ews_metrics`) should be overlaid on the actogram plot. Requires `movingwindow = TRUE` in the same call, since it plots against the rolling-window results.
#' @param on_high_missings What to do when more than 0.01\% of a dataset's activity values are missing and `missings_report` is TRUE. One of `"continue"` (default; proceed with the analysis and note the situation) or `"abort"` (stop processing this dataset). Replaces the old interactive `readline()` prompt so batch/CI runs never hang.
#' @param on_missing_markers What to do when the sleeplog derived from marker/button-press files has missing Bedtime/Gotup values. One of `"median"` (default; impute missing times with the median), `"manual"` (open an interactive editor via `fix()`; only usable in an interactive session), or `"abort"` (stop). Replaces the old interactive `readline()` prompt.
#'
#' @return if iwantsleepanalysis, this returns the sleepdata overview, else if movingwindow, it returns the moving window results, and otherwise it returns the actdata overview.
#' @examples
#' \dontrun{
#' ## Using the real, anonymized example recording bundled with the package
#' ## (see vignette("actman-intro") for a full walkthrough):
#' workdir <- tempfile()
#' dir.create(workdir)
#' file.copy(
#'   system.file("extdata", "example-mw8-participant.csv", package = "ACTman"),
#'   file.path(workdir, "participant01.csv")
#' )
#' result <- ACTman(
#'   workdir = workdir,
#'   myACTdevice = "MW8",
#'   circadian_analysis = TRUE,
#'   iwantsleepanalysis = FALSE,
#'   plotactogram = FALSE,
#'   lengthcheck = FALSE
#' )
#' result$circadian
#' }
#' @importFrom stats na.omit
#' @importFrom utils View
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @importFrom utils tail
#' @importFrom utils write.table
#'
#' @export
ACTman <- function(workdir = "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata",
                   sleepdatadir = paste("C:/Bibliotheek/Studie/PhD/Publishing/",
                     "ACTman/R-part/Actogram & Sleep analysis", sep = ""),
                   myACTdevice = "Actiwatch2", iwantsleepanalysis = FALSE, plotactogram = FALSE,
                   selectperiod = FALSE, startperiod = NULL, daysperiod = FALSE, endperiod = NULL,
                   movingwindow = FALSE, movingwindow.size = 14, movingwindow.jump = 1,
                   circadian_analysis = TRUE, nparACT_compare = FALSE, na_omit = FALSE, na_impute = FALSE,
                   missings_report = TRUE, lengthcheck = TRUE, i_want_EWS = FALSE,
                   on_high_missings = c("continue", "abort"),
                   on_missing_markers = c("median", "manual", "abort")) {
  ## Consolidate parameter validation into a single config object (fails
  ## fast, before any file is processed), then unpack it back into local
  ## variables of the same names so the rest of this function is unchanged.
  ## See ?actman_config.
  config <- actman_config(workdir = workdir, sleepdatadir = sleepdatadir, myACTdevice = myACTdevice,
    iwantsleepanalysis = iwantsleepanalysis, plotactogram = plotactogram,
    selectperiod = selectperiod, startperiod = startperiod, daysperiod = daysperiod,
    endperiod = endperiod, movingwindow = movingwindow,
    movingwindow.size = movingwindow.size, movingwindow.jump = movingwindow.jump,
    circadian_analysis = circadian_analysis, nparACT_compare = nparACT_compare,
    na_omit = na_omit, na_impute = na_impute, missings_report = missings_report,
    lengthcheck = lengthcheck, i_want_EWS = i_want_EWS,
    on_high_missings = on_high_missings, on_missing_markers = on_missing_markers)
  myACTdevice <- config$myACTdevice
  on_high_missings <- config$on_high_missings
  on_missing_markers <- config$on_missing_markers

  ## Step 1: Basic Operations-----------------------------------------------------------------

  ## Build the absolute-path structure used throughout this function (and
  ## passed down to sleepdata_overview()/sleeplog_from_markers()/
  ## plot_actogram()), replacing the previous setwd()-based approach.
  paths <- actman_paths(workdir, sleepdatadir)
  workdir <- paths$workdir

  ## List actigraphy files and seperate out sleeplogs:
  pattern_file <- ".csv"
  ACTdata.files <- sort(list.files(paths$workdir, pattern = pattern_file))
  if (any((grep(pattern = "sleeplog", x = ACTdata.files)))) {
    ACTdata.files <- ACTdata.files[-(grep(pattern = "sleeplog", x = ACTdata.files))] # Remove any sleeplogs from data listing
  }
  if (any((grep(pattern = "markers", x = ACTdata.files)))) {
    ACTdata.files <- ACTdata.files[-(grep(pattern = "markers", x = ACTdata.files))] # Remove any markers files from data listing
  }

  if (length(ACTdata.files) == 0) {
    stop(paste("No actigraphy .csv files found in workdir (after excluding sleeplog/markers files):", workdir))
  }

  ## Initialise overview:
  ACTdata.overview <- data.frame("filename" = ACTdata.files, "start" = NA, "end" = NA, "end2" = NA,
    "numberofobs" = NA, "numberofobs2" = NA, "recordingtime" = NA,
    "recordingtime2" = NA, "summertime.start" = NA, "summertime.end" = NA, "missings" = NA,
    "missings_perc" = NA, "IS" = NA, "IV" = NA, "RA" = NA, "L5" = NA, "L5_starttime" = NA, "M10" = NA,
    "M10_starttime" = NA, "r2.IS" = NA, "r2.IV" = NA, "r2.RA" = NA, "r2.L5" = NA,
    "r2.L5_starttime" = NA, "r2.M10" = NA, "r2.M10_starttime" = NA, "last5act.active" = NA,
    "lengthcheck" = NA)

  ## Initiate parameters:
  i <- 1 # set i
  secshour <- 60 * 60 # Seconds per hour.
  secsday <- 24 * secshour # Seconds per day.
  secs14day <- secsday * 14 # Seconds in 14 days.
  minsaday <- (secsday / 60) # Minutes per day.

  ## Semantic checks:
  if (selectperiod && length(startperiod) != length(ACTdata.files)) {
    stop(paste("The number of start periods does not match the number of data files found:", startperiod, length(ACTdata.files)))
  }

  if (".mtn" %in% substr(list.files(paths$workdir), nchar(list.files(paths$workdir)) - 4 + 1, nchar(list.files(paths$workdir)))) {
    message(paste("There is at least 1 unsupported Actigraphy file format present in the working directory!
                  Please convert, stash, or remove these unsupported files before rerunning:"))
    print(list.files(paths$workdir)[grep(".mtn", list.files(paths$workdir))])
    stop("Unsupported .mtn file(s) found in workdir; convert, stash, or remove them before rerunning.")
  }


  ## Step 2: Main ACTman Loop------------------------------------------------------------------------
  ## Description: ...

  for (i in seq_along(ACTdata.files)) {

    print(paste("*** Start of Dataset", i, "***"))
    print("")
    print(paste("Dataset Name:", ACTdata.overview[i, "filename"]))
    print("")


    ## Step 2.1.: Reading Data-----------------------------------------------------------------------

    ## Reading in .CSV data, plus some device-specific Data Management:
    ## Device-specific Data Management is required as raw data format differs between
    ## devices, in e.g., headers and raw data locations.
    if (myACTdevice == "Actiwatch2") {

      ACTdata.1 <- read.csv(file.path(paths$workdir, ACTdata.files[i]), header = FALSE)
      ACTdata.1.sub <- ACTdata.1[, c(4, 5, 6)]
      colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")

    } else if (myACTdevice == "MW8") {

      ACTdata.1 <- read.csv(file.path(paths$workdir, ACTdata.files[i]), header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"))

      if (all(is.na(ACTdata.1$B)) && all(is.na(ACTdata.1$C))) { ## Note! If TRUE ACTdata.1 is (suddenly) tab-seperated!

        ACTdata.1 <- read.csv(file.path(paths$workdir, ACTdata.files[i]), header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"), sep = "\t")

      }




      if (any(ACTdata.1[, 1] == "Raw data:")) {
        ACTdata.1 <- as.data.frame(ACTdata.1[((which(ACTdata.1[, 1] == "Raw data:")) + 2):nrow(ACTdata.1), ])
      } else {
        ACTdata.1 <- read.csv(file.path(paths$workdir, ACTdata.files[i]), header = TRUE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"))
      }

      ## Make a copy of the original data to work on:
      ACTdata.1.sub <- ACTdata.1
      colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
      ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)

      ## Test for 30 sec. bins:
      if (any(grepl(pattern = ":30", x = ACTdata.1$B[1:2]))) {
        print("Detecting Epoch Length.......")
        print("Warning: 30 sec. Epoch's Detected!")
        print("Action: Binning 30 sec. Epochs in 60 sec. Epochs")
        print("")

        ## If epochs are 30 sec. instead of 60 sec., bin them together to form 60 sec. epochs.
        ACTdata.TEMP <- ACTdata.1[(grepl(pattern = ".*(?<!:30)$", x = ACTdata.1$B, perl = TRUE)), ]

        halfminute_data <- as.numeric(ACTdata.1[(grepl(pattern = ".*(?<=:30)$", x = ACTdata.1$B, perl = TRUE)), ]$C)
        if (length(grep(pattern = ":30$", x = ACTdata.1$B[1]))) {
          # if it starts wtih :30, throw first val away
          halfminute_data <- tail(halfminute_data, n = -1)
        }
        if (length(grep(pattern = ":00$", x = ACTdata.1$B[length(ACTdata.1$B)]))) {
          # if it ends on a full minute, add a zero value
          halfminute_data <- c(halfminute_data, 0)
        }
        ACTdata.TEMP$C <- as.numeric(ACTdata.TEMP$C) + halfminute_data
        # #! Workaround for aforementioned issue

        ## Write binned data in ACTdata.TEMP to workable data object ACTdata.1.sub:
        ACTdata.1.sub <- ACTdata.TEMP
        colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
        ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)
        rm(ACTdata.TEMP) # Remove temporary data object
        rm(halfminute_data)

      } else {
        ## Make no changes if 60 sec. bins
        print("Detecting Epoch Length.......")
        print("Normal 60 sec. Epochs detected")
        print("No changes made")
        print("")
      }
    }

    ## Step 2.2: Managing the Data---------------------------------------------------------------

    ## Reformat dates and times into required format for further processing:
    ACTdata.1.sub$Date <- gsub(pattern = "/20", replacement = "/", x = ACTdata.1.sub$Date) # Take only last two year digits
    ACTdata.1.sub$Date <- paste(ACTdata.1.sub$Date, ACTdata.1.sub$Time) # Merge Date Time

    if (grepl("-", ACTdata.1.sub$Date[1])) { # Reformat Date
      ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%Y-%m-%d %H:%M:%S")
    } else {
      ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%d/%m/%y %H:%M:%S")
    }

    ACTdata.1.sub$Time <- NULL # Remove empty Time variable


    ## Check for empty first row and if so, remove it:
    if (all(is.na(ACTdata.1.sub[1, ]))) {
      ACTdata.1.sub <- ACTdata.1.sub[-1, ]
    }


    ## Period Selection (user-defined option):
    ## Start by obtaining the row location of the user-defined start of period (startperiod)
    ## (format: "2016-10-03 00:00:00").
    if (selectperiod) {
      startperiod.loc <- which(ACTdata.1.sub$Date == startperiod[i])
      ## Then, if number of days from startperiod (daysperiod) is given, take only data from
      ## start (startperiod.loc) untill start + specified number of days (startperiod.loc + (daysperiod*minsaday)).
      if (daysperiod) {
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(startperiod.loc + (daysperiod * minsaday))), ]
        ## Else, if no daysperiod if given, see if user-defined end of period (endperiod) is given.
        ## If so, take only data from user-defined start of period to end of period.
      } else if (endperiod %in% ACTdata.1.sub$Date) {
        endperiod.loc <- which(ACTdata.1.sub$Date == endperiod)
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:endperiod.loc), ]
        ## Else, take only data from start of period to end of dataset.
      } else {
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(nrow(ACTdata.1.sub))), ]
      }
    }


    ## Write values to overview file:
    ## Start- and end dates and times of actigraph data:
    start_date <- ACTdata.1.sub$Date[1] # Get start date
    end_date <- ACTdata.1.sub$Date[nrow(ACTdata.1.sub)] # Get end date.
    nr_obs <- nrow(ACTdata.1.sub) # Get number of observations.
    ACTdata.overview[i, "start"] <- as.character(start_date) # Write start date to overview.
    ACTdata.overview[i, "end"] <- as.character(end_date) # Write end date to overview.
    ## Recordingtime (time difference between start and end date):
    ACTdata.overview[i, "recordingtime"] <- round((as.POSIXct(start_date) - as.POSIXct(end_date)), 2) # write recordingtime to overview
    ## Number of observations:
    ACTdata.overview[i, "numberofobs"] <- nr_obs


    ## Identify Last Whole 24 Hour component and its Position:
    ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub[tail(grep("00:00:00", ACTdata.1.sub$Date), 2), "Date"]
    ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub.lastwhole24h[1]


    ## Add 14 days in a way that respects daylight savings time changes:
    # ! Number of days needs to be made dynamic!! Needs to correspond to number of days in dataset!!
    ACTdata.1.sub.14day <- increase_by_days(ACTdata.1.sub$Date[1], 14)


    ## Lengthcheck:

    if (lengthcheck) {
      ## If Dataset is longer than Start Date plus 14 days, Remove data recorded thereafter:
      print("Task 2: Detecting if Dataset is longer than Start Date plus 14 full days")
      if (ACTdata.1.sub[nrow(ACTdata.1.sub), "Date"] > ACTdata.1.sub.14day) {
        print("Warning: Dataset is longer than Start Date plus 14 full days!")

        ACTdata.1.sub <- ACTdata.1.sub[1:((secs14day / 60) + 1), ]

        print("Action: Observations recorded after Start Date plus 14 full days were removed.")
        print("Task 2 DONE: Dataset shortened to Start Date plus 14 days. Tasks 3, 4, and 5 also OK")
        print("")
        ACTdata.overview$lengthcheck[i] <- TRUE
      } else {
        print("Task 2 OK: Dataset not longer than Start Date plus 14 days.")
        print("")
      }

      # Update overview after removal
      ACTdata.overview[i, "numberofobs2"] <- nrow(ACTdata.1.sub)
      ACTdata.overview[i, "recordingtime2"] <- round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2)
      ACTdata.overview[i, "end2"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview
    }



    ## Handling of Missing Data (NA's):
    ## Write Missings values (total number of missings and percentage of total data missing)
    ## to overview file:
    ACTdata.overview[i, "missings"] <- sum(is.na(ACTdata.1.sub$Activity))  # write missings to overview
    ACTdata.overview[i, "missings_perc"] <- round(ACTdata.overview[i, "missings"] / ACTdata.overview[i, "numberofobs"], 3) # write missings percentage to overview
    ## Report missings in console:
    print("Task: Reporting NA's")
    print(paste("Number of NA's in this Dataset:", ACTdata.overview[i, "missings"]))
    print(paste("This is:", round(ACTdata.overview[i, "missings"] / ACTdata.overview[i, "numberofobs"], 3), "% of the total number of observations!"))
    print("")
    ## If user-defined argument "na_omit" is TRUE, then use na.omit{stats} to row-wise delete NA's:
    if (na_omit) {
      print("Row-wise removal of NA's as user defined na.omit = TRUE")
      ACTdata.1.sub <- na.omit(ACTdata.1.sub) # ! Creates error because many NA's!!!!
      print("All NA's removed!")
    }
    ## If user-defined argument "na_impute" is TRUE, then use mice{mice} to impute missings through
    ## Multivariate Imputation by Chained Equations (MICE). This installs the 'mice' package and dependencies.
    if (na_impute) {
      ## Impute Missings
      tempData <- mice::mice(matrix(data = c(ACTdata.1.sub$Activity, rep.int(x = 0, times = (ACTdata.overview[i, "numberofobs"]))), ncol = 2), m = 5, maxit = 50, meth = "pmm", seed = 500)
      tempData2 <- mice::complete(tempData, 1)
      ACTdata.1.sub$Activity <- tempData2$V1
    }

    ## User-control over Analysis if too much Missings:
    ## Too much is specified in this case as > 0.01% missing of total dataset.
    # ! This 0.01% is arbitrarily chosen, as of now no suitable validated criterion is yet found.
    ## Initialise exception handling for when there are no missings:
    number_of_missings <- ifelse(is.na(ACTdata.overview[i, "missings"]), 0, ACTdata.overview[i, "missings"])
    ## If a missings-prompt is required (default is missings_report = TRUE), ..
    if (missings_report && !na_impute) {
      ## .. see if number of missings exceeds 0.01% of total number of data.
      if ((number_of_missings / ACTdata.overview[i, "numberofobs"]) > 0.01) {
        ## If so, explain situation to user via text prompt, and give them the choice to
        ## either continue or stop with the analyses.
        message("\nMore than 0.01% of data is missing!\nAnalysis results might deviate from true values!")
        if (on_high_missings == "abort") {
          message("on_high_missings = 'abort': stopping processing of this dataset.")
          break()
        } else {
          print("Continue analysis with > 0.01% missings (on_high_missings = 'continue')")
          print("")
        }
      }
    }


    ## Check if there is activity in the tail of the dataset. As sometimes at the end of the study
    ## the actigraph is handed over by the participant, but not immediately stopped.
    ## If Activity in Last 5 observations is on average zero, Skip to Last Activity:
    ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
    ACTdata.1.sub.last5act.active <- sum(ACTdata.1.sub.last5act, na.rm = TRUE) >= (5 * length(ACTdata.1.sub.last5act)) # Is there on average more than 5 counts per obs?
    print("Task: Checking for Activity in Last 5 observations")
    if (ACTdata.1.sub.last5act.active == FALSE) {
      print("Warning: No Activity in Last 5 observations!")
      print("Last 5 Activity Counts, before Correction:")
      print(ACTdata.1.sub.last5act)
      ACTdata.1.sub <- ACTdata.1.sub[1:max(which(ACTdata.1.sub$Activity >= (5 * length(ACTdata.1.sub.last5act)))), ] # Shortens data untill reached last activity
      ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
      ACTdata.overview$last5act.active[i] <- FALSE
      print("Last 5 Activity Counts, after Correction:")
      print(ACTdata.1.sub.last5act)
      print("Task DONE: Dataset Skipped to last Activity.")
      print("")
    } else {
      print("Task OK: Dataset contained Activity in Last 5 observations.")
      print("")
    }


    ## Update overview file:
    ## Write number of observations, total recording time, and end date/time, as these values
    ## might have been altered after previous data-management steps.
    ACTdata.overview[i, "numberofobs2"] <- nrow(ACTdata.1.sub)
    ACTdata.overview[i, "recordingtime2"] <- round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2)
    ACTdata.overview[i, "end2"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview


    ## Step 2.3: Write managed data to file for analyses-------------------------------------------
    ## Create a new directory for writing managed data files.
    ensure_dir(paths$managed_dir)
    name <- paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]))
    newdir <- file.path(paths$managed_dir, name)
    ensure_dir(newdir)
    ## Write managed data:
    ACTdata.1.sub$Date <- format(ACTdata.1.sub$Date, "%Y-%m-%d %H:%M:%S")
    write.table(ACTdata.1.sub, quote = FALSE, row.names = FALSE,
      col.names = FALSE, file = file.path(newdir, paste(gsub(pattern = ".csv",
        replacement = "", x = ACTdata.files[i]), "MANAGED.txt")))

    ## Step 2.4: Initialising analyses and funtionalities--------------------------------------
    ## Description: .....

    ## Read managed dataset for analyses and functionalities:
    CRV.data <- read.table(file = file.path(newdir, paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]), "MANAGED.txt")),
      stringsAsFactors = FALSE)
    colnames(CRV.data) <- c("Date", "Time", "Activity")


    sleepdata.overview <- NULL
    rollingwindow.results <- NA
    ## Moving/Rolling Window
    ## Check first if Moving Window is required, as this requires it's own analysis calls.
    # ! Add Sleep-analysis for Rolling Window!
    # ! Add possibility to change 'jump-length' of rolling window (now 1 day) to multiple days!
    if (movingwindow) {
      ## Assign results from rolling window (see ?run_rolling_window):
      rollingwindow.results <- run_rolling_window(x = CRV.data,
        window = (1440 * (movingwindow.size)),
        jump = (1440 * (movingwindow.jump)),
        myACTdevice = myACTdevice,
        ACTdata.1.sub = ACTdata.1.sub)

      ## Initialise normal circadian rhythm analysis without moving window:
    } else {
      if (circadian_analysis) {
        ## Use the nparcalc{ACTman} function to calculate circadian rhythm variables over
        ## the whole period:

        r2 <- nparcalc(myACTdevice = myACTdevice, movingwindow = movingwindow, CRV.data = CRV.data, ACTdata.1.sub = ACTdata.1.sub)

        ## Attach r2 output to overview
        ACTdata.overview[i, "r2.IS"] <- r2$IS
        ACTdata.overview[i, "r2.IV"] <- r2$IV
        ACTdata.overview[i, "r2.RA"] <- round(r2$RA, 2)
        ACTdata.overview[i, "r2.L5"] <- r2$L5
        ACTdata.overview[i, "r2.L5_starttime"] <- r2$L5_starttime
        ACTdata.overview[i, "r2.M10"] <- round(r2$M10, 2)
        ACTdata.overview[i, "r2.M10_starttime"] <- r2$M10_starttime
      }
    }
    ## If a comparison with another actigraphy R package is required, run nparACT_base_loop{nparACT}:
    if (nparACT_compare) {
      if (!requireNamespace("nparACT", quietly = TRUE)) {
        stop("nparACT_compare = TRUE requires the 'nparACT' package. Install it with install.packages('nparACT').")
      }
      ## Use nparACT Package to calculate circadian rhythm variables:
      r <- nparACT::nparACT_base_loop(path = newdir, SR = 1 / 60, fulldays = TRUE, plot = TRUE)

      ## Attach nparACT output to overview
      ACTdata.overview[i, "IS"] <- r$IS
      ACTdata.overview[i, "IV"] <- r$IV
      ACTdata.overview[i, "RA"] <- r$RA
      ACTdata.overview[i, "L5"] <- r$L5
      ACTdata.overview[i, "L5_starttime"] <- r$L5_starttime
      ACTdata.overview[i, "M10"] <- r$M10
      ACTdata.overview[i, "M10_starttime"] <- r$M10_starttime
    }

    ## Write rollingwindow.results to .CSV, if requested
    ensure_dir(paths$results_dir)

    if (movingwindow) {
      write.table(rollingwindow.results,
        file = file.path(paths$results_dir, paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 4)),
          "-rollingwindow-results.csv", sep = "")),
        row.names = FALSE, sep = ",")
    }


    ## Sleep Analysis:
    ## Use the sleepdata_overview{ACTman} function to calculate sleep variables over
    ## the whole period.
    if (iwantsleepanalysis) {
      sleepdata.overview <- sleepdata_overview(workdir = workdir, actdata = ACTdata.1.sub, i = i, lengthcheck = lengthcheck, ACTdata.files = ACTdata.files, on_missing_markers = on_missing_markers)
    }

    ## Actogram:
    ## Use the plot_actogram{ACTman} function to plot an Actogram of the whole period.
    if (plotactogram != FALSE) {
      plot_actogram(workdir = workdir, ACTdata.1.sub = ACTdata.1.sub, i = i, plotactogram = plotactogram,
        rollingwindow.results = rollingwindow.results, i_want_EWS = i_want_EWS)
    }


    ## Report progress in console:
    print(paste("--------------------------------------", "END OF DATASET", i, "---", "@",
      round(i * (100 / length(ACTdata.files))), "% DONE",  "--------------------------------------"))
  }


  ## Step 3: After loop processing-----------------------------------------------------------------

  ## Transform negative recordingtimes to positive:
  ACTdata.overview$recordingtime <- ((ACTdata.overview$recordingtime)^2)^(1 / 2)
  ACTdata.overview$recordingtime2 <- ((ACTdata.overview$recordingtime2)^2)^(1 / 2)

  ## Assign zero to missings column in overview when there are no missings:
  ACTdata.overview[is.na(ACTdata.overview[, "missings"]), "missings"] <- 0

  ## Update overview if comparison to nparACt is not required:
  if (!nparACT_compare) {
    ACTdata.overview["IS"] <- NULL
    ACTdata.overview["IV"] <- NULL
    ACTdata.overview["RA"] <- NULL
    ACTdata.overview["L5"] <- NULL
    ACTdata.overview["L5_starttime"] <- NULL
    ACTdata.overview["M10"] <- NULL
    ACTdata.overview["M10_starttime"] <- NULL
    colnames(ACTdata.overview) <- gsub(pattern = "r2.", x = colnames(ACTdata.overview), replacement = "")
  }

  ## Subset experimental variables
  ACTdata.1.sub.expvars <- ACTdata.overview[c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime")]


  ## Write results of circadian analysis and overview to .CSV
  ensure_dir(paths$results_dir)

  if (circadian_analysis) {
    write.table(ACTdata.1.sub.expvars, file = file.path(paths$results_dir, "ACTdata_circadian_res.csv"), sep = ",", row.names = FALSE)
  }

  ## Write ACTdata.overview to .CSV
  write.table(ACTdata.overview, file = file.path(paths$results_dir, "ACTdata_overview.csv"), sep = ",", row.names = FALSE)


  ## Returned result.
  ## Returned result: a single actman_result object instead of a different
  ## bare data frame depending on which flags were set, so callers no
  ## longer need to guess what a given call returns. $overview is always
  ## present (and, when nparACT_compare = FALSE, includes the circadian
  ## columns merged in, same content as before); $circadian/$sleep/
  ## $rolling_window are NULL when the corresponding analysis wasn't run.
  ## NOTE: when processing multiple files, $sleep and $rolling_window
  ## reflect only the last file processed (matching prior behavior --
  ## sleepdata.overview/rollingwindow.results were never accumulated across
  ## files; each file's own results are still written to disk in full via
  ## the per-file CSV writes above).
  structure(
    list(
      overview = ACTdata.overview,
      circadian = if (circadian_analysis) ACTdata.1.sub.expvars else NULL,
      sleep = if (iwantsleepanalysis) sleepdata.overview else NULL,
      rolling_window = if (movingwindow) rollingwindow.results else NULL
    ),
    class = "actman_result"
  )
}

#' print.actman_result
#'
#' Console print method for the object returned by `ACTman()`. Shows a
#' short summary of which analyses ran and how large each result is,
#' rather than dumping every component's full contents.
#'
#' @param x An `actman_result` object.
#' @param ... Ignored (kept for S3 method compatibility).
#'
#' @return `x`, invisibly.
#' @export
print.actman_result <- function(x, ...) {
  cat("<actman_result>\n")
  cat(sprintf("  $overview:       %d file(s) x %d column(s)\n", nrow(x$overview), ncol(x$overview)))
  cat(sprintf("  $circadian:      %s\n", if (is.null(x$circadian)) "NULL (circadian_analysis = FALSE)" else sprintf("%d file(s) x %d column(s)", nrow(x$circadian), ncol(x$circadian))))
  cat(sprintf("  $sleep:          %s\n", if (is.null(x$sleep)) "NULL (iwantsleepanalysis = FALSE)" else sprintf("%d night(s) x %d column(s)", nrow(x$sleep), ncol(x$sleep))))
  cat(sprintf("  $rolling_window: %s\n", if (is.null(x$rolling_window)) "NULL (movingwindow = FALSE)" else sprintf("%d window(s) x %d column(s)", nrow(x$rolling_window), ncol(x$rolling_window))))
  invisible(x)
}

#' Example actigraphy data
#'
#' A real, anonymized MotionWatch 8 (CamNtech) actigraphy recording: ~33
#' days (2017-07-05 to 2017-08-07) at 30-second epochs (94,721
#' observations), already extracted to the 3-column Date/Time/Activity form
#' `ACTman()` produces internally after reading a raw MW8 export (i.e. this
#' is what `ACTdata.1.sub` looks like partway through the pipeline, not a
#' raw device file -- for a raw-format example file usable directly with
#' `ACTman(myACTdevice = "MW8", ...)`, see
#' `system.file("extdata", "example-mw8-participant.csv", package =
#' "ACTman")`, a ~7-day subset of the same recording in real MW8 export
#' format). No missing values. See `vignette("actman-intro")` for a full
#' walkthrough.
#'
#' @format A data frame with 94,721 rows and 3 variables:
#' \describe{
#'   \item{A}{Date, as \code{"YYYY-MM-DD"}}
#'   \item{B}{Time, as \code{"HH:MM:SS"} (30-second epochs)}
#'   \item{C}{Activity count for that epoch}
#' }
#'
#' @name ACTdata.1
#' @docType data
#' @author Yoram K. Kunkels \email{y.k.kunkels@umcg.nl}
#' @references Kunkels, Y. K., Knapen, S. E., Zuidersma, M., Wichers, M.,
#'   Riese, H., & Emerencia, A. C. (2020). ACTman: Automated preprocessing
#'   and analysis of actigraphy data. \emph{Journal of Science and Medicine
#'   in Sport}, 23(5), 481-486.
#' @keywords data
NULL
