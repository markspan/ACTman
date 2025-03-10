ACTman <- function(workdir = "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata",
                   sleepdatadir = paste("C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/Actogram & Sleep analysis", sep = ""),
                   myACTdevice = "Actiwatch2", iwantsleepanalysis = FALSE, plotactogram = FALSE,
                   selectperiod = FALSE, startperiod = NULL, daysperiod = FALSE, endperiod = NULL,
                   movingwindow = FALSE, movingwindow.size = 14, movingwindow.jump = 1,
                   circadian_analysis = TRUE, nparACT_compare = FALSE, na_omit = FALSE, na_impute = FALSE,
                   missings_report = TRUE, lengthcheck = TRUE, i_want_EWS = FALSE) {
  # Set working directory and initialize file list
  setwd(workdir)
  pattern_file <- ".csv"
  ACTdata.files <- sort(list.files(getwd(), pattern = pattern_file))
  
  # Remove sleep logs and marker files from data listing
  ACTdata.files <- ACTdata.files[!grepl(pattern = "sleeplog", ACTdata.files)]
  ACTdata.files <- ACTdata.files[!grepl(pattern = "markers", ACTdata.files)]
  
  # Initialize data overview
  ACTdata.overview <- data.frame("filename" = ACTdata.files, "start" = NA, "end" = NA, "end2" = NA,
                                 "numberofobs" = NA, "numberofobs2" = NA, "recordingtime" = NA,
                                 "recordingtime2" = NA, "summertime.start" = NA, "summertime.end" = NA, 
                                 "missings" = NA, "missings_perc" = NA, "IS" = NA, "IV" = NA, "RA" = NA, 
                                 "L5" = NA, "L5_starttime" = NA, "M10" = NA, "M10_starttime" = NA, "r2.IS" = NA,
                                 "r2.IV" = NA, "r2.RA" = NA, "r2.L5" = NA, "r2.L5_starttime" = NA, "r2.M10" = NA,
                                 "r2.M10_starttime" = NA, "last5act.active" = NA, "lengthcheck" = NA)
  
  secshour <- 60 * 60  # Seconds per hour
  secsday <- 24 * secshour  # Seconds per day
  secs14day <- secsday * 14  # Seconds in 14 days
  minsaday <- secsday / 60  # Minutes per day
  
  # Check for unsupported file formats (e.g., .mtn)
  if (".mtn" %in% substr(list.files(getwd()), nchar(list.files(getwd())) - 4 + 1, nchar(list.files(getwd())))) {
    message("Unsupported Actigraphy file format detected. Please convert or remove these files.")
    print(list.files(getwd())[grep(".mtn", list.files(getwd()))])
    stop()
  }

  # Process each data file
  for (i in seq_along(ACTdata.files)) {
    # Device validation
    if (myACTdevice != "MW8" && myACTdevice != "Actiwatch2") {
      stop(paste("Unknown value for myACTdevice (should be MW8 or Actiwatch2):", myACTdevice))
    }
    print(paste("*** Start of Dataset", i, "***"))
    print(paste("Dataset Name:", ACTdata.files[i]))
    
    # Load and process the data based on the device type
    if (myACTdevice == "Actiwatch2") {
      ACTdata.1 <- read.csv(ACTdata.files[i], header = FALSE)
      ACTdata.1.sub <- ACTdata.1[, c(4, 5, 6)]
      colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
    } else if (myACTdevice == "MW8") {
      ACTdata.1 <- read.csv(ACTdata.files[i], header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"))
      if (all(is.na(ACTdata.1$B)) && all(is.na(ACTdata.1$C))) {
        ACTdata.1 <- read.csv(ACTdata.files[i], header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"), sep = "\t")
      }
      if (any(ACTdata.1[, 1] == "Raw data:")) {
        ACTdata.1 <- ACTdata.1[((which(ACTdata.1[, 1] == "Raw data:")) + 2):nrow(ACTdata.1), ]
      }
      ACTdata.1.sub <- ACTdata.1
      colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
      ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)
      
      # Epoch Length Detection
      if (any(grepl(pattern = ":30", x = ACTdata.1$B[1:2]))) {
        print("Warning: 30 sec Epochs detected. Binning 30 sec Epochs into 60 sec Epochs.")
        ACTdata.TEMP <- ACTdata.1[grepl(pattern = ".*(?<!:30)$", x = ACTdata.1$B, perl = TRUE), ]
        halfminute_data <- as.numeric(ACTdata.1[grepl(pattern = ".*(?<=:30)$", x = ACTdata.1$B, perl = TRUE), ]$C)
        
        if (length(grep(pattern = ":30$", x = ACTdata.1$B[1]))) {
          halfminute_data <- tail(halfminute_data, n = -1)
        }
        if (length(grep(pattern = ":00$", x = ACTdata.1$B[length(ACTdata.1$B)]))) {
          halfminute_data <- c(halfminute_data, 0)
        }
        ACTdata.TEMP$C <- as.numeric(ACTdata.TEMP$C) + halfminute_data
        ACTdata.1.sub <- ACTdata.TEMP
        colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
        ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)
        rm(ACTdata.TEMP) # Remove temporary data object
        rm(halfminute_data)
      } else {
        print("Normal 60 sec Epochs detected. No changes made.")
      }
    }
    
    # Date and Time Formatting
    ACTdata.1.sub$Date <- gsub(pattern = "/20", replacement = "/", x = ACTdata.1.sub$Date)
    ACTdata.1.sub$Date <- paste(ACTdata.1.sub$Date, ACTdata.1.sub$Time)
    if (grepl("-", ACTdata.1.sub$Date[1])) {
      ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%Y-%m-%d %H:%M:%S")
    } else {
      ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%d/%m/%y %H:%M:%S")
    }
    ACTdata.1.sub$Time <- NULL # Remove empty Time variable

    # Handle missing data
    if (all(is.na(ACTdata.1.sub[1, ]))) { 
      ACTdata.1.sub <- ACTdata.1.sub[-1, ] 
    }

    # Select period if needed
    if (selectperiod) {
      startperiod.loc <- which(ACTdata.1.sub$Date == startperiod[i])
      if (daysperiod) {
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(startperiod.loc + (daysperiod * minsaday))), ]
      } else if (endperiod %in% ACTdata.1.sub$Date) {
        endperiod.loc <- which(ACTdata.1.sub$Date == endperiod)
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:endperiod.loc), ]
      } else {
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(nrow(ACTdata.1.sub))), ]
      }
    }
    
    # Collect dataset information
    start_date <- ACTdata.1.sub$Date[1]
    end_date <- ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]
    nr_obs <- nrow(ACTdata.1.sub)
    ACTdata.overview[i, "start"] <- as.character(start_date)
    ACTdata.overview[i, "end"] <- as.character(end_date)
    ACTdata.overview[i, "recordingtime"] <- round((as.POSIXct(start_date) - as.POSIXct(end_date)), 2)
    ACTdata.overview[i, "numberofobs"] <- nr_obs
    ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub[tail(grep("00:00:00", ACTdata.1.sub$Date), 1), ]
  }
  return(ACTdata.overview)
}
