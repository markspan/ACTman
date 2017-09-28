###########################################################
### ACTman package                                      ###
### Script authors: Yoram Kunkels & Stefan Knapen       ###
### Contributors: Ando Emerencia                        ###
### Date: 28-09-2017                                    ###
### Supported devices: Actiwatch 2 Respironics          ###
###                    & MW8                            ###
### > Made ACTman function & added arguments            ###
### > Runs sleep or CRV analyses                        ###
###~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~###
#' Loop script
#'
#' This script loops and does some stuff.
#'
#' @param workdir The working directory of the script.
#! Zou je beschrijvingen van de andere parameters hieronder kunnen invullen?
#' @param myACTdevice Name of the input device used. Should be either Actiwatch2 or MW8.
#' @param iwantsleepanalysis
#' @param plotactogram
#' @param selectperiod
#' @param startperiod
#' @param daysperiod
#' @param tune
#'
#' @return nothing
#' @examples
#' \dontrun{
#' ACTman(workdir = workdir, myACTdevice = myACTdevice, iwantsleepanalysis = FALSE,
#'        plotactogram = FALSE, selectperiod = FALSE, startperiod = "2016-10-03 00:00:00",
#'        daysperiod = 14, tune = FALSE)
#' }
#' @export
ACTman <- function(workdir = "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata2",
                   myACTdevice = "Actiwatch2", iwantsleepanalysis, plotactogram, selectperiod,
                   startperiod, daysperiod, tune) {

## Step 1: Basic Operations:
# Load and/or install required libraries
#! Dit is niet nodig als je de functie exporteert uit de package
#! Deze regels kun je dus weghalen.
#if(!require('nparACT')){install.packages('nparACT', dep = TRUE)};library('nparACT')
#if(!require('gridExtra')){install.packages('gridExtra', dep = TRUE)};library('gridExtra')
#if(!require('beepr')){install.packages('beepr', dep = TRUE)};library('beepr')

# Set working directory
# workdir <- "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata3" # mydata2 = Actiwatch2, mydata3 = MW8 sleep
setwd(workdir)

#! De onderstaande regels kun je weghalen omdat ze worden meegegeven in de functieaanroep.
# Set ACTman function arguments
# myACTdevice <- "MW8" # Either Actiwatch2 or MW8
# iwantsleepanalysis <- TRUE # TRUE for mydata3, FALSE for others

# List files and initiate overview file
pattern_file <- ""
#! Is dit een generieke conventie die mensen buiten dit project ook snappen?
if (iwantsleepanalysis) { # iwantsleepanalysis determines input .csv's because of added sleeplog .csv
  pattern_file <- "NK_data.csv"
} else {
  pattern_file <- ".csv"
}
ACTdata.files <- list.files(getwd(), pattern = pattern_file)
#! Misschien wil je deze files eerst nog even sorteren op bestandsnaam? Anders kan de volgorde verschillen
#! op verschillende platforms geloof ik.

ACTdata.overview <- data.frame("filename" = ACTdata.files, "start" = NA, "end" = NA, "end2" = NA , "end3" = NA,
                               "numberofobs" = NA, "numberofobs2" = NA, "numberofobs3" = NA, "recordingtime" = NA,
                               "recordingtime2" = NA, "recordingtime3" = NA, "summertime.start" = NA,
                               "summertime.end" = NA, "missings" = NA, "IS" = NA, "IV" = NA, "RA" = NA, "L5" = NA,
                               "L5_starttime" = NA, "M10" = NA, "M10_starttime" = NA, "r2.IS" = NA, "r2.IV" = NA,
                               "r2.RA" = NA, "r2.L5" = NA, "r2.L5_starttime" = NA, "r2.M10" = NA, "r2.M10_starttime" = NA,
                               "lengthcheck" = NA, "last5act.active" = NA)
# Initiate loop parameters
i <- 1 # set i
secshour <- 60*60 # Seconds per hour
secsday <- 24*secshour # Seconds per day
secs14day <- secsday*14 # Seconds in 14 days
minsaday <- (secsday/60) # Minutes per day

# Semantic checks
if (selectperiod && length(startperiod) != length(ACTdata.files)) {
  stop(paste("The number of start periods does not match the number of data files found:", startperiod, length(ACTdata.files)))
}

## END OF Step 1: Basic Operations.----------------------------------------------------------------------------

## Step 2: Loop:
## Loop Description: for each of the files listed in working directory, ...
#! Waarom is deze onderstaande stap nodig? (initializing the pdf plot)
pdf("Actigraphy Data - Plot.pdf") # Initialise .pdf plotting
for (i in 1:length(ACTdata.files)) {

  print(paste("*** Start of Dataset", i, "***"))
  print("")
  print(paste("Dataset Name:", ACTdata.overview[i, "filename"]))
  print("")

  # Read and manage data:
  if (myACTdevice == "Actiwatch2") { # Device-specific Data Management
    ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE)
    ACTdata.1.sub <- ACTdata.1[, c(4, 5, 6)]
    colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
  } else if (myACTdevice == "MW8") { # Device-specific Data Management
    ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"))
    # ACTdata.1 <- read.csv2(paste(ACTdata.files[i]), header = TRUE, stringsAsFactors = FALSE)

    ACTdata.1 <- as.data.frame(ACTdata.1[((which(ACTdata.1[, 1] == "Raw data:")) + 2):nrow(ACTdata.1), ])

    ACTdata.1.sub <- ACTdata.1
    colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
    ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)
  } else {
    stop(paste("Unknown value for myACTdevice (should be Actiwatch2 or MW8):", myACTdevice))
  }


  ## Step 2.1: Managing the Data
  # Reformat dates and times into required format for further processing:
  ACTdata.1.sub$Date <- gsub(pattern = "/20", replacement = "/", x = ACTdata.1.sub$Date) # Take only last two year digits
  #! Klopt dit formaat wel? In de vorige stap zocht je namelijk naar slashes en houd je een jaar over van twee cijfers.
  #! Ik snap deze onderstaande stap dus niet helemaal.
  ACTdata.1.sub$Date <- paste(ACTdata.1.sub$Date, ACTdata.1.sub$Time) # Merge Date Time, format: "2016-01-04 20:00:00"

  if (grepl("-", ACTdata.1.sub$Date[1])) {
    # ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date[1], "%d%m/%y %H:%M:%S") # Reformat Date
    #! Moet je deze waarde niet weer in ACTdata.1.sub$Date opslaan?
    format(as.POSIXct(ACTdata.1.sub$Date), "%d-%m-%y %H:%M:%S")
  } else {
    ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%d/%m/%y %H:%M:%S") # Reformat Date
  }

  # ACTdata.1.sub$Date <- as.POSIXct(ACTdata.1.sub$Date)

  ACTdata.1.sub$Time <- NULL # Remove empty Time variable

  # Check for empty first row and if so, remove it:
  if (all(is.na(ACTdata.1.sub[1, ]))) { ACTdata.1.sub <- ACTdata.1.sub[-1, ] } # If first row is empty, remove it.

  # Add Period Selection
  # Custom enddates should be added
  if (selectperiod) {
    # startperiod <- ACTdata.1.sub$Date[1]+secsday # Test startperiod
    startperiod.loc <- which(ACTdata.1.sub$Date == startperiod[i])

    if (daysperiod != FALSE) {
      ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(startperiod.loc + (daysperiod*minsaday))), ]
    } else {
      ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(nrow(ACTdata.1.sub))), ]
    }
  }

  start_date <- ACTdata.1.sub$Date[1]
  end_date <- ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]
  nr_obs <- nrow(ACTdata.1.sub)

  # Write start- and end dates and times of actigraph data to overview file:
  ACTdata.overview[i, "start"] <- as.character(start_date) # Write start date to overview
  ACTdata.overview[i, "end"] <- as.character(end_date) # Write end date to overview

  # Assign recordingtime as time difference between start and end date to overview file:
  ACTdata.overview[i, "recordingtime"] <- round((as.POSIXct(start_date) - as.POSIXct(end_date)), 2) # write recordingtime to overview

  # Write starting number of obs. to overview file:
  ACTdata.overview[i, "numberofobs"] <- nr_obs

  # Check for Activity in Last 5 observations:
  ACTdata.1.sub.last <- ACTdata.1.sub$Activity[(nr_obs - 4):nr_obs]

  # Check if Activity in Last 5 observations contain substantial activity (on average, 5 activity counts per obs.)
  #! Deze variabele wordt nergens gebruikt?
  ACTdata.1.sub.done <- sum(ACTdata.1.sub.last) > 5*length(ACTdata.1.sub.last)

  # Identify Last Whole 24 Hour component and its Position: (EDITED for NK_data)
  #! Aan mezelf: als we data hebben dit doorlopen of het wel allemaal klopt.
  ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub[tail(grep("00:00:00", ACTdata.1.sub$Date), 2), "Date"]
  ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub.lastwhole24h[1]
  # ACTdata.1.sub.lastwhole24h <- as.POSIXct(ACTdata.overview[i, "start"]) + secs14day - secsday # Last whole 24 hour component

  # ACTdata.1.sub.lastwhole24h.pos <<- which(ACTdata.1.sub$Date == as.character(ACTdata.1.sub.lastwhole24h))
  ACTdata.1.sub.lastwhole24h.pos <- tail(grep("00:00:00", ACTdata.1.sub$Date), 2)[1]
  # ACTdata.1.sub.lastwhole24h.pos <- tail(which((grepl(substr(ACTdata.1.sub.lastwhole24h, 12, 19), ACTdata.1.sub$Date)) == TRUE), n = 1) # Position of Last whole 24 hour component

  # ACTdata.1.sub.lastwhole24h.pos <- which(ACTdata.1.sub$Date == ACTdata.1.sub.lastwhole24h) # Position of Last whole 24 hour component

  #! De deze detectie van zomer- en/of wintertijd is niet generiek. R kan deze detectie gewoon zelf prima voor je doen,
  #! zolang je al je date/time objecten als POSIXct objecten opslaat. Hieronder een paar regels die ik hierover eerder
  #! heb geschreven:
  #! I think the best way to handle times and dates is probably with the built-in class POSIXct.
  #!
  #! If we have a string that represents a time and date, we can convert it to a POSIXct object using strptime:
  #!
  #!  mytimedatea = as.POSIXct(strptime('2017-03-06 11:19:03',format='%Y-%m-%d %H:%M:%S'),tz="Europe/Amsterdam")
  #!
  #! Or, if you have the values as numbers, you can use ISOdate:
  #!  mytimedateb = ISOdate(2017,3,6,11,19,3,tz="Europe/Amsterdam")
  #!
  #! These two statements return identical objects.
  #!
  #! Now that you have the times as POSIXct objects, you can change their timezones very easily like this:
  #!  attr(mytimedatea, "tzone") <- "UTC"
  #!
  #! Of course you can also print the time using a different format, e.g.:
  #!  format(mytimedatea,'%d %B %Y, %H:%M:%S %z')
  #!
  #! Maar dit alles lijkt me typisch iets dat ik wel kan doen (gegeven wat meer tijd).
  # Summertime start and end Detection Parameters
  summertime.start.2015 <- "2015-03-29 02:00:00" # Assign summertime start date for 2015
  summertime.start.2014 <- "2014-03-30 02:00:00" # Assign summertime start date for 2014
  summertime.start.indata <- summertime.start.2014 %in% ACTdata.1.sub$Date | summertime.start.2015 %in% ACTdata.1.sub$Date # Detect if summertime start is in dataset
  #! De rest van deze summertime variabelen wordt niet gebruikt? (regels weghalen?)
  # summertime.start.pos <- which(ACTdata.1.sub$Date == summertime.start) # Position of summertime start
  # summertime.end <- "2015-10-25 02:00:00" # Assign summertime end date for 2015
  # summertime.end.indata <- summertime.end %in% ACTdata.1.sub$Date # Detect if summertime end is in dataset
  # summertime.end.pos <- which(ACTdata.1.sub$Date == summertime.end) # Position of summertime end

  ## Summertime start Detection and 14day length correction
  print("Task 1: Summertime Start Detection and Correction")
  if (summertime.start.indata == TRUE) {
    print("Warning: Start of Summertime Detected!")
    ACTdata.1.sub.14day <- as.POSIXct(ACTdata.1.sub$Date[1]) + secs14day # Start Date plus 14 days
    ACTdata.1.sub.14day <- ACTdata.1.sub.14day - secshour
    ACTdata.overview$summertime.start[i] <- TRUE
    print("Action: Subtracted the one hour excess from 14day length.")
    print("Task 1 DONE: Dataset corrected for Summertime.")
    print("")
  } else {
    ACTdata.1.sub.14day <- as.POSIXct(ACTdata.1.sub$Date[1]) + secs14day # Start Date plus 14 days
    print("Task 1 OK: No Start of Summertime in Dataset.")
    print("")
  }

  ## If Dataset is longer than Start Date plus 14 days, Remove data recorded thereafter:
  print("Task 2: Detecting if Dataset is longer than Start Date plus 14 full days")
  if (ACTdata.1.sub[nrow(ACTdata.1.sub), "Date"] > ACTdata.1.sub.14day) {
    print("Warning: Dataset is longer than Start Date plus 14 full days!")

    # ACTdata.1.sub <- ACTdata.1.sub[1:which(ACTdata.1.sub$Date == ACTdata.1.sub.14day), ]
    ACTdata.1.sub <- ACTdata.1.sub[1:((secs14day/60) + 1), ]

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

  ## Remove NA's
  print("Task 6: Reporting NA's")
  ACTdata.overview[i, "missings"] <- table(is.na(ACTdata.1.sub))["TRUE"]  # write missings to overview
  ACTdata.1.sub <- na.omit(ACTdata.1.sub)
  print(paste("Number of NA's in this Dataset:", ACTdata.overview[i, "missings"]))
  print("")

  ## If Activity in Last 5 observations is on average zero, Skip to Last Activity:
  ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
  ACTdata.1.sub.last5act.active <- sum(ACTdata.1.sub.last5act) >= (5 * length(ACTdata.1.sub.last5act)) # Is there on average more than 5 counts per obs?
  print("Task 7: Checking for Activity in Last 5 observations")
  if (ACTdata.1.sub.last5act.active == FALSE) {
    print("Warning: No Activity in Last 5 observations!")
    print("Last 5 Activity Counts, before Correction:")
    print(ACTdata.1.sub.last5act)
    ACTdata.1.sub <- ACTdata.1.sub[1:max(which(ACTdata.1.sub$Activity >= (5 * length(ACTdata.1.sub.last5act)))), ] # Shortens data untill reached last activity
    ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
    ACTdata.overview$last5act.active[i] <- FALSE
    print("Last 5 Activity Counts, after Correction:")
    print(ACTdata.1.sub.last5act)
    print("Task 7 DONE: Dataset Skipped to last Activity.")
    print("")
  } else {
    print("Task 7 OK: Dataset contained Activity in Last 5 observations.")
    print("")
  }

  ## END OF Step 2.1: Managing the Data
  ## Assign Original & Managed data as global
  ACTdata.1 <<- ACTdata.1
  ACTdata.1.sub <<- ACTdata.1.sub

  # Update overview file after NA- and non-activity removal
  ACTdata.overview[i, "numberofobs3"] <- nrow(ACTdata.1.sub)
  ACTdata.overview[i, "recordingtime3"] <- round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2)
  ACTdata.overview[i, "end3"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview

  ## Use nparACT Package to calculate Experimental Variables
  ## Pre-process!!

  dir.create(file.path(workdir, "Managed Datasets"))
  setwd(file.path(workdir, "Managed Datasets"))

  wd <- getwd()
  name <- paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]))
  newdir <- paste(wd, name, sep = "/")
  dir.create(newdir)
  setwd(newdir)

  write.table(ACTdata.1.sub, row.names = FALSE, col.names = FALSE,
              file = paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]), "MANAGED.txt"))

  # Calculate IS, etc. with IS Calculation Module (Check version for correct operation!)
  #! Dit staat nu in een ander bestand.
  r2 <- nparcalc(lastwhole24h.pos = ACTdata.1.sub.lastwhole24h.pos,
                      newdir = newdir,
                      myACTdevice = myACTdevice,
                      ACTdata_file = ACTdata.files[i])

  # Attach r2 output to overview
  ACTdata.overview[i, "r2.IS"] <- r2$IS
  ACTdata.overview[i, "r2.IV"] <- r2$IV
  ACTdata.overview[i, "r2.RA"] <- round(r2$RA, 2)
  ACTdata.overview[i, "r2.L5"] <- round(r2$L5, 2)
  ACTdata.overview[i, "r2.L5_starttime"] <- as.character(strftime(r2$L5_starttime, format = "%H:%M:%S"))
  ACTdata.overview[i, "r2.M10"] <- round(r2$M10, 2)
  ACTdata.overview[i, "r2.M10_starttime"] <- as.character(strftime(r2$M10_starttime, format = "%H:%M:%S"))


  ## Use nparACT Package to calculate Experimental Variables

  # Calculate IS, etc. with nparACT
  r <- nparACT_base_loop(path = newdir, SR = 1/60, fulldays = T, plot = F)

  # Attach nparACT output to overview
  ACTdata.overview[i, "IS"] <- r$IS
  ACTdata.overview[i, "IV"] <- r$IV
  ACTdata.overview[i, "RA"] <- r$RA
  ACTdata.overview[i, "L5"] <- r$L5
  ACTdata.overview[i, "L5_starttime"] <- r$L5_starttime
  ACTdata.overview[i, "M10"] <- r$M10
  ACTdata.overview[i, "M10_starttime"] <- r$M10_starttime


  # Reset working directory to main directory
  setwd(workdir)

  # Plot data (Actogrammen)
  if (plotactogram) {
    #! Dit bestand staat niet in de github repo?
    source(file = "Actogram ACTman READYv3.R",
           local = TRUE) # This script has to be put in the data folder(e.g. "data2")(!!)
  }

  ## Sleep Analysis Source
  ## Loop for sleep_calc
  ## (!!) "Sleep_calculation_functional v1" still has to be made dynamic, now only woks for NK_data (!!)
  if (iwantsleepanalysis) {
    #! Waarom is een trycatch hier nodig?
    tryCatch({
      #! Je kan beter gewoon een functie definieren, bv sleep_calculation.
      #! en dan noem je het bestand hetzelfde, nl. sleep_calculation.r
      #! en dan voeg je die functie toe aan het NAMESPACE bestand
      #! en dan kan je m hier gewoon aanroepen als functie.
      source(file = "Sleep_calculation_functional v4WORKS.R",
             local = TRUE) # This script has to be put in the data folder(e.g. "data2")(!!)

      # Reset working directory to main directory
      setwd(workdir)

      sleepdata.overview <<- sleepdata.overview
      # # Attach Sleep Analysis output to overview
      # ACTdata.overview[i, "sleep.start"] <- sleep.start
      # ACTdata.overview[i, "sleep.end"] <- sleep.end
      # ACTdata.overview[i, "sleep.efficiency"] <- SleepEfficiency
      # ACTdata.overview[i, "sleep.latency"] <- SleepLatency
    })
  }



  print(paste("--------------------------------------", "END OF DATASET", i, "---", "@", round(i*(100/length(ACTdata.files))), "% DONE",  "--------------------------------------"))
}
dev.off()

## END OF Step 2: Loop.----------------------------------------------------------------------------------------

## Step 3: After loop processing:
# Transform negative recordingtime to positive
ACTdata.overview$recordingtime <- ((ACTdata.overview$recordingtime)^2)^(1/2)
ACTdata.overview$recordingtime2 <- ((ACTdata.overview$recordingtime2)^2)^(1/2)
# Assign zero to missings without missings
ACTdata.overview[is.na(ACTdata.overview[, "missings"]), "missings"] <- 0
# Subset experimental variables
ACTdata.1.sub.expvars <- ACTdata.overview[c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime", "recordingtime2")]
colnames(ACTdata.1.sub.expvars) <- c("IS", "IV", "RA", "L5", "L5 Start time", "M10", "M10 Start time", "No. of Days")

# Export Experimental variables to .pdf
pdf("Experimental Variables - Table.pdf")
grid.table(ACTdata.1.sub.expvars)
dev.off()

if (iwantsleepanalysis) {
  View(sleepdata.overview)
} else {
  View(ACTdata.overview)
}

if (tune) {
  beep(3)
}

}

## END OF Step 3: After loop processing.------------------------------------------------------------------------


## Step 4: Start up ACTman

# startperiod.dates <- c("2014-04-18 18:30:00", "2014-05-03 17:00:00", "2015-02-05 17:00:00", "2014-08-15 15:00:00", "2015-04-03 15:00:00")

# ACTman(workdir = workdir, myACTdevice = myACTdevice, iwantsleepanalysis = FALSE, plotactogram = TRUE,
#        selectperiod = TRUE, startperiod = "2014-04-18 18:30:00",
#        tune = FALSE)

startperiod.dates <- c("2016-10-03 00:00:00")

ACTman(iwantsleepanalysis = FALSE, plotactogram = FALSE,
       selectperiod = FALSE, startperiod = startperiod.dates, daysperiod = 14,
       tune = FALSE)




