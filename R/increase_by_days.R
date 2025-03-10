#' increase_by_days
#'
#' This function increases a given time object by a specified number of days.
#' It works with POSIXct time objects, and it correctly handles the time zone offset, 
#' ensuring that the resulting time is adjusted properly.
#'
#' The function internally converts the given time object into seconds, adds the specified
#' number of days in seconds (86400 seconds per day), and then adjusts for any time zone offsets 
#' to ensure the correct time is returned in the desired time zone.
#'
#' @param timeobj A POSIXct or character time object. This represents the initial time point 
#'                to which days will be added. The time object should be in the format recognized 
#'                by `as.POSIXct()`, and if given as a character string, it will be automatically 
#'                converted to a POSIXct object.
#' @param nr_days An integer representing the number of days to be added to the `timeobj`. 
#'                This can be positive (to increase the time) or negative (to decrease the time).
#'
#' @return A POSIXct object representing the new time after the specified number of days have been added.
#'         The time zone will be adjusted according to the system's current time zone setting.
#'
#' @examples
#' # Example 1: Increase time by 5 days
#' increase_by_days("2025-03-10 10:00:00", 5)
#'
#' # Example 2: Decrease time by 3 days
#' increase_by_days("2025-03-10 10:00:00", -3)
#'
#' @importFrom base as.POSIXct as.POSIXlt
increase_by_days <- function(timeobj, nr_days) {

  ## Step 1: Convert Days to Seconds --------------------------------------------------------------
  # Each day consists of 86400 seconds (24 hours * 60 minutes * 60 seconds).
  # This step converts the number of days to the corresponding number of seconds.
  time_in_seconds <- nr_days * 86400

  ## Step 2: Convert the Input Time Object to POSIXct -----------------------------------------
  # Convert the input time object (which can be a POSIXct object or a character string) to a POSIXct object
  # for easier manipulation.
  cttimeobj <- as.POSIXct(timeobj)

  ## Step 3: Add the Specified Time (in Seconds) to the Original Time ------------------------
  # Add the computed number of seconds (time_in_seconds) to the original POSIXct time.
  new_time <- cttimeobj + time_in_seconds

  ## Step 4: Adjust for Time Zone Offset -------------------------------------------------------
  # Time zone offsets can cause discrepancies in time calculations due to the difference between 
  # UTC time and the local time zone.
  # This step ensures that the time zone offset is taken into account.
  # The adjustment is done by comparing the GMT offset of the original and the new time objects.
  new_time <- new_time + as.POSIXlt(cttimeobj)$gmtoff - as.POSIXlt(new_time)$gmtoff

  ## Step 5: Return the New Time Object -------------------------------------------------------
  # Return the new time object with the time zone adjustment applied.
  return(new_time)
}
