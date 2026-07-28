#' increase_by_days
#'
#' Adds (or subtracts, for negative \code{nr_days}) whole calendar days to a
#' date/time, preserving wall-clock time across daylight-saving-time
#' transitions.
#'
#' Uses \pkg{lubridate}'s \code{Period} arithmetic (\code{days()}), which is
#' calendar-based rather than duration-based: adding "3 days" always lands
#' on the same clock time 3 calendar days later, even if a DST transition
#' falls in between. Plain \code{POSIXct + (nr_days * 86400)} arithmetic, by
#' contrast, adds a fixed number of seconds and will land an hour off across
#' a DST transition.
#'
#' @param timeobj A POSIXct object or a character string parseable by
#'   \code{as.POSIXct()}.
#' @param nr_days Integer number of days to add (negative to subtract).
#'
#' @return A POSIXct object, \code{nr_days} calendar days after (or before)
#'   \code{timeobj}, at the same wall-clock time.
#'
#' @examples
#' increase_by_days("2025-03-10 10:00:00", 5)
#' increase_by_days("2025-03-10 10:00:00", -3)
#'
#' @importFrom lubridate days
#' @export
increase_by_days <- function(timeobj, nr_days) {
  as.POSIXct(timeobj) + lubridate::days(nr_days)
}
