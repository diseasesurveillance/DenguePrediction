#' Produce Lagged Data
#'
#' This function produces lagged data for the specified period and lag months.
#'
#' @param data A data frame containing the data to be lagged.
#' @param start The start date from which to begin lagging, in "YYYY-MM-DD" format.
#' @param end The end date for the lagging period, in "YYYY-MM-DD" format.
#' @param lag_m The number of months to lag by.
#'
#' @return A vector containing the lagged N_cases for the specified period.
#' @examples
#' lag_Gen(data, "2020-01-01", "2020-12-31", 3)
#' @export
lag_Gen <- function(data, start, end, lag_m) {
  # This function is to produce lagged data
  # The data have to return the N_cases

  # Change the type of date
  data$date <- as.Date(data$date)

  start_lag <- as.Date(start) %m-% months(lag_m)
  end_lag <- as.Date(end) %m-% months(lag_m)
  out <- data %>% filter(date >= start_lag & date <= end_lag)
  return(out$N_cases)
}

