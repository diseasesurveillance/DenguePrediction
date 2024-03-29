#' Produce Lagged Data
#'
#' This function produces lagged data for the specified period and the number of lags.
#' It supports lagging data based on "Month" or "Week" units.
#'
#' @param data A data frame containing the data to be lagged. It expects at least two columns:
#' one for the date and another for the N_cases.
#' @param start The start date from which to begin lagging, in "YYYY-MM-DD" format.
#' @param end The end date for the lagging period, in "YYYY-MM-DD" format.
#' @param lags The number of periods to lag by. The unit of this lag is determined by the `pred_unit` parameter.
#' @param pred_unit The unit of prediction and lagging. It accepts either "Month" or "Week" to indicate
#' the resolution of lagging. Defaults to "Month".
#'
#' @return A data frame containing the lagged data for the specified period. The returned data frame
#' includes the original date and N_cases columns, along with a new column for the lagged N_cases.
#' @examples
#' lag_Gen(data, "2020-01-01", "2020-12-31", 3, "Month")
#' @export
lag_Gen <- function(data, start, end, lags, pred_unit = "Month") {
  # Ensure column names are correctly set and date is in Date format
  colnames(data) <- c("date","N_cases")
  data$date <- as.Date(data$date)

  # Calculate lagged data based on the specified prediction unit
  if(pred_unit == "Week"){
    start_lag <- as.Date(start) %m-% weeks(lags)
    end_lag <- as.Date(end) %m-% weeks(lags)
  } else if(pred_unit == "Month"){
    start_lag <- as.Date(start) %m-% months(lags)
    end_lag <- as.Date(end) %m-% months(lags)
  }

  # Filter and return the lagged N_cases
  out <- data %>% filter(date >= start_lag & date <= end_lag)
  return(out$N_cases)
}
