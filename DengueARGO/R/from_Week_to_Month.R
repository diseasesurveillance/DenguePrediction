#' Convert Weekly Data to Monthly Data
#'
#' Aggregated data often comes in weekly format but some analyses require monthly aggregation. This function converts weekly
#' aggregated data into monthly aggregates by summing the values within each month. The conversion assumes that the date
#' provided in the data is the start of the week. It outputs a data frame with a single date entry for each month, which
#' represents the aggregated sum of cases for that month. The date is set to the 15th of the respective month.
#'
#' @param data A data frame containing the weekly data, with the date variable named "date".
#' @param type A string specifying the type of data, defaulting to "case" which expects the second column to be "N_cases".
#'             Currently only supports the default "case" type.
#'
#' @return A data frame with the dates converted to the first of each month and the weekly data aggregated into monthly sums.
#'         The resulting data frame will have two columns: "date" (of class Date) and "N_cases".
#' @examples
#' weekly_cases <- data.frame(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "week"),
#'                            N_cases = sample(1:10, 53, replace = TRUE))
#' monthly_cases <- from_Week_to_Month(weekly_cases, type = "case")
#' @export
from_Week_to_Month <- function(data,
                               type = "case"){
  # Sometimes data is aggregated by week while we need data
  # in Month, so this is the function to convert.

  if(type == "case"){
    colnames(data) <- c("date","N_cases")

    # Convert the default weekly cases into Monthly
    data_out <- data %>% mutate(
      Year = year(date),
      Month = month(date),) %>%
      group_by(Year, Month) %>% # Sum the case by Year - Month
      summarise(N_cases = sum(N_cases, na.rm = TRUE), .groups = "drop") %>% ungroup() %>%
      mutate(date = as.Date(paste0(Year,"-",Month,"-","15"))) %>%
      select(date, N_cases)
  }

  return(data_out)
}
