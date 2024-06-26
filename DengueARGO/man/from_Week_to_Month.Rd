% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/from_Week_to_Month.R
\name{from_Week_to_Month}
\alias{from_Week_to_Month}
\title{Convert Weekly Data to Monthly Data}
\usage{
from_Week_to_Month(data, type = "case")
}
\arguments{
\item{data}{A data frame containing the weekly data, with the date variable named "date".}

\item{type}{A string specifying the type of data, defaulting to "case" which expects the second column to be "N_cases".
Currently only supports the default "case" type.}
}
\value{
A data frame with the dates converted to the first of each month and the weekly data aggregated into monthly sums.
        The resulting data frame will have two columns: "date" (of class Date) and "N_cases".
}
\description{
Aggregated data often comes in weekly format but some analyses require monthly aggregation. This function converts weekly
aggregated data into monthly aggregates by summing the values within each month. The conversion assumes that the date
provided in the data is the start of the week. It outputs a data frame with a single date entry for each month, which
represents the aggregated sum of cases for that month. The date is set to the 15th of the respective month.
}
\examples{
weekly_cases <- data.frame(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "week"),
                           N_cases = sample(1:10, 53, replace = TRUE))
monthly_cases <- from_Week_to_Month(weekly_cases, type = "case")
}
