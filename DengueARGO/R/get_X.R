#' Get the X Matrix for Model Fitting
#'
#' This function generates the X matrix required for different prediction models by processing case and Google Trends (GT) data.
#'
#' @param start The start date for generating the X matrix, in "YYYY-MM-DD" format.
#' @param end The end date for generating the X matrix, in "YYYY-MM-DD" format.
#' @param case A data frame of case data, with Dengue case variable named "N_cases" and date variable named "date".
#' @param GT A data frame of Google Trends data.
#' @param model A string specifying the model type ("ARGO", "SAR", "GT", or "SAR_GT").
#'
#'
#' @return A matrix X used for fitting the specified model.
#' @examples
#' get_X("2020-01-01", "2020-12-31", case_data, GT_data, "ARGO")
#' @export
get_X <- function(start, end, case, GT, model){
  # This function is to get the X matrix according to
  # the need of different models
  if(model != "ARGO" & model != "SAR" & model != "GT" & model != "SAR_GT"){
    return("Error! Unexpected model")
  }

  if(model == "ARGO"){
    lags <- c(1:12,24)
  }else if(model == "SAR" | model == "SAR_GT"){
    lags <- c(1:3,12,24)
  }

  if(model != "GT"){
    # Get the case lag data
    case_lag <- NULL
    for (i in seq_along(lags)) {
      # Generate the data
      generated_data <- lag_Gen(case, start, end, lags[i])

      # Initialise the dafa frame
      if (i == 1) {
        case_lag <- as.data.frame(generated_data)
        colnames(case_lag) <- paste0("lag_", lags[i])
      } else {
        # Paste it to the data frame
        case_lag[paste0("lag_", lags[i])] <- generated_data
      }
    }
  }

  if(model != "SAR"){
    # Get the GT data
    GT_out <-GT %>%mutate(YearMonth = format(as.Date(X), "%Y-%m")) %>%
      group_by(YearMonth) %>%
      summarise(across(2:11, sum, na.rm = TRUE)) %>%
      mutate(YearMonthDay =as.Date(paste0(YearMonth,"-15"))) %>%
      filter(YearMonthDay >= start & YearMonthDay <= end) %>%
      mutate(across(2:11,log))
  }

  if(model == "ARGO" | model == "SAR_GT"){
    X <- as.matrix(cbind(case_lag,GT_out[,2:11]))
  }else if(model == "SAR"){
    X <- as.matrix(case_lag)
  }else if(model == "GT"){
    X <- as.matrix(GT_out[,2:11])
  }

  # Return the X
  return(X)
}
