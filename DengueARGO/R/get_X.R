#' Generate X Matrix for Model Fitting
#'
#' This function prepares the X matrix needed for model fitting. It processes both case data and Google Trends data,
#' applying standardization to case counts, aggregation to the specified prediction unit (weekly or monthly),
#' and generates lagged data. Additionally, it can apply a logarithmic transformation to Google Trends data
#' to handle zeros through an offset adjustment.
#'
#' @param start The starting date for generating the X matrix, specified in "YYYY-MM-DD" format.
#' @param end The ending date for generating the X matrix, specified in "YYYY-MM-DD" format.
#' @param case An optional data frame containing case data. The data frame should have a date column named "date"
#'             and a case count column named "N_cases". If not provided, case data will not be included in the X matrix.
#' @param GT An optional data frame of Google Trends data. If not provided, Google Trends data will not be included in the X matrix.
#' @param lags A numeric vector indicating the lags to apply for generating lagged case data. Defaults to `c(1:12, 24)`.
#' @param pred_unit The prediction unit for aggregation, either "W" for weekly or "M" for monthly data. Defaults to "M" for monthly.
#' @param offset_GT A small positive value added to Google Trends data before applying the logarithmic transformation to avoid
#'                  the logarithm of zero. Defaults to 0.001.
#'
#' @return A matrix `X` suitable for model fitting, containing either or both lagged case data and logarithmically transformed
#'         Google Trends data, depending on the inputs provided.
#' @examples
#' get_X("2020-01-01", "2020-12-31", case_data, GT_data, lags = c(1:12, 24), pred_unit = "M", offset_GT = 0.001)
#' @export

get_X <- function(start,
                  end,
                  case = NULL,
                  GT = NULL,
                  lags = c(1:12,24), # ARGO's setting
                  pred_unit = "Month",
                  offset_GT = 0.001){
  # This function is to get the X matrix according to
  # the need of different models
  null_GT <- is.null(GT); null_case <- is.null(case)
  if(null_GT & null_case){
    stop("Error! Please input at least one valid dataset.")
  }

  if(pred_unit != "Month" & pred_unit != "Week"){
    stop("Error! Please input pred_unit = 'Week' or 'Month' as the expected resolution.")
  }

  if(!null_case){
    # Standardize the variables for "case" data
    colnames(case) <- c("date","N_cases")
    if(pred_unit == "Month"){
      # Convert the default weekly cases into Monthly
      case <- from_Week_to_Month(case)
    }

    # Get the case lag data
    case_lag <- NULL
    for (i in seq_along(lags)) {
      # Generate the data
      generated_data <- lag_Gen(case, start, end, lags[i], pred_unit = pred_unit)

      # Initialise the dafa frame
      if (i == 1) {
        case_lag <- as.data.frame(generated_data)
        colnames(case_lag) <- paste0("lag_", lags[i])
      } else {
        # Paste it to the data frame
        case_lag[paste0("lag_", lags[i])] <- generated_data
      }
    }
    case_lag <- apply(case_lag, 2, function(x) log(x + 1))
  }

  if(!null_GT){
    # Number of queries
    query_num <- length(GT)

    # Filtering the data by Time
    if(pred_unit == "Month"){ # Monthly
      GT_out <- GT %>%
        mutate(Time_new = as.Date(paste0(Time, "-15"))) %>%
        filter(Time_new >= start_d & Time_new <= end_d) %>%
        mutate(across(2:query_num, as.numeric),
               across(2:query_num, ~log(.x + offset_GT)))
    }else{  # Weekly
      GT_out <- GT %>%
        mutate(Time = as.Date(Time, format="%d/%m/%Y")) %>%
        filter(as.Date(Time) >= start_d & as.Date(Time) <= end_d) %>%
        mutate(across(2:query_num, as.numeric),
               across(2:query_num, ~log(.x + offset_GT)))
    }
  }

  if(!null_GT & !null_case){
    X <- as.matrix(cbind(case_lag,GT_out[,2:query_num]))
  }else if(!null_case){
    X <- as.matrix(case_lag)
  }else{
    X <- as.matrix(GT_out[,2:query_num])
  }

  # Return the X matrix
  return(X)
}
