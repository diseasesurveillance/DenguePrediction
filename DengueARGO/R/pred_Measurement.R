#' Aggregate Prediction and Measurement for Multiple Models and Periods
#'
#' This function aggregates predictions and calculates performance measurements across multiple models and prediction periods.
#'
#' @param start_d The start date for the overall prediction period.
#' @param end_d The end date for the overall prediction period.
#' @param pred_period The total number of prediction periods.
#' @param pred_length The length of each prediction period in months (either 1 or 2).
#'
#' @return A data frame of performance measurements for each model and prediction period.
#' @examples
#' pred_Measurement("2020-01-01", "2020-12-31", 12, 1)
#' @export
pred_Measurement <- function(start_d, end_d, pred_period, pred_length = 1) {
  # Validates the prediction length
  if(pred_length != 1 & pred_length != 2) {
    stop("Error! The length of prediction should be 1 or 2 (month/s)!")
  }

  # Initializes models and prediction data frame
  models <- c("ARGO", "SAR", "GT", "SAR_GT")
  pred <- data.frame(matrix(NA, nrow = pred_period * pred_length, ncol = length(models) + 1))

  # Iterates over models and prediction periods
  for (i in seq_along(models)) {
    for (j in 1:pred_period) {
      # Calculates training and prediction periods
      start_d_train <- start_d %m+% months(j - 1)
      end_d_train <- end_d %m+% months(j - 1)
      start_d_pred <- start_d %m+% months(j + pred_length - 1)
      end_d_pred <- end_d %m+% months(j + pred_length - 1)

      # Fits the model and gets predictions
      temp_pred <- as.data.frame(get_Prediction(models[i], start_d_train, end_d_train, start_d_pred, end_d_pred, case, GT))
      temp_pred <- tail(temp_pred, pred_length) # Gets the last month(s) of prediction

      # Store the result based on prediction length
      if(pred_length == 1){
        if(i == 1){
          pred[j, 1:2] <- temp_pred
        }else{
          pred[j, i+1] <- temp_pred[,2]
        }
      }else{ # for pred_length == 2
        if(i == 1){
          pred[c(2*j - 1, 2*j), 1:2] <- temp_pred
        }else{
          pred[c(2*j - 1, 2*j), i+1] <- temp_pred[,2]
        }
      }

      # Prints procedure tip
      cat("This is the", j, "/", pred_period, "prediction for model", models[i], ".\n")
    }
  }

  # Initialize Measurements data frame
  Measurements <- data.frame()

  # Get prediction measurements for each model
  for(j in 1:(ncol(pred)-1)){
    if(j == 1){
      Measurements <- get_Measurement(pred[, 1], pred[, j+1], F)
    }else{
      Measurements <- rbind(Measurements, get_Measurement(pred[, 1], pred[, j+1], F))
    }
  }

  return(Measurements)
}
