#' Compare Prediction Performance Across Models
#'
#' Evaluates the performance of various predictive models by calculating performance metrics 
#' for each model's forecasts compared to actual data. Optionally, it also compares each model's 
#' performance relative to a baseline "Naive" model. The function expects a data frame where the 
#' first column contains the actual values, and subsequent columns contain predicted values from 
#' different models. The function leverages `get_Measurement` for calculating metrics such as RMSE, MAE, etc.
#'
#' @param data A data frame where the first column contains actual values and the subsequent 
#'             columns contain predictions from different models. Column names should reflect model names.
#' @param relative_to_naive A logical flag indicating whether to compute performance metrics relative 
#'                          to those of a "Naive" model, which is assumed to be the last column in `data`.
#'                          If `TRUE`, metrics for each model are divided by the corresponding metric 
#'                          of the Naive model to assess performance relative to this baseline. 
#'                          Defaults to `TRUE`.
#'
#' @return A data frame containing performance metrics (RMSE, MAE, RMSPE, MAPE, and CORR) for each model. 
#'         If `relative_to_naive` is TRUE, these metrics are presented relative to the Naive model's performance. 
#'         The data frame includes model names and rounded metrics for clearer comparison.
#'
#' @examples
#' # Assuming `model_predictions` is your data frame with actual values in the first column and
#' # predictions from various models in subsequent columns
#' compare_metrics <- compare_Measurement(model_predictions, relative_to_naive = TRUE)
#' @export


compare_Measurement <- function(data,
                                relative_to_naive = TRUE){
  # Input data with first column to be the real value
  # and other columns are predicted values
  # And model names
  
  # Save the col names to use it in the end
  Models <- colnames(data)[-1]
  
  
  # Initialize Measurements data frame
  Measurements <- data.frame()
  
  # Get prediction measurements for each model
  for(j in 1:(ncol(data)-1)){
    if(j == 1){
      Measurements <- get_Measurement(data[, 1], data[, j+1], F)
    }else{
      Measurements <- rbind(Measurements, get_Measurement(data[, 1], data[, j+1], F))
    }
  }
  
  Measurements <- cbind(Models, Measurements)
  
  # If take values that are relative to naive
  if(relative_to_naive){
    # Get the measurement of Naive model
    naive_values <- unlist(Measurements[nrow(Measurements), -1])
    
    
    # Divided by Naive
    for(i in 1:(nrow(Measurements) - 1)){
      Measurements[i, c(-1,-6)] <- Measurements[i, c(-1,-6)] / naive_values
    }
    
    # Move Naive till the end 
    Measurements <- rbind(Measurements[-nrow(Measurements), ], Measurements[nrow(Measurements), ])
  }
  
  
  Measurements[,-1] <- round(Measurements[,-1],3)
  
  return(Measurements)
}