#' Model Fitting and Prediction
#'
#' Utilizes specified prediction models to fit training data, incorporating both epidemiological case data and Google Trends data. After model fitting, it forecasts case counts for a designated future period. The function supports a variety of models including ARGO, SAR, GT, and SAR_GT for a comprehensive analysis.
#'
#' @param start_train The start date for the training dataset, in "YYYY-MM-DD" format.
#' @param end_train The end date for the training dataset, in "YYYY-MM-DD" format.
#' @param start_pred The start date for the forecasting period, in "YYYY-MM-DD" format.
#' @param end_pred The end date for the forecasting period, in "YYYY-MM-DD" format.
#' @param model The prediction model to be applied. Accepted models include "ARGO", "SAR", "GT", or "SAR_GT", with "ARGO" being the default.
#' @param case A data frame containing the case data, which must include columns for "date" and "N_cases".
#' @param GT A data frame of Google Trends data, relevant to the case study.
#' @param lags An optional numeric vector detailing the lags to be applied for generating lagged data. The default setting varies based on the chosen model.
#' @param pred_unit The prediction unit, either "Month" or "Week", with "Month" set as the default.
#' @param offset_GT A small positive value added to the Google Trends data before logarithmic transformation, set to avoid the log of zero issues. The default value is 0.001.
#'
#' @return Returns a data frame containing both the actual and predicted case counts for the forecast period, facilitating direct comparison and model evaluation.
#' @examples
#' get_Prediction(start_train = "2020-01-01", end_train = "2020-06-30",
#'                start_pred = "2020-07-01", end_pred = "2020-07-31",
#'                model = "ARGO", case = case_data, GT = GT_data)
#' @export

get_Prediction <- function(start_train,
                           end_train,
                           start_pred,
                           end_pred,
                           model = 'ARGO',
                           case,
                           GT ,
                           lags = NULL, # ARGO's setting
                           pred_unit = "Month",
                           offset_GT = 0.001){
  # This function is to fit the model and to do the prediction
  # The outcome would be predictted time series

  if(model != "ARGO" & model != "SAR" & model != "GT" & model != "SAR_GT"){
    stop("Error! Unexpected model")
  }

  # Change the unit to Month
  if(pred_unit == "Month"){ case <- from_Week_to_Month(case) }

  # Default lag setting
  if(is.null(lags)){
    if(model == "SAR" | model == "SAR_GT"){
      lags <- c(1:3,24)
    }else{
      lags <- c(1:12,24)
    }
  }

  # Standardize the variables for "case" data
  colnames(case) <- c("date","N_cases")
  case_y <- case # Create a case data for y

  # Drop data if the model doesn't need it
  if(model == "SAR"){ GT <- NULL }
  if(model == "GT"){ case <- NULL }

  # Get X and y train
  X <- get_X(start_train, end_train,
             case = case, GT = GT,
             lags = lags,
             pred_unit = pred_unit,
             offset_GT)

  yt <- case_y %>% filter(date >= start_train & date <= end_train)
  yt <- log(yt$N_cases + 1)



  # Fit the model
  # Set different lambda
  n_vars <- ncol(X)
  penalty_factors <- c(rep(1,n_vars))
  # Set the penalty. There is no penalty for the first three lag.
  # If needed
  # if(model == 'ARGO'){
  #   penalty_factors <- c(rep(0,3),rep(1,n_vars-3))
  # }else{
  #   penalty_factors <- c(rep(1,n_vars))
  # }


  cv_fit <- cv.glmnet(X, yt, penalty.factor = penalty_factors,
                      grouped = F)

  # Best lambda
  best_lambda <- cv_fit$lambda.min

  # Final model
  final_model <- glmnet(X, yt, lambda = best_lambda,
                        penalty.factor = penalty_factors)

  # Prediction
  new_X <- get_X(start_pred, end_pred,
                 case = case, GT = GT,
                 lags = lags,
                 pred_unit = pred_unit,
                 offset_GT)

  y_pred <- predict(final_model, newx = new_X)

  y_pred_case <- exp(y_pred) - 1 # Re-scale the result

  # Combine the data to check the difference
  y_true <- case_y %>% filter(date >= start_pred & date <= end_pred)

  out <- cbind(y_true$N_cases, y_pred_case)

  return(out)
}
