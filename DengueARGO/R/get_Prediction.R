#' Fit Model and Make Prediction
#'
#' This function fits the specified prediction model to the training data and makes predictions for the given period.
#'
#' @param model The model to fit ("ARGO", "SAR", "GT", or "SAR_GT").
#' @param start_train The start date for the training period.
#' @param end_train The end date for the training period.
#' @param start_pred The start date for the prediction period.
#' @param end_pred The end date for the prediction period.
#' @param case A data frame containing case data.
#' @param GT A data frame containing Google Trends data.
#'
#' @return A data frame with actual and predicted cases for the prediction period.
#' @examples
#' get_Prediction("ARGO", "2020-01-01", "2020-06-30", "2020-07-01", "2020-07-31", case_data, GT_data)
#' @export
get_Prediction <- function(model = 'ARGO',
                            start_train, end_train, start_pred, end_pred,
                            case, GT){
  # This function is to fit the model and to do the prediction
  # The outcome would be predictted time series

  if(model != "ARGO" & model != "SAR" & model != "GT" & model != "SAR_GT"){
    return("Error! Unexpected model")
  }

  # Obtain the X matrix
  X <- get_X(start_train, end_train, case, GT, model)

  # Obtain the y as response
  yt <- case %>% filter(date >= start_train & date <= end_train)
  y <- log(yt$N_cases + 1)

  # Fit the model
  # Set different lambda
  n_vars <- ncol(X)

  # Set the penalty. There is no penalty for the first three lag.
  if(model == 'ARGO'){
    penalty_factors <- c(rep(0,3),rep(1,n_vars-3))
  }else{
    penalty_factors <- c(rep(1,n_vars))
  }


  cv_fit <- cv.glmnet(X, y, penalty.factor = penalty_factors)

  # Best lambda
  best_lambda <- cv_fit$lambda.min

  # Final model
  final_model <- glmnet(X, y, lambda = best_lambda,
                        penalty.factor = penalty_factors)

  # Prediction
  new_X <- get_X(start_pred, end_pred, case, GT, model)

  y_pred <- predict(final_model, newx = new_X)

  y_pred_case <- exp(y_pred) - 1 # Re-scale the result

  # Combine the data to check the difference
  y_true <- case %>% filter(date >= start_pred & date <= end_pred)

  out <- cbind(y_true$N_cases, y_pred_case)

  return(out)
}
