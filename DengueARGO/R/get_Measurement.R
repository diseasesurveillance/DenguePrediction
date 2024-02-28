#' Calculate Prediction Performance Measurements
#'
#' This function calculates various performance measurements for the fitted (predicted) vs. actual data.
#'
#' @param y_fitted The fitted (predicted) values.
#' @param y The actual values.
#' @param IF_log A logical indicating whether the values were transformed using logarithm. Default is FALSE.
#'
#' @return A data frame containing the performance measurements: RMSE, MAE, RMSPE, MAPE, and CORR.
#' @examples
#' get_Measurement(fitted_values, actual_values)
#' @export
get_Measurement <- function(y_fitted, y , IF_log = F){
  # This function is to get the measurement by giving
  # the fitted(predicted) value and the real value
  # Unscale the value first
  if(IF_log == T){
    case <- exp(y) - 1; case_fitted <- exp(y_fitted) - 1
  }else{
    case <- y ; case_fitted <- y_fitted
  }

  n <- length(case)
  M <- as.data.frame(matrix(0, nrow = 1, ncol = 5))
  names(M) <- c("RMSE", "MAE", "RMSPE", "MAPE", "CORR")

  M[1,1] <- sqrt(mean((case_fitted - case) ^ 2))
  M[1,2] <- mean(abs(case_fitted - case))
  M[1,3] <- sqrt(mean(((case_fitted - case) / case) ^ 2))
  M[1,4] <- mean(abs((case_fitted - case) / case))
  M[1,5] <- cor(case_fitted, case)

  return(M)
}
