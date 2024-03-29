#' Generate Boxplot for Model Prediction Errors
#'
#' Creates a boxplot visualizing the distribution of prediction errors (differences or percentages) 
#' for various predictive models compared to actual values. This function allows for a direct 
#' visual comparison of model performance in terms of prediction accuracy. Models can be ordered 
#' by their names or the median of their prediction errors in the plot.
#'
#' @param data A data frame with the first column containing the actual values and subsequent 
#'             columns containing predictions from different models. Column names should represent model names.
#' @param type Specifies whether to calculate absolute differences ("Diff") or percentage differences ("Perc") 
#'             between predicted and actual values. Default is "Diff".
#' @param order_by Determines the ordering of models in the boxplot, either by "Model" names or by the 
#'                 "Median" value of their prediction errors. Default is "Model".
#' @param plot_title Title for the boxplot. Default is "Difference between Predictions and Real Value".
#' @param x_lab Label for the x-axis, typically representing the prediction method. Default is "Prediction Method".
#' @param y_lab Label for the y-axis, representing the type of difference (absolute or percentage). 
#'             Default is "Difference".
#'
#' @return A `ggplot` object representing the boxplot of prediction errors for each model. The boxplot 
#'         can be customized further by the user using `ggplot2` functions.
#'
#' @examples
#' # Assuming `predictions_data` is your data frame with actual values in the first column and
#' # predictions from various models in subsequent columns:
#' plot <- get_Boxplot(predictions_data, type = "Diff", order_by = "Median")
#' plot
#' @export

get_Boxplot <- function(data, 
                        type = "Diff",
                        order_by = "Model",
                        plot_title = "Difference between Predictions and Real Value",
                        x_lab = "Prediction Method",
                        y_lab = "Difference") {
  
  # Determine the real value column name dynamically
  real_value_col <- colnames(data)[1]
  
  # Get the names of all the prediction columns
  # prediction_cols <- colnames(data)[-1] # now we use a customize order
  prediction_cols <- c("SAR_GT", "ARGO", "SAR", "Naive", "GT")
  
  
  
  # Add a prefix to distinguish between absolute and percentage differences
  prefix <- ifelse(type == "Diff", "Diff", "Perc")
  
  # Calculate the differences based on the type
  df_long <- data %>%
    mutate(across(all_of(prediction_cols),
                  list(Diff = ~ if(type == "Diff") {
                    abs(. - .data[[real_value_col]])
                  } else {
                    100 * abs(. - .data[[real_value_col]]) / .data[[real_value_col]]
                  }),
                  .names = "{prefix}_{.col}")) %>%
    pivot_longer(cols = starts_with(prefix), 
                 names_to = "Prediction", 
                 values_to = "Difference") %>%
    mutate(Prediction = sub(paste0("^", prefix, "_"), "", Prediction),
           Prediction = factor(Prediction, levels = prediction_cols)) %>%
    select(Prediction, Difference)
  
  # Order by median or model names
  if (order_by == "Median") {
    median_order <- df_long %>%
      group_by(Prediction) %>%
      summarize(median_val = median(Difference)) %>%
      arrange(median_val) %>%
      pull(Prediction)
    
    df_long$Prediction <- factor(df_long$Prediction, levels = median_order)
  } else {
    df_long$Prediction <- factor(df_long$Prediction, levels = prediction_cols)
  }
  
  # Set relation between color and model
  colors <- c("green", "red", "purple", "orange", "blue")
  model_names <- c("SAR_GT", "ARGO", "SAR", "Naive", "GT")
  
  # Plot the differences using ggplot2, with different colors for each prediction method
  p_out <- ggplot(df_long, aes(x = Prediction, y = Difference, fill = Prediction)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "red", size = 2, shape = 18) +
    labs(title = plot_title,
         x = x_lab,
         y = y_lab) +
    theme_minimal() +
    scale_fill_manual(values = colors, breaks = model_names) # Use a color palette for different fills
  
  return(p_out)
}