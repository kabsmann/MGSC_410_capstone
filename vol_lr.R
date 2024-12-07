library(tidyverse)
library(lubridate)
library(zoo)
library(scales)

# Read the CSV file
df <- read_csv("all_metrics.csv") %>%
  # Convert Date to Date type
  mutate(Date = as.Date(Date)) %>%
  # Sort by date to ensure correct order for NA handling
  arrange(Date) %>%
  # Fill in missing variance with the last known value
  mutate(var = na.locf(var, na.rm = FALSE),
         st_dev = na.locf(st_dev, na.rm = FALSE))

# Create a function for standardization
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Standardize the variables
df_scaled <- df %>%
  mutate(
    scaled_var = standardize(var),
    scaled_st_dev = standardize(st_dev),
    scaled_vol = standardize(vol)
  ) %>%
  # Create a lagged scaled variance column
  mutate(prev_scaled_var = lag(scaled_var))

# Remove rows with NA in prev_scaled_var or scaled_vol
df_clean <- df_scaled %>%
  drop_na(prev_scaled_var, scaled_vol)

# Perform linear regression on scaled variables
lm_model <- lm(scaled_vol ~ prev_scaled_var, data = df_clean)

# Summary of the regression
summary_lm <- summary(lm_model)

# Print the results
print(summary_lm)

# Create a dataframe with predictions
df_predictions <- df_scaled %>%
  mutate(
    predicted_scaled_vol = predict(lm_model, newdata = .),
    residuals = scaled_vol - predicted_scaled_vol
  )

# Save the model and predictions
saveRDS(lm_model, "volatility_prediction_model.rds")
write_csv(df_predictions, "volatility_predictions.csv")

# Also save original scaling parameters for potential inverse transformation
scaling_params <- list(
  var_mean = mean(df$var, na.rm = TRUE),
  var_sd = sd(df$var, na.rm = TRUE),
  vol_mean = mean(df$vol, na.rm = TRUE),
  vol_sd = sd(df$vol, na.rm = TRUE)
)
saveRDS(scaling_params, "scaling_parameters.rds")