# MGSC_410_capstone
Kabir Mann
Taher Rastkar

Volatility Prediction Shiny Dashboard
Project Overview

This project is focused on analyzing market volatility using sentiment analysis and other relevant data. Below are the instructions on how to run the code and use the provided files.

Prerequisites

Make sure you have the following installed:

R (latest version)

Required R libraries:

shiny

tidyverse

readr

caret

data.table

e1071

Any other packages referenced in the code files

Files Included

The following files are included in this repository:

.RData - Contains saved R workspace data.

.Rhistory - Records the command history of R sessions.

README.md - This file.

all_metrics.csv - Stores evaluation metrics for the model.

app.R - Shiny app for visualizing volatility predictions.

headlines_with_sentiment.csv - Preprocessed dataset with sentiment scores for headlines.

scaling_parameters.rds - Saved scaling parameters for data normalization.

tick_data.csv - Raw tick data used in model training.

vol_lr.R - Code for implementing linear regression to model volatility.

vol_shiny.R - Code for setting up the Shiny visualization app.

volatility_prediction_model.rds - Trained machine learning model for predicting volatility.

volatility_predictions.csv - Predicted volatility values for visualization.

Instructions for Running the Code

Step 1: Verify Data Files

Ensure that the following files are already in place and do not need to be re-generated:

headlines_with_sentiment.csv (already includes sentiment analysis for headlines)

tick_data.csv

Step 2: Run Linear Regression Model

Open vol_lr.R in RStudio or any R environment.

Execute the script to:

Load the headlines_with_sentiment.csv and tick_data.csv files.

Train a linear regression model on the data.

Save results to volatility_prediction_model.rds and all_metrics.csv.

Step 3: Shiny Application for Visualization

Open app.R in RStudio.

Run the Shiny app by clicking the "Run App" button.

The app will use the following files for visualization:

volatility_predictions.csv

all_metrics.csv

Notes

The code assumes that the headlines_with_sentiment.csv file already exists and includes processed sentiment data. You do not need to rerun sentiment analysis or headline pulling.

Any changes to the headline data require re-running scripts that preprocess the headlines and save headlines_with_sentiment.csv.
