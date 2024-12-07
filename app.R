library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)

# Read the predictions and model
df_predictions <- read_csv("volatility_predictions.csv")
lm_model <- readRDS("volatility_prediction_model.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Sentiment Volatility Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Regression Details", tabName = "regression", icon = icon("chart-line")),
      menuItem("Data Exploration", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Project Explanation", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  HTML("
              <h3>Sentiment-Based Volatility Prediction</h3>
              <p>This dashboard explores the relationship between sentiment variance 
              of S&P 100 stocks and index volatility.</p>
              
              <h4>Methodology:</h4>
              <ul>
                <li>Collected news headlines for S&P 100 stocks over 30 days</li>
                <li>Used ChatGPT to generate sentiment scores for each headline</li>
                <li>Calculated daily sentiment score standard deviation and variance</li>
                <li>Investigated correlation with index volatility</li>
              </ul>
            ")
                )
              ),
              fluidRow(
                box(
                  title = "Volatility vs Previous Day Variance", 
                  plotlyOutput("scatter_plot"),
                  width = 12
                )
              )
      ),
      
      # Regression Details Tab
      tabItem(tabName = "regression",
              fluidRow(
                box(
                  title = "Regression Model Summary", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("model_summary")
                )
              ),
              fluidRow(
                box(
                  title = "Residuals Distribution", 
                  plotlyOutput("residuals_plot"),
                  width = 12
                )
              )
      ),
      
      # Data Exploration Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Data Table", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  dataTableOutput("data_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Scatter plot of volatility vs previous day variance
  output$scatter_plot <- renderPlotly({
    ggplot(df_predictions, aes(x = prev_day_var, y = vol)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "red") +
      labs(
        title = "Volatility vs Previous Day Variance",
        x = "Previous Day Variance", 
        y = "Volatility"
      ) +
      theme_minimal() %>%
      ggplotly()
  })
  
  # Model Summary
  output$model_summary <- renderPrint({
    summary(lm_model)
  })
  
  # Residuals Plot
  output$residuals_plot <- renderPlotly({
    ggplot(df_predictions, aes(x = predicted_vol, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = "Residuals vs Predicted Volatility",
        x = "Predicted Volatility", 
        y = "Residuals"
      ) +
      theme_minimal() %>%
      ggplotly()
  })
  
  # Data Table
  output$data_table <- renderDataTable({
    df_predictions %>%
      select(Date, prev_day_var, vol, predicted_vol, residuals)
  })
}

shinyApp(ui, server)