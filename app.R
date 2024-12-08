library(shiny)
library(shinydashboard)
library(DT)

# Read the predictions and model
df_predictions <- read.csv("volatility_predictions.csv", stringsAsFactors = FALSE)
lm_model <- readRDS("volatility_prediction_model.rds")

# Read headlines CSV
headlines_df <- read.csv("headlines_with_sentiment.csv", stringsAsFactors = FALSE)

# Generate dummy data for potential future implementation
set.seed(12)
sp500_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "FB", "TSLA", "NVDA", "V", "JPM", "JNJ")

# Function to generate mock headlines and sentiment scores
generate_mock_headlines <- function(ticker, n_headlines = 20) {
  sentiment_words_positive <- c("growth", "innovation", "success", "breakthrough", "excellent")
  sentiment_words_negative <- c("challenges", "decline", "setback", "struggle", "uncertainty")
  
  mock_data <- t(sapply(1:n_headlines, function(x) {
    # Randomly generate headline
    is_positive <- sample(c(TRUE, FALSE), 1)
    sentiment_words <- if(is_positive) sentiment_words_positive else sentiment_words_negative
    
    headline <- paste(
      sample(c("Company", ticker, "Stock"), 1),
      sample(c("sees", "reports", "announces"), 1),
      sample(sentiment_words, 1),
      "in recent",
      sample(c("quarter", "month", "week"), 1)
    )
    
    # Generate sentiment score (-1 to 1)
    sentiment_score <- if(is_positive) runif(1, 0, 1) else runif(1, -1, 0)
    
    c(headline = headline, sentiment_score = sentiment_score)
  }))
  
  # Transpose and convert to dataframe
  mock_df <- as.data.frame(mock_data)
  
  # Add ticker column and convert sentiment score to numeric
  mock_df$ticker <- ticker
  mock_df$sentiment_score <- as.numeric(as.character(mock_df$sentiment_score))
  
  return(mock_df)
}

# Generate mock headlines for all SP500 tickers
mock_headlines_df <- do.call(rbind, lapply(sp500_tickers, generate_mock_headlines))

ui <- dashboardPage(
  dashboardHeader(title = "Sentiment Volatility Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-area")),
      menuItem("Regression Details", tabName = "regression", icon = icon("chart-line")),
      menuItem("Actual Headlines", tabName = "actual_headlines", icon = icon("newspaper")),
      menuItem("Potential Future Implementation", tabName = "future_implementation", icon = icon("lightbulb"))
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
                <li>Variables are standardized for easier comparison</li>
              </ul>
            ")
                )
              ),
              fluidRow(
                box(
                  title = "Volatility vs Previous Day Variance (Standardized)", 
                  plotOutput("scatter_plot"),
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
                  plotOutput("residuals_plot"),
                  width = 12
                )
              )
      ),
      
      # Actual Headlines Tab
      tabItem(tabName = "actual_headlines",
              fluidRow(
                box(
                  title = "Actual Headlines", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput("actual_headlines_table")
                ),
                box(
                  title = "Sentiment Scores", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("sentiment_distribution")
                )
              )
      ),
      
      # Potential Future Implementation Tab
      tabItem(tabName = "future_implementation",
              fluidRow(
                box(
                  title = "Potential Implementation Design", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  HTML("
              <h3>Proposed Feature: Dynamic Sentiment Analysis</h3>
              <p>This section demonstrates a potential future implementation of real-time sentiment analysis.</p>
              <h4>Key Features:</h4>
              <ul>
                <li>Dynamic stock selection from S&P 500</li>
                <li>Real-time news headline fetching</li>
                <li>Automated sentiment scoring</li>
                <li>Volatility prediction based on sentiment</li>
              </ul>
            ")
                )
              ),
              fluidRow(
                box(
                  title = "Stock Selection", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("ticker_selection", 
                              "Select Stocks for Portfolio Analysis", 
                              choices = sp500_tickers, 
                              multiple = TRUE, 
                              selected = c("AAPL", "MSFT"))
                ),
                box(
                  title = "Mock Headlines", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput("mock_headlines_table")
                )
              ),
              fluidRow(
                box(
                  title = "Mock Sentiment Analysis", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("mock_sentiment_distribution")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Scatter plot of standardized volatility vs previous day variance
  output$scatter_plot <- renderPlot({
    plot(df_predictions$prev_scaled_var, df_predictions$scaled_vol, 
         main = "Standardized Volatility vs Previous Day Variance",
         xlab = "Standardized Previous Day Variance", 
         ylab = "Standardized Volatility",
         pch = 16, col = rgb(0,0,0,0.5))
    abline(lm(scaled_vol ~ prev_scaled_var, data = df_predictions), col = "red")
  })
  
  # Model Summary
  output$model_summary <- renderPrint({
    summary(lm_model)
  })
  
  # Residuals Plot
  output$residuals_plot <- renderPlot({
    plot(df_predictions$predicted_scaled_vol, df_predictions$residuals,
         main = "Residuals vs Predicted Standardized Volatility",
         xlab = "Predicted Standardized Volatility", 
         ylab = "Residuals",
         pch = 16, col = rgb(0,0,0,0.5))
    abline(h = 0, col = "red", lty = 2)
  })
  
  # Actual Headlines Table
  output$actual_headlines_table <- renderDataTable({
    datatable(headlines_df, 
              options = list(pageLength = 10, 
                             scrollX = TRUE))
  })
  
  # Sentiment Score Distribution
  output$sentiment_distribution <- renderPlot({
    # Gather all sentiment columns dynamically
    sent_cols <- grep("_sent_score$", names(headlines_df), value = TRUE)
    
    # Gather sentiment scores manually
    sentiment_scores <- unlist(headlines_df[, sent_cols])
    
    hist(sentiment_scores, 
         breaks = 30, 
         main = "Distribution of Sentiment Scores",
         xlab = "Sentiment Score", 
         col = "blue", 
         border = "white")
  })
  
  # Mock Headlines Table (Filtered by selected tickers)
  output$mock_headlines_table <- renderDataTable({
    req(input$ticker_selection)
    mock_filtered <- mock_headlines_df[mock_headlines_df$ticker %in% input$ticker_selection, 
                                       c("ticker", "headline", "sentiment_score")]
    
    datatable(mock_filtered, 
              options = list(pageLength = 5, 
                             scrollX = TRUE))
  })
  
  # Mock Sentiment Distribution
  output$mock_sentiment_distribution <- renderPlot({
    req(input$ticker_selection)
    
    mock_filtered <- mock_headlines_df[mock_headlines_df$ticker %in% input$ticker_selection, ]
    
    # Use base R histogram with multiple series
    tickers <- unique(mock_filtered$ticker)
    colors <- palette()[1:length(tickers)]
    
    hist(mock_filtered$sentiment_score, 
         breaks = 20, 
         main = "Mock Sentiment Score Distribution by Selected Stocks",
         xlab = "Sentiment Score", 
         col = colors, 
         border = "white")
    legend("topright", legend = tickers, fill = colors)
  })
}

shinyApp(ui, server)