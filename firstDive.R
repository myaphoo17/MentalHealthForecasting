library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(rlang)
library(countrycode)

mental_health_data <- read_csv('E:/R_FINAL_ANALYSIS/mental_health_data.csv')

# Clean column names to avoid issues with special characters
names(mental_health_data) <- tolower(gsub("[^[:alnum:]_]", "_", names(mental_health_data)))
mental_health_data <- mental_health_data %>% mutate(year = as.numeric(year))

# Ensure 'entity' contains valid country names
mental_health_data$entity <- as.factor(mental_health_data$entity)

ui <- fluidPage(
  div(
    HTML("<h4 style='text-align: center;'>Global Mental Health Disorders Analysis</h4>")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(mental_health_data$entity)),
      selectInput("disorder", "Select Disorder:", 
                  choices = c("Schizophrenia" = "schizophrenia_disorders__share_of_population____sex__both___age__age_standardized",
                              "Depression" = "depressive_disorders__share_of_population____sex__both___age__age_standardized",
                              "Anxiety" = "anxiety_disorders__share_of_population____sex__both___age__age_standardized",
                              "Bipolar" = "bipolar_disorders__share_of_population____sex__both___age__age_standardized",
                              "Eating Disorders" = "eating_disorders__share_of_population____sex__both___age__age_standardized")),
      actionButton("predictBtn", "Predict Future Values", class = "btn-primary"),
      downloadButton("downloadPlot", "Download Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = div(icon("chart-line"), "Trend Analysis"),
          tags$div(
            "Visualize trends over time for the selected mental health disorder.",
            style = "font-size: 0.9rem; color: gray; margin-bottom: 10px;"
          ),
          plotOutput("trendPlot")
        ),
        tabPanel(
          title = div(icon("braille"), "Scatter Plot"),
          tags$div(
            "Explore the relationship between years and disorder prevalence using scatter plots.",
            style = "font-size: 0.9rem; color: gray; margin-bottom: 10px;"
          ),
          plotOutput("scatterPlot")
        ),
        tabPanel(
          title = div(icon("chart-bar"), "Histogram"),
          tags$div(
            "Understand the distribution of disorder prevalence for the selected country.",
            style = "font-size: 0.9rem; color: gray; margin-bottom: 10px;"
          ),
          plotOutput("histPlot")
        ),
        tabPanel(
          title = div(icon("chart-pie"), "Bar Plot"),
          tags$div(
            "Compare average disorder prevalence across continents.",
            style = "font-size: 0.9rem; color: gray; margin-bottom: 10px;"
          ),
          plotOutput("barPlot")
        ),
        tabPanel(
          title = div(icon("chart-line"), "Predictions"),
          tags$div(
            "Forecast future trends based on historical data.",
            style = "font-size: 0.9rem; color: gray; margin-bottom: 10px;"
          ),
          verbatimTextOutput("prediction")
        ),
        tabPanel(
          title = div(icon("table"), "Summary Statistics"),
          tags$div(
            "Get descriptive statistics for the selected mental health disorder.",
            style = "font-size: 0.9rem; color: gray; margin-bottom: 10px;"
          ),
          verbatimTextOutput("summaryStats")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to filter data
  filtered_data <- reactive({
    mental_health_data %>%
      filter(entity == input$country) %>%
      as.data.frame()  
  })
  
  # Render trend plot
  output$trendPlot <- renderPlot({
    data <- filtered_data()
    disorder_col <- input$disorder  
    
    validate(need(nrow(data) > 0, "No data available for the selected country."))
    
    ggplot(data, aes_string(x = "year", y = disorder_col)) +  
      geom_line(color = "#0073C2FF", size = 1.2) +
      geom_point(color = "#0073C2FF") +
      labs(title = paste("Trend of", input$disorder, "in", input$country),
           x = "Year", y = "Share of Population (%)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # Render scatter plot
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    disorder_col <- input$disorder  
    
    validate(need(nrow(data) > 0, "No data available for the selected country."))
    
    ggplot(data, aes_string(x = "year", y = disorder_col)) +
      geom_point(color = "blue") +
      labs(title = paste("Scatter Plot of", input$disorder, "in", input$country),
           x = "Year", y = "Share of Population (%)") +
      theme_minimal()
  })
  
  # Render histogram
  output$histPlot <- renderPlot({
    data <- filtered_data()
    disorder_col <- input$disorder  
    
    validate(need(nrow(data) > 0, "No data available for the selected country."))
    
    ggplot(data, aes_string(x = disorder_col)) +
      geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
      labs(title = paste("Distribution of", input$disorder, "in", input$country),
           x = "Share of Population (%)", y = "Frequency") +
      theme_minimal()
  })
  
  # Render bar plot
  output$barPlot <- renderPlot({
    # Add continent information to the dataset if not already present
    mental_health_data <- mental_health_data %>%
      mutate(continent = countrycode(entity, "country.name", "continent"))
    
    # Calculate average disorder value by continent
    avg_disorder_value <- mental_health_data %>%
      group_by(continent) %>%
      summarise(avg_value = mean(.data[[input$disorder]], na.rm = TRUE)) %>%
      arrange(desc(avg_value))
    
    # Plot the averages by continent
    ggplot(avg_disorder_value, aes(x = reorder(continent, avg_value), y = avg_value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Average", input$disorder, "Across Continents"),
           x = "Continent", y = "Average Share of Population (%)") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10), plot.title = element_text(hjust = 0.5))
  })
  
  # Render summary statistics
  output$summaryStats <- renderPrint({
    data <- filtered_data()
    disorder_col <- input$disorder
    
    validate(need(nrow(data) > 0, "No data available for the selected country."))
    
    selected_data <- data[[disorder_col]]
    
    # Create a list of detailed statistics
    stats <- list(
      Summary = summary(selected_data),
      Mean = mean(selected_data, na.rm = TRUE),
      Median = median(selected_data, na.rm = TRUE),
      Standard_Deviation = sd(selected_data, na.rm = TRUE),
      Variance = var(selected_data, na.rm = TRUE),
      Range = range(selected_data, na.rm = TRUE)
    )
    
    # Print the statistics
    stats
  })
  
  # Predict future values
  output$prediction <- renderPrint({
    input$predictBtn
    
    isolate({
      data <- filtered_data()
      validate(need(nrow(data) > 0, "No data available for the selected country."))
      
      model <- lm(as.formula(paste(input$disorder, "~ year")), data = data)
      
      future_years <- data.frame(year = seq(max(data$year) + 1, max(data$year) + 10))
      predictions <- predict(model, newdata = future_years)
      
      prediction_df <- data.frame(Year = future_years$year, Predicted_Value = round(predictions, 4))
      prediction_df
    })
  })
  
  # Download handler for plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$disorder, "-", input$country, ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf", width = 10, height = 6)
    }
  )
}

shinyApp(ui = ui, server = server)
