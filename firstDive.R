library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(rlang)


mental_health_data <- read_csv('E:/rer_textbook/mental_health_data.csv')


names(mental_health_data) <- tolower(gsub("[^[:alnum:]_]", "_", names(mental_health_data)))
mental_health_data <- mental_health_data %>% mutate(year = as.numeric(year))


ui <- fluidPage(
  titlePanel("Global Mental Health Disorders Analysis"),
  
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
        tabPanel("Trend", plotOutput("trendPlot")),
        tabPanel("Scatter", plotOutput("scatterPlot")),
        tabPanel("Histogram", plotOutput("histPlot")),
        tabPanel("Bar Plot", plotOutput("barPlot")),
        tabPanel("Prediction", verbatimTextOutput("prediction")),
        tabPanel("Summary", verbatimTextOutput("summaryStats"))
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
    avg_disorder_value <- mental_health_data %>%
      group_by(entity) %>%
      summarise(avg_value = mean(.data[[input$disorder]], na.rm = TRUE)) %>%
      arrange(desc(avg_value))
    
    ggplot(avg_disorder_value, aes(x = reorder(entity, avg_value), y = avg_value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Average", input$disorder, "Across Countries"),
           x = "Country", y = "Average Share of Population (%)") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10), plot.title = element_text(hjust = 0.5))
  })
  
  # Render summary statistics
  output$summaryStats <- renderPrint({
    data <- filtered_data()
    disorder_col <- input$disorder  # Clean up the column name directly
    
    validate(need(nrow(data) > 0, "No data available for the selected country."))
    
    summary(data[[disorder_col]])
  })
  
  # Predict future values (5 years ahead)
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
