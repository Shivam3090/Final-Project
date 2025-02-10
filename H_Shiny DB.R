library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(plotly)
library(rsconnect)
library(tidyr)

# Load the dataset
data("txhousing")

# Handle missing values by replacing them with column medians
txhousing_imputed <- txhousing %>%
  mutate(across(c(sales, volume, median, listings, inventory), ~ replace_na(., median(., na.rm = TRUE)))) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# Define UI for the Shiny App
ui <- dashboardPage(
  dashboardHeader(title = "Texas Housing Market Trends (2000-2015)"),
  dashboardSidebar(
    selectInput("city", "Select City:", choices = unique(txhousing_imputed$city), selected = "Houston"),
    sliderInput("year", "Select Year:", min = min(txhousing_imputed$year), max = max(txhousing_imputed$year), 
                value = c(2005, 2015), sep = ""),
    selectInput("month", "Select Month:", choices = month.name, selected = "January")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("median_price", width = 4),
      valueBoxOutput("total_sales", width = 4),
      valueBoxOutput("avg_inventory", width = 4)
    ),
    fluidRow(
      box(title = "Price Trends Over Time", status = "primary", solidHeader = TRUE, width = 6,
          plotlyOutput("price_trend_plot")),
      box(title = "Sales vs Inventory", status = "warning", solidHeader = TRUE, width = 6,
          plotlyOutput("sales_inventory_plot"))
    ),
    fluidRow(
      box(title = "Sales vs Median Price", status = "success", solidHeader = TRUE, width = 6,
          plotlyOutput("sales_median_price_plot")),
      box(title = "Median Price by City", status = "info", solidHeader = TRUE, width = 6,
          plotlyOutput("median_price_by_city_plot"))
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  filtered_data <- reactive({
    txhousing_imputed %>%
      filter(city == input$city, year >= input$year[1], year <= input$year[2]) %>%
      mutate(month_name = format(date, "%B")) %>%
      filter(month_name == input$month)
  })
  
  output$median_price <- renderValueBox({
    valueBox(
      formatC(median(filtered_data()$median, na.rm = TRUE), format = "f", big.mark = ",", digits = 0),
      "Median Price", icon = icon("dollar-sign"), color = "blue"
    )
  })
  
  output$total_sales <- renderValueBox({
    valueBox(
      formatC(sum(filtered_data()$sales, na.rm = TRUE), format = "f", big.mark = ",", digits = 0),
      "Total Sales", icon = icon("chart-line"), color = "green"
    )
  })
  
  output$avg_inventory <- renderValueBox({
    valueBox(
      formatC(mean(filtered_data()$inventory, na.rm = TRUE), format = "f", big.mark = ",", digits = 0),
      "Avg Inventory", icon = icon("warehouse"), color = "purple"
    )
  })
  
  output$price_trend_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = date, y = median)) +
      geom_line(color = "blue") +
      labs(title = "Median Price Over Time", x = "Year", y = "Median Price")
    ggplotly(p)
  })
  
  output$sales_inventory_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = date)) +
      geom_line(aes(y = sales, color = "Sales")) +
      geom_line(aes(y = listings, color = "Listings")) +
      labs(title = "Sales vs Inventory", x = "Year", y = "Count") +
      scale_color_manual(values = c("Sales" = "red", "Listings" = "green"))
    ggplotly(p)
  })
  
  output$sales_median_price_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = sales, y = median)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Sales vs Median Price", x = "Sales", y = "Median Price")
    ggplotly(p)
  })
  
  output$median_price_by_city_plot <- renderPlotly({
    city_medians <- txhousing_imputed %>%
      group_by(city) %>%
      summarize(median_price = median(median, na.rm = TRUE))
    
    p <- ggplot(city_medians, aes(x = reorder(city, -median_price), y = median_price, fill = city)) +
      geom_bar(stat = "identity") +
      labs(title = "Median Price by City", x = "City", y = "Median Price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

# Run the Shiny App
shinyApp(ui, server)
setwd("C:\Users\shiva\OneDrive\Desktop\CPS-Analytics\ALY-6070\Finalproject")

rsconnect::setAccountInfo(name='shivam0312', token='FF2ECCEE8ADAB3C23E95A896F9787452', secret='fGE/ZpAH4d+2ftdHXJL3642E3N8Sq2ISaCJjzm8M')
library(rsconnect)
rsconnect::deployApp()


