library(shiny)
library(ggplot2)
library(tidyverse)

dataset <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv")
filtered_data <- dataset |>
    drop_na(medal) 

# Define UI
ui <- fluidPage(
  titlePanel("Your Choice of Interest"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:",
                  min = 1896, max = 2016, value = c(1896, 2016), sep = ""),
      selectInput("team", "Country of Interest:", choices = sort(unique(filtered_data$team))),
      selectInput("season", "Season of Interest:", choices = unique(filtered_data$season))
    ),
    mainPanel(
      plotOutput("line_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Create reactive data for selected range of years, country and season of interest
  subset_data <- reactive({
    subset(filtered_data, 
           year >= input$year_range[1] & year <= input$year_range[2] & 
             team == input$team & season == input$season)
  })
  
  # Calculate total number of medals for the country and season of interest
  total_medals <- reactive({
    aggregate(medal ~ year, data = subset_data(), FUN = length)
  })
  
  # Line plot for total number of medals over the selected range of years
  output$line_plot <- renderPlot({
    ggplot(data = total_medals(), aes(x = year, y = medal)) +
      geom_line() +
      labs(x = "Year", y = "Total Number of Medals") +
      scale_x_continuous(limits = c(input$year_range[1], input$year_range[2]))
  })
}

# Run the application
shinyApp(ui = ui, server = server)


