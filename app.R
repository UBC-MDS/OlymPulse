library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(sf)
library(countrycode)
library(RColorBrewer)

# Read the world map data 
world_map_data <- sf::st_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")

# Read the raw Olympics data 
dataset <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv")

# Data wrangling to convert the country code from ioc format to iso such that it matches with the map data 
df_map <- as_tibble(world_map_data) |>
  select(id, name) |>
  rename(code = id, country_name = name)

filtered_data <- dataset |>
  drop_na(medal) |>
  mutate(code = countrycode(noc, origin = "ioc", destination = "iso3c")) |>
  left_join(df_map, by = "code") |>
  mutate(team = ifelse(!is.na(country_name), country_name, team)) |>
  select(-c(code, country_name))


# Define the UI
ui <- fluidPage(
  titlePanel("Your Choice of Interest"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:",
                  min = 1896, max = 2016, value = c(1896, 2016), sep = ""),
      selectInput("team", "Country of Interest:", choices = sort(unique(na.omit(c(filtered_data$team, df_map$country_name))))),
      selectInput("season", "Season of Interest:", choices = unique(filtered_data$season)),
      selectInput("sport", "Sport of Interest:", choices = c("All Sports", sort(unique(filtered_data$sport))))
    ),
    mainPanel(
      leafletOutput("world_map"),
      verbatimTextOutput("country_stats"),
      fluidRow(splitLayout(cellWidths =c("50%", "50%"), plotOutput("bar_plot"), plotOutput("line_plot"))
    )
  )
))


# Define server
server <- function(input, output, session) {
  
  # Create reactive data for selected range of years, country and season of interest
  subset_data <- reactive({
    subset(filtered_data, 
           year >= input$year_range[1] & year <= input$year_range[2] & 
             team == input$team & season == input$season & 
             (input$sport == "All Sports" | sport == input$sport))
  })
  
  # Create the interactive map of the world 
  output$world_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(data = world_map_data, 
                  group = "World Map", 
                  layerId = world_map_data$name, 
                  label = world_map_data$name,
                  color = "#FFFFFF", 
                  weight = 1, 
                  fillOpacity = 0.8, 
                  highlight = highlightOptions(weight = 1.5, 
                                               color = "lightblue", 
                                               fillColor = "lightblue",
                                               fillOpacity = 0.3,
                                               bringToFront = TRUE)) |>
      setView(-10, -30, zoom = 1)
  })
  
  # Display country stats
  output$country_stats <- renderPrint({
    if (input$team == "All Sports") {
      "Showing top 5 countries with most medals. Please select a country to view its individual statistics."
    } else {
      country_data <- subset_data() |> filter(team == input$team)
      if (nrow(country_data) == 0) {
        "No medals were achieved in the selected range of years."
      } else {
        paste0("Total number of medals: ", nrow(country_data))
      }
    }
  })
  
  # Update selected country in the dropdown menu when clicked on the map
  observeEvent(input$world_map_shape_click, {
    selected_country <- input$world_map_shape_click$id
    updateSelectInput(session, "team", selected = selected_country)
  })
  
  output$bar_plot <- renderPlot({
    if (input$sport == "All Sports") {
      # Top 5 years when most total number of medals are achieved in all sports
      top_five_years <- subset_data() |>
        group_by(year) |>
        summarize(total_medals = n()) |>
        arrange(desc(total_medals)) |>
        slice(1:5)
    } else {
      # Top 5 years where most total number of medals are achieved in the sport selected
      top_five_years <- subset_data() |>
        filter(sport == input$sport) |>
        group_by(year) |>
        summarize(total_medals = n()) |>
        arrange(desc(total_medals)) |>
        slice(1:5)
    }
      ggplot(data = top_five_years) +
        aes(x = total_medals, y = reorder(year, -total_medals), fill = as.factor(year)) +
        geom_bar(stat = "identity") +
        labs(x = "Total Number of Medals", y = "Year") +
        ggtitle(paste0("Top 5 Years for Most Medals in ", input$sport)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
        scale_fill_brewer(palette = "Set2")
  })
  
  output$line_plot <- renderPlot({
    if (input$sport == "All Sports") {
      # Trend in total number of medals over the selected range of years in all sports
      trend <- subset_data() |>
        group_by(year) |>
        summarize(total_medals = n()) 
    } else {
      # Trend in total number of medals over the selected range of years in the selected sport 
      trend <- subset_data() |>
        filter(sport == input$sport) |>
        group_by(year) |>
        summarize(total_medals = n()) 
    }
    ggplot(data = trend) +
      aes(y = total_medals, x = year) +
      geom_line(color = "lightblue", size = 2) +
      labs(y = "Total Number of Medals", x = "Year") +
      ggtitle(paste0("Trend in Total Number of Medals in ", input$sport)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)