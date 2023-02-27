library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(sf)
library(countrycode)
library(RColorBrewer)
library(treemapify)

# Read the world map data 
world_map_data <- sf::st_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")

# Read the raw Olympics data 
#dataset <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv")
dataset <- read.csv("olympics_data.csv")

# Data wrangling to convert the country code from ioc format to iso such that it matches with the map data 
df_map <- as_tibble(world_map_data) |>
  select(id, name) |>
  rename(code = id, country_name = name)

filtered_data <- dataset |>
  drop_na(medal) |>
  mutate(code = countrycode(noc, origin = "ioc", destination = "iso3c")) |>
  inner_join(df_map, by = 'code') |>
  mutate(team = ifelse(!is.na(country_name), country_name, team)) |>
  select(-c(code, country_name)) |> 
  dplyr::distinct()


ui <- fluidPage(
  tabsetPanel(
  tabPanel("Page 1",
  titlePanel(h1(id = "title","Country Level Overview of Medals", align = "center"),
            tags$head( tags$style(HTML("#title{color: yellow;
                                              font-size: 20px;
                                              font-style: bold;
                                              background-color:black
                                       }")))),
  fluidRow(
   column(3,
           sliderInput("year_range", "Select Year Range:",
               min = 1896, max = 2016, value = c(1896, 2016), sep = ""),
    selectInput("team", "Country of Interest:", selected = 'Canada',
                choices = sort(unique(na.omit(c(filtered_data$team, df_map$country_name))))),

    checkboxGroupInput("season", "Winter/Summer", choices = unique(filtered_data$season),
                      selected = unique(filtered_data$season)),
    selectInput("sport", "Sport of Interest:", choices = c("All Sports", sort(unique(filtered_data$sport)))
             )
  ),
   column(9,leafletOutput("world_map"))),
  
  fluidRow(column (12,verbatimTextOutput("country_stats"))),
  fluidRow(
      column(6,plotOutput("bar_plot")),
      column(6,plotOutput("line_plot")))
  
  
),
tabPanel("Page 2",
         titlePanel(h1(id = "title2","Medal Tally Breakdown", align = "center"),
                    tags$head( tags$style(HTML("#title2{color: yellow;
                                              font-size: 20px;
                                              font-style: bold;
                                              background-color:black
                                       }")))),
         fluidRow(
           column(3,
                  sliderInput("year_range_p2", "Select Year Range:",
                              min = 1896, max = 2016, value = c(1896, 2016), sep = ""),
                  selectInput("team_p2", "Country of Interest:", selected = 'Canada',
                              choices = sort(unique(na.omit(c(filtered_data$team, df_map$country_name))))),
                  
                  checkboxGroupInput("season_p2", "Winter/Summer", choices = unique(filtered_data$season),
                                     selected = unique(filtered_data$season)),
                  checkboxGroupInput("medal", "Medal Type", choices = unique(filtered_data$medal),
                                     selected = unique(filtered_data$medal))
                  
           ),
           
           column(9,dataTableOutput("medalTable"))),
         
         fluidRow(
           column(12,plotOutput("treemap")))
         
         
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
  
  subset_data_p2 <- reactive({
    subset(filtered_data, 
           year >= input$year_range_p2[1] & year <= input$year_range_p2[2] & 
             team == input$team_p2 & season == input$season_p2 & 
             medal == input$medal)
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
      paste("Showing ", input$team,
            "'s top years based on medal tally. Please select a sport to view medal tally in that sport")
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
  
  output$medalTable <- renderDataTable({

    medal_table <- subset_data_p2() |> 
      rename(Country = team,
             #Year = year,
             #Season = season,
             Sport = sport) |> 
      group_by(Country,Sport) |> 
      summarize("Total Medals" = n()) |> 
      arrange(desc(`Total Medals`))


    medal_table
  })
  
  output$treemap <- renderPlot( 
    { subset_data_p2() |> 
        group_by(team,sport) |> 
        summarize("Total Medals" = n()) |> 
        ggplot(aes(area = `Total Medals`, fill = `Total Medals`, label = sport,
                   subgroup = `Total Medals`)) +
        geom_treemap() + 
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 5,
                          grow = FALSE) +
        geom_treemap_subgroup_text(place = "centre", grow = FALSE,
                                   colour = "black",
                                   fontface = "italic") +
        theme(legend.position = 'none') +
        ggtitle("Tree Map showing Medal Tally by Sport")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)