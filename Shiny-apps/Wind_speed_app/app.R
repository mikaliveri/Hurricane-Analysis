# ------------------------------------------------------------------------------
# Title: Hurricane Tracker
# ------------------------------------------------------------------------------
# Description:
# This Shiny app allows users to track and analyze hurricanes based on their 
# wind speed and location. Users can select a hurricane by name and then choose 
# the specific year if the hurricane has occurred multiple times.
# ------------------------------------------------------------------------------
# Details:
# - Uses the ibtracs dataset to visualize hurricane paths and wind speeds.
# - Filters hurricanes by **name** first, then **year** dynamically.
# - Displays a **Leaflet map** showing the hurricane's track.
# - Plots a **wind speed trend graph** with storm categories.
# ------------------------------------------------------------------------------
# Inputs:
# - **ibtracs dataset (CSV file)**: Contains hurricane details including ID, 
#   year, wind speed, latitude, and longitude.
# - **User-selected filters:**
#   - Hurricane name (dropdown, ordered alphabetically)
#   - Hurricane year (dropdown, updates dynamically)
# ------------------------------------------------------------------------------
# Output:
# - **Interactive map plot** displaying hurricane tracks.
# - **Wind speed trend visualization** for selected hurricanes.
# ------------------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)

# Load the hurricane data
ibtracs <- "../../ibtracs.NA.csv"

col_names <- c("SID", "SEASON", "NUMBER", "BASIN", "SUBBASIN", "NAME", "ISO_TIME", 
               "NATURE", "LAT", "LON", "WMO_WIND", "WMO_PRES", "WMO_AGENCY", 
               "TRACK_TYPE", "DIST2LAND", "LANDFALL")
col_types <- c("character", "integer", "integer", "character", "character", 
               "character", "character", "character", "double", "double", 
               "integer", "integer", "character", "character", "integer", "integer")

dat <- read.csv(
  file = ibtracs,
  colClasses = c(col_types, rep("NULL", 158)),
  stringsAsFactors = FALSE, 
  # skip = 77876,
  na.strings = " "
)
colnames(dat) <- col_names

# Convert date columns
dat$DATE <- as.Date(dat$ISO_TIME)
dat$DATETIME <- as.POSIXct(dat$ISO_TIME, format = "%Y-%m-%d %H:%M:%S")

# Remove hurricanes with missing names and order names alphabetically
dat <- dat |> filter(!is.na(NAME) & NAME != "") |> arrange(NAME)

# Define hurricane wind speed categories
category_thresholds <- c(34, 64, 83, 96, 113, 137)  # Knots
category_labels <- c("Tropical Storm", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5")

# Define UI
ui <- fluidPage(
  titlePanel("Hurricane Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectInput("hurricane", "Select Hurricane:", choices = unique(dat$NAME), selected = unique(dat$NAME)[1]),
      selectInput("year", "Select Year:", choices = NULL)  # Populated dynamically
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("wind_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Update year selection based on chosen hurricane
  observeEvent(input$hurricane, {
    available_years <- dat |> 
      filter(NAME == input$hurricane) |> 
      pull(SEASON) |> 
      unique() |> 
      sort()
    
    updateSelectInput(session, "year", choices = available_years, selected = available_years[1])
  })
  
  # Filter data based on selected hurricane and year
  selected_data <- reactive({
    req(input$hurricane, input$year)  # Ensure selections are not NULL
    dat |> filter(NAME == input$hurricane, SEASON == input$year) |> 
      mutate(DATETIME = as.POSIXct(ISO_TIME, format = "%Y-%m-%d %H:%M:%S"))
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    data <- selected_data()
    leaflet(data) |> 
      addTiles() |> 
      addCircleMarkers(~LON, ~LAT, popup = ~paste("Wind:", WMO_WIND, "knots"), 
                       radius = 3, color = "red")
  })
  
  # Render wind speed plot
  output$wind_plot <- renderPlot({
    data <- selected_data()
    category_lines <- tibble(WMO_WIND = category_thresholds, Category = category_labels)
    
    ggplot(data, aes(x = DATETIME, y = WMO_WIND)) +
      geom_area(fill = "red", alpha = 0.7) + geom_point() +
      geom_hline(data = category_lines, aes(yintercept = WMO_WIND), 
                 linetype = "dashed", color = "black", alpha = 0.7) +
      geom_text(data = category_lines, aes(x = min(data$DATETIME), y = WMO_WIND, 
                                           label = Category), 
                hjust = -0.1, vjust = -0.2, size = 3, fontface = "bold", color = "black") +
      labs(
        title = paste("Windspeed Trends for", input$hurricane, "in", input$year),
        x = "Datetime",
        y = "Wind Speed (knots)"
      ) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
