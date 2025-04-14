# ------------------------------------------------------------------------------
# Title: Wind_Speed_Version_2
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
library(plotly)

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

wind_palette <- colorNumeric(
  palette = "Reds",     # Uses RColorBrewer's Reds palette (light to dark red)
  domain = dat$WMO_WIND # Full dataset's wind speed range
)

# Define UI
ui <- fluidPage(
  titlePanel("Hurricane Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectInput("hurricane", "Select Hurricane:", choices = unique(dat$NAME), selected = unique(dat$NAME)[1]),
      selectInput("year", "Select Year:", choices = NULL),
      checkboxInput("show_points", "Show Points on Wind Speed Plot", TRUE),
      selectInput("show_trendline_category", "Show Threshold Line for:", 
                  choices = c("All", "Tropical Storm", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "None"), 
                  selected = "All")
      
    )
    ,
    mainPanel(
      leafletOutput("map"), hr(),
      plotlyOutput("wind_plot")
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
    req(input$hurricane, input$year)
    
    dat |> 
      filter(NAME == input$hurricane, SEASON == input$year) |> 
      mutate(
        DATETIME = as.POSIXct(ISO_TIME, format = "%Y-%m-%d %H:%M:%S"),
        Category = case_when(
          WMO_WIND < 34 ~ "Below TS",  # you can drop this if you want only TS and above
          WMO_WIND < 64 ~ "Tropical Storm",
          WMO_WIND < 83 ~ "Category 1",
          WMO_WIND < 96 ~ "Category 2",
          WMO_WIND < 113 ~ "Category 3",
          WMO_WIND < 137 ~ "Category 4",
          TRUE ~ "Category 5"
        ),
        radius = pmax(WMO_WIND / 10, 3)  # already added for size scaling
      )
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    data <- selected_data()
    
    leaflet(data) |> 
      addTiles() |> 
      addCircleMarkers(
        ~LON, ~LAT,
        popup = ~paste("Wind:", WMO_WIND, "knots"),
        radius = ~radius,
        color = ~wind_palette(WMO_WIND),
        stroke = FALSE,
        fillOpacity = 0.8
      ) |> 
      addLegend(
        "bottomright",
        pal = wind_palette,
        values = data$WMO_WIND,
        title = "Wind Speed (knots)",
        opacity = 1
      )
  })
  
  # Render wind speed plot
  output$wind_plot <- renderPlotly({
    data <- selected_data()
    
    category_lines <- tibble(
      WMO_WIND = category_thresholds,
      Category = c("Tropical Storm", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5")
    )
    
    p <- ggplot(data, aes(x = DATETIME, y = WMO_WIND)) +
      geom_area(fill = "red", alpha = 0.7)
    
    if (input$show_points) {
      p <- p + geom_point()
    }
    
    if (input$show_trendline_category == "All") {
      p <- p + geom_hline(data = category_lines, aes(yintercept = WMO_WIND), 
                          linetype = "dashed", color = "black", alpha = 0.7) +
        geom_text(data = category_lines, aes(x = min(data$DATETIME), y = WMO_WIND, 
                                             label = Category), 
                  hjust = -0.1, vjust = -0.2, size = 3, fontface = "bold", color = "black")
    } else if (input$show_trendline_category != "None") {
      line_data <- category_lines |> filter(Category == input$show_trendline_category)
      p <- p + geom_hline(data = line_data, aes(yintercept = WMO_WIND), 
                          linetype = "dashed", color = "black", alpha = 0.7) +
        geom_text(data = line_data, aes(x = min(data$DATETIME), y = WMO_WIND, 
                                        label = Category), 
                  hjust = -1, vjust = -0.2, size = 3, fontface = "bold", color = "black")
    }
    
    p + labs(
      title = paste("Windspeed Trends for", input$hurricane, "in", input$year),
      x = "Datetime",
      y = "Wind Speed (knots)"
    ) + theme_minimal()
  })
  
  
    
}

# Run the app
shinyApp(ui = ui, server = server)
