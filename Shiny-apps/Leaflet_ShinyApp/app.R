# Title: Hurricane Map Analysis (Leaflet Version)

# Description: Visualizes hurricane data in the NA.

# Details: Size of circles are proporional to the wind speed. Users can adjust 
# the color palette and the scaling factor for the circle size.

# Inputs: ibtracs.NA.list.v04r01.csv

# Output: Leaflet map showing hurricane locations, color-coded and sieze adjusted 
# according to wind speed.

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(RColorBrewer)

# Load and prepare data
ibtracs <- "../../ibtracs.NA.csv"

col_names <- c(
  "SID", "SEASON", "NUMBER", "BASIN", "SUBBASIN", "NAME", "ISO_TIME",
  "NATURE", "LAT", "LON", "WMO_WIND", "WMO_PRES", "WMO_AGENCY",
  "TRACK_TYPE", "DIST2LAND", "LANDFALL"
)
col_types <- c(
  "character", "integer", "integer", "character", "character", "character",
  "character", "character", "double", "double", "integer", "integer",
  "character", "character", "integer", "integer"
)

dat <- read.csv(
  file = ibtracs,
  colClasses = c(col_types, rep("NULL", 158)),
  stringsAsFactors = FALSE, 
  # skip = 77876, 
  na.strings = " "
)

colnames(dat) <- col_names

dat <- dat |> 
  mutate(
    DATE = as.Date(ISO_TIME),
    DATETIME = as.POSIXct(ISO_TIME, format = "%Y-%m-%d %H:%M:%S"),
    TIME = format(DATETIME, "%H:%M:%S"),
    START_MONTH = format(DATETIME, "%m")
  )

dat_2 <- dat |>
  group_by(SID) |>
  summarize(max_wind = max(WMO_WIND, na.rm = TRUE)) |>
  mutate(
    SEASON = as.numeric(substr(SID, 1, 4)),
    Category = case_when(
      max_wind < 64 ~ "Tropical Storm",
      max_wind >= 64 & max_wind <= 82 ~ "Category 1",
      max_wind >= 83 & max_wind <= 95 ~ "Category 2",
      max_wind >= 96 & max_wind <= 112 ~ "Category 3",
      max_wind >= 113 & max_wind <= 136 ~ "Category 4",
      max_wind >= 137 ~ "Category 5",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(Category))

# Define UI
ui <- fluidPage(
  titlePanel("Hurricane Map Analysis (Leaflet Version)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_category",
        label = "Select Hurricane Category:",
        choices = c("All", "Tropical Storm", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5"),
        selected = "All"
      ),
      sliderInput(
        inputId = "wind_range",
        label = "Select Wind Speed Range (knots):",
        min = min(dat$WMO_WIND, na.rm = TRUE),
        max = max(dat$WMO_WIND, na.rm = TRUE),
        value = c(50, 150)
      ),
      sliderInput(
        inputId = "year_range",
        label = "Select Year Range:",
        min = min(dat$SEASON, na.rm = TRUE),
        max = max(dat$SEASON, na.rm = TRUE),
        value = c(2000, 2010),
        step = 1
      ),
      selectInput(
        inputId = "month_filter",
        label = "Filter by Start Month:",
        choices = c("All", sprintf("%02d", 1:12)),
        selected = "All"
      ),
      selectInput(
        inputId = "color_palette",
        label = "Choose Color Palette:",
        choices = c("Reds", "Blues", "Greens", "Purples", "Spectral", "YlOrRd"),
        selected = "YlOrRd"
      ),
      sliderInput(
        inputId = "radius_factor",
        label = "Adjust Circle Size Scaling Factor:",
        min = 10,  # Minimum scaling factor
        max = 500,  # Maximum scaling factor
        value = 20  # Default scaling factor
      )
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Define server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data_filtered <- dat |> 
      filter(
        SEASON >= input$year_range[1],
        SEASON <= input$year_range[2],
        WMO_WIND >= input$wind_range[1],
        WMO_WIND <= input$wind_range[2]
      )
    
    if (input$selected_category != "All") {
      selected_sids <- dat_2 |> filter(Category == input$selected_category) |> pull(SID)
      data_filtered <- data_filtered |> filter(SID %in% selected_sids)
    }
    
    if (input$month_filter != "All") {
      data_filtered <- data_filtered |> filter(START_MONTH == input$month_filter)
    }
    
    data_filtered
  })
  
  output$map <- renderLeaflet({
    hurricane_data <- filtered_data()
    
    pal <- colorNumeric(palette = input$color_palette, domain = hurricane_data$WMO_WIND)
    
    leaflet(hurricane_data) |>
      addProviderTiles(provider= "NASAGIBS.ViirsEarthAtNight2012") |>
      addCircles(
        lng = ~LON, lat = ~LAT,
        color = ~pal(WMO_WIND),
        fillOpacity = 0.8,
        radius = ~WMO_WIND * input$radius_factor,  # Dynamic radius scaling
        popup = ~paste(
          "<b>Name:</b>", NAME, "<br>",
          "<b>Year:</b>", SEASON, "<br>",
          "<b>Wind Speed:</b>", WMO_WIND, "knots"
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = hurricane_data$WMO_WIND,
        title = "Wind Speed (knots)"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
