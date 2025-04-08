library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(viridis)

# ------------------------------------------------------------------------------
# Title: Hurricane Map Analysis (Version 3)
# ------------------------------------------------------------------------------
# Description:
# This Shiny app visualizes historical hurricane tracks. Users can filter hurricanes 
# based on category, wind speed, year range, and month. The app provides an 
# interactive map where storms are color-coded by wind speed, with a customizable 
# color palette and the option to reverse colors.
# ------------------------------------------------------------------------------
# Details:
# - Uses the ibtracs dataset to visualize hurricane tracks.
# - Filters storms based on user input (year, wind speed, category, and month).
# - Provides multiple color palette options for visualization.
# - Users can reverse the color scale for better contrast.
# ------------------------------------------------------------------------------
# Inputs:
# - **ibtracs dataset (CSV file)**: Contains hurricane details including ID, year, 
#   wind speed, latitude, and longitude.
# - **User-selected filters:**
#   - Hurricane category
#   - Wind speed range
#   - Year range
#   - Start month
#   - Color palette (Viridis, Magma, Plasma, Inferno)
#   - Option to reverse color scale
# ------------------------------------------------------------------------------
# Output:
# - **Interactive map plot** displaying hurricane tracks.
# - **Color-coded wind speed representation** with customizable palette.
# - **Highlighted storm paths** based on selected filters.
# ------------------------------------------------------------------------------

# Load and prepare data
ibtracs <- "/Users/mikaellaliveri/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Year 4/Spring/STAT199/ibtracs-data/csv-raw-files/ibtracs.NA.list.v04r01.csv"

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
  skip = 77876, 
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

world_map <- ne_countries(returnclass = "sf")

# Define UI
ui <- fluidPage(
  titlePanel("Hurricane Map Analysis (Version 3)"),
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
        choices = c("viridis", "magma", "plasma", "inferno"),
        selected = "viridis"
      ),
      checkboxInput(
        inputId = "reverse_palette",
        label = "Reverse Color Palette",
        value = FALSE
      )
    ),
    mainPanel(
      plotOutput("map_plot", height = "600px")
    )
  )
)

# Define server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data_filtered <- dat |> 
      filter(
        SEASON >= input$year_range[1],
        SEASON <= input$year_range[2]
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
  
  output$map_plot <- renderPlot({
    hurricane_data <- filtered_data()
    
    color_scale <- scale_color_viridis_c(
      option = input$color_palette,
      direction = ifelse(input$reverse_palette, -1, 1),
      name = "Wind Speed (knots)"
    )
    
    ggplot() +
      geom_sf(data = world_map, fill = "grey70", color = "grey50") +  # Darker grey continents
      geom_path(data = hurricane_data, aes(x = LON, y = LAT, group = SID),
                color = "grey20", linewidth = 0.5, alpha = 0.5) +  # Full path in light grey
      geom_path(data = hurricane_data |> 
                  filter(WMO_WIND >= input$wind_range[1], WMO_WIND <= input$wind_range[2]),
                aes(x = LON, y = LAT, group = SID, color = WMO_WIND),
                linewidth = 1.5, alpha = 0.9) +  # Highlighted section
      color_scale +
      labs(
        title = paste("Hurricane Tracks:", input$year_range[1], "-", input$year_range[2]),
        subtitle = paste("Category:", input$selected_category, "| Wind Speed:", input$wind_range[1], "-", input$wind_range[2], "knots"),
        x = "Longitude",
        y = "Latitude"
      ) +
      coord_sf(xlim = c(-130, -10), ylim = c(0, 70)) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16, face = "bold", color = "black"),
        plot.subtitle = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        axis.text = element_text(color = "black"),
        legend.position = "right",
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
