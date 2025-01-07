# Install necessary packages if not already installed
install.packages(c("shiny", "shinydashboard", "tidyverse", "tigris", "sf", "tmap"))

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tigris)
library(sf)
library(tmap)

# Import sociodemographics dataset
socio_demographics <- read.csv("us_sociodemographics_county_05-06-2024.csv")

# Import food desert dataset
food_desert <- read.csv("us_food_desert_tract_05-06-2024.csv")

# Filter for Virginia state and specific counties for sociodemographics
counties_of_interest <- c("Chesterfield", "Hopewell", "Petersburg", "Sussex", "Prince George", "Dinwiddie")
socio_demographics_va <- socio_demographics %>%
  filter(State == "Virginia") %>%
  filter(grepl(paste(counties_of_interest, collapse = "|"), County, ignore.case = TRUE))

# Convert sociodemographics dataset to long format for ggplot
socio_demographics_va_long <- socio_demographics_va %>%
  pivot_longer(cols = !c(State, County), names_to = "Variable", values_to = "Value")

# Filter for Virginia state and specific counties for food desert data
food_desert_va <- food_desert %>%
  filter(State == "Virginia") %>%
  filter(grepl(paste(counties_of_interest, collapse = "|"), County, ignore.case = TRUE))

# Convert food desert dataset to long format for ggplot
food_desert_va_long <- food_desert_va %>%
  pivot_longer(cols = !c(State, County, Tract), names_to = "Variable", values_to = "Value")

# Get Virginia tracts shapefile/geographical boundaries
va_tracts <- tracts(state = "VA", cb = TRUE)
va_tracts <- st_as_sf(va_tracts)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Virginia Sociodemographics and Food Desert Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Map and Graph", tabName = "map_graph", icon = icon("dashboard"))
    ),
    selectInput('dataset', 'Select Dataset', choices = c("Sociodemographics", "Food Desert")),
    uiOutput("variable_ui")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                box(title = "Introduction to the Datasets", width = 12,
                    p("This dashboard provides insights into sociodemographic variables and food desert status across selected counties in Virginia."),
                    p("The Socio-demographics dataset includes variables such as income levels, educational attainment, and employment status."),
                    p("The Food Desert dataset highlights areas with limited access to affordable and nutritious food, which can impact community health."),
                    p("Use the 'Map and Graph' tab to explore the data visually.")
                )
              )
      ),
      tabItem(tabName = "map_graph",
              fluidRow(
                box(width = 12, tmapOutput("map_county")),  # Update leafletOutput to tmapOutput
                box(width = 12, plotOutput("plot_socio_demographics"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Update variable selection based on selected dataset
  output$variable_ui <- renderUI({
    dataset <- if (input$dataset == "Sociodemographics") {
      socio_demographics_va_long
    } else {
      food_desert_va_long
    }
    selectInput('variable', 'Select Variable', choices = unique(dataset$Variable))
  })
  
  # Render choropleth map of the selected variable
  output$map_county <- renderTmap({
    dataset <- if (input$dataset == "Sociodemographics") {
      socio_demographics_va_long
    } else {
      food_desert_va_long
    }
    
    selected_variable <- dataset %>%
      filter(Variable == input$variable)
    
    # Prepare spatial data
    va_counties <- counties(state = "VA", cb = TRUE)
    
    # Align names between datasets
    county_lookup <- data.frame(
      disparity_name = counties_of_interest,
      spatial_name = c("Chesterfield", "Hopewell", "Petersburg", "Sussex", "Prince George", "Dinwiddie")
    )
    
    selected_variable <- selected_variable %>%
      left_join(county_lookup, by = c("County" = "disparity_name"))
    
    va_counties_filtered <- va_counties %>%
      filter(NAME %in% county_lookup$spatial_name)
    
    va_counties_merged <- va_counties_filtered %>%
      left_join(selected_variable, by = c("NAME" = "spatial_name"))
    
    # Ensure Value is numeric for color mapping
    va_counties_merged$Value <- as.numeric(va_counties_merged$Value)
    
    # Create tmap choropleth with color coding
    tm_shape(va_counties_merged) +
      tm_polygons(col = "Value", 
                  palette = "viridis",    # Color palette
                  title = paste(input$variable, "Value"),
                  style = "quantile") +    # Style can be "quantile" or "cont"
      tm_borders(lwd = 2, col = "black") +
      tm_text("NAME", size = 1) +  # Add labels with county names
      tm_layout(title = paste("Map of", input$variable, "in Selected VA Counties"))
  })
  
  # Render bar graph of the selected variable
  output$plot_socio_demographics <- renderPlot({
    dataset <- if (input$dataset == "Sociodemographics") {
      socio_demographics_va_long
    } else {
      food_desert_va_long
    }
    
    selected_data <- dataset %>%
      filter(Variable == input$variable)
    
    ggplot(selected_data, aes(x = reorder(County, -Value), y = Value, fill = County)) +
      geom_col() +
      theme_minimal() +
      labs(title = paste(input$variable, "across counties"),
           x = "County",
           y = input$variable) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



