# Load required libraries
library(shiny)
library(dplyr)
library(caret)
library(ggplot2)
# install.packages("leaflet")
library(leaflet)
library(sf)

ui <- fluidPage(
  titlePanel("Melbourne Housing Suitability Score Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("color", "Select Polygon Color:", choices = c("Red", "Grey", "Green"))
    ),
    mainPanel(
      h3("Worst 5 Suburbs Based on Suitability Score"),  # Title for the map
      leafletOutput("map"),            # This will show the map
      plotOutput("suitabilityPlot"),  # This will show the plot
      tableOutput("topSuburbsTable")  # This will show the table of top 5 suburbs
    )
  )
)

# Server: Define the server logic
server <- function(input, output) {
  
  # Load and preprocess the data
  file_path <- "/Users/arvindhraghavan/Desktop/Bootstrap_Landing/MELBOURNE_HOUSE_PRICES_LESS_CLEAN.csv"
  melbourne_housing <- read.csv(file_path)
  
  melbourne_housing <- melbourne_housing %>%
    mutate(Normalized_Price = scale(Price, center = TRUE, scale = TRUE)) %>%
    mutate(Affordability = 1 / (Normalized_Price + abs(min(Normalized_Price)) + 1))
  
  alpha <- 0.4
  beta <- 0.4
  gamma <- 0.2
  
  melbourne_housing <- melbourne_housing %>%
    mutate(Suitability_Score = alpha * Affordability - beta * Distance + gamma * Rooms)
  
  data <- melbourne_housing %>%
    select(Price, Distance, Rooms, Suitability_Score)
  
  set.seed(123)
  train_index <- createDataPartition(data$Suitability_Score, p = 0.8, list = FALSE)
  train_data <- data[train_index,]
  test_data <- data[-train_index,]
  
  model_lr <- train(Suitability_Score ~ Price + Distance + Rooms, data = train_data, method = "lm")
  
  train_predictions <- predict(model_lr, train_data)
  train_actual <- train_data$Suitability_Score
  test_predictions <- predict(model_lr, test_data)
  test_actual <- test_data$Suitability_Score
  
  train_results <- data.frame(Predicted = train_predictions, Actual = train_actual, Set = "Training")
  test_results <- data.frame(Predicted = test_predictions, Actual = test_actual, Set = "Test")
  combined_results <- rbind(train_results, test_results)
  
  # Output the plot
  output$suitabilityPlot <- renderPlot({
    ggplot(combined_results, aes(x = Actual, y = Predicted, color = Set)) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Model Performance: Training vs Test Data",
           x = "Actual Suitability Score",
           y = "Predicted Suitability Score") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  # Calculate and display the top 5 suburbs with highest average suitability score
  top_5_suburbs <- melbourne_housing %>%
    group_by(Suburb) %>%
    summarize(Average_Suitability_Score = mean(Suitability_Score, na.rm = TRUE)) %>%
    arrange(desc(Average_Suitability_Score)) %>%
    head(5)
  
  output$topSuburbsTable <- renderTable({
    top_5_suburbs
  })
  
  # Load the suburb boundary data 
  suburb_boundaries <- st_read("suburb/suburb.shp")
  
  # Check if the shapefile is in the correct projection (WGS84), if not, transform it
  if (st_crs(suburb_boundaries)$epsg != 4326) {
    suburb_boundaries <- st_transform(suburb_boundaries, 4326)
  }
  
  # Filter the boundary data to include only the top 5 suburbs using the correct column name 'Name'
  top_5_boundaries <- suburb_boundaries %>%
    filter(Name %in% top_5_suburbs$Suburb)
  
  # Render map with highlighted top 5 suburbs areas
  output$map <- renderLeaflet({
    # Get the selected color from the dropdown
    polygon_color <- switch(input$color, "Red" = "red", "Grey" = "grey", "Green" = "green")
    
    leaflet(data = top_5_boundaries) %>%
      addTiles() %>%
      addPolygons(color = polygon_color, weight = 2, smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.3,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  label = ~as.character(Name)) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 10)  # Center the map on Melbourne
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
