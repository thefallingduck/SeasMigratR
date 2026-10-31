# Ensure required packages are installed
required_packages <- c("shiny", "ggplot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

library(shiny)
library(ggplot2)

# Source the required functions
source("BoundRandWalk.R")
source("DepthMigrationSIM.R")
source("TimeFilter.R")
source("WaterTempInterpolate.R")

# Define UI
ui <- fluidPage(
  titlePanel("Simulation of Depth Migration"),
  sidebarLayout(
    sidebarPanel(
      numericInput("Days", "Days of Growth:", value = 365, min = 1),
      numericInput("GrowthRate", "Growth Rate (microns/day):", value = 35, min = 1),
      numericInput("SampleWidth", "Sample Width:", value = 260, min = 1),
      numericInput("Depth", "Water Column Depth:", value = 400, min = 1),
      numericInput("Rate", "Rate of Change (2SD):", value = 150, min = 1),
      numericInput("Spacing", "Spacing of Averages:", value = 400, min = 1),
      numericInput("Years", "Number of Years:", value = 1, min = 1),
      fileInput("WOAdata", "Upload WOA Data (CSV):"),
      numericInput("shallowSummerHigh", "Shallow Summer High Temp (°C):", value = 30, min = -10, max = 40),
      numericInput("shallowWinterLow", "Shallow Winter Low Temp (°C):", value = 2, min = -10, max = 40),
      numericInput("deepSummerHigh", "Deep Summer High Temp (°C):", value = 10, min = -10, max = 40),
      numericInput("deepWinterLow", "Deep Winter Low Temp (°C):", value = 4, min = -10, max = 40),
      actionButton("runSim", "Run Simulation")
    ),
    mainPanel(
      plotOutput("simulationPlot"),
      tableOutput("simulationTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  simulationData <- reactive({
    req(input$runSim)  # Ensure simulation only runs after button press
    if (is.null(input$WOAdata)) {
      # Generate synthetic WOA data using user inputs for water column conditions
      WOAdata <- GenerateSyntheticWOA(
        maxDepth = input$Depth,
        shallowSummerHigh = input$shallowSummerHigh,
        shallowWinterLow = input$shallowWinterLow,
        deepSummerHigh = input$deepSummerHigh,
        deepWinterLow = input$deepWinterLow
      )
    } else {
      WOAdata <- read.csv(input$WOAdata$datapath)
    }
    
    # Run the simulation
    result <- DepthMigration(
      Days = input$Days,
      GrowthRate = input$GrowthRate,
      SampleWidth = input$SampleWidth,
      depth = input$Depth,
      rate = input$Rate,
      spacing = input$Spacing,
      WOAdata = WOAdata,
      Years = input$Years  # Pass the number of years to the simulation
    )
    return(result)
  })
  
  output$simulationPlot <- renderPlot({
    req(input$runSim)
    data <- simulationData()
    
    # Convert simulation results into a data.frame for ggplot
    plot_data <- data.frame(
      Distance = data$Distance,
      Averages = data$Averages
    )
    
    ggplot(plot_data, aes(x = Distance, y = Averages)) +
      geom_line(color = "blue") +
      labs(title = "Depth Migration Simulation", x = "Distance", y = "d18O") +
      theme_minimal()
  })
  
  output$simulationTable <- renderTable({
    req(input$runSim)
    simulationData()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
