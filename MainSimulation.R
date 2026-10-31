source("DepthMigrationSIM.R")
source("Visualization.R")

# Run simulation for multiple years
Years <- 100  # Adjust as needed (e.g., 100, 1000, 10000)
simulation_results <- DepthMigration(Years = Years)

# Visualize results
VisualizeResults(
  Distance = simulation_results$Distance,
  Averages = simulation_results$Averages,
  BoundingPolygons = simulation_results$BoundingPolygons
)
