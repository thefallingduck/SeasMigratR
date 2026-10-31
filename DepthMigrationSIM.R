#' Title
#'
#' @param Days 
#' @param GrowthRate 
#' @param SampleWidth 
#' @param depth 
#' @param rate 
#' @param spacing 
#' @param WOAdata 
#' @param Years  # New parameter to specify the number of years
#'
#' @return
#' @export
#'
#' @examples
DepthMigration <- function(
  Days = 365,
  GrowthRate = 35,
  SampleWidth = 260,
  depth = 400,
  rate = 150,
  spacing = 400,
  WOAdata = '',
  Years = 1  # New parameter to specify the number of years
){
  # Modeling depth migration with random walk and changing water column temperature structure
  #
  # Args:
  #   Days:         Days of growth in the model
  #   GrowthRate:   Rate of growth in microns/day
  #   SampleWidth:  Linear width of sample, time averaging effect
  #   depth:        Water column depth
  #   rate:         change in depth per hour 2SD
  #   spacing:      spacing of averages
  #   WOAdata:      World ocean atlas data for temperature parameterization
  #   Years:        Number of years to run the simulation
  #
  # Output:         d18Oaragonite per sample distance
  
  all_results <- list()  # Store results for each year
  
  for (year in 1:Years) {
    # Run the simulation for a single year
    Temperatures <- WaterTemperature(WOAdata = WOAdata)
    DayTempsDF <- Temperatures
    HourlyRW <- BoundRW(N = Days * 24, UB = depth, LB = 0, rate = rate)
    
    Day <- vector()
    Hour <- vector()
    for(i in 1:Days){
      
      Day <- append(Day, rep(i, times = 24))
      Hour <- append(Day, 1:24)
    }
    
    DepthTimeDF <- as.data.frame(cbind(Day, Hour, HourlyRW))
    
    Distance <- (1:nrow(DepthTimeDF))*(GrowthRate/24)
    Hourlyd18O <- DepthTimeDF$HourlyRW*0
    
    for(i in 1:nrow(DepthTimeDF)){
      
      Hourlyd18O[i] <- DayTempsDF[round(DepthTimeDF$HourlyRW[i]+.5),DepthTimeDF$Day[i]]
      
    }
    
    Hourlyd18O <- (((17.88*(1000/(Hourlyd18O+273.4))-31.14)+0)-30.91)/1.03091
    
    x <- cbind(Distance, Hourlyd18O)
    SimsResults <- SIMSFilter(x, width = SampleWidth, spacing = spacing)
    all_results[[year]] <- SimsResults$d18O
  }
  
  # Summarize results across years
  all_results_matrix <- do.call(cbind, all_results)
  averages <- rowMeans(all_results_matrix, na.rm = TRUE)
  std_devs <- apply(all_results_matrix, 1, sd, na.rm = TRUE)
  
  # Generate bounding polygons
  bounding_polygons <- list(
    "1_sd_upper" = averages + std_devs,
    "1_sd_lower" = averages - std_devs,
    "2_sd_upper" = averages + 2 * std_devs,
    "2_sd_lower" = averages - 2 * std_devs
  )
  
  # Return summarized results
  return(list(
    Distance = SimsResults$Distance,
    Averages = averages,
    BoundingPolygons = bounding_polygons
  ))
}