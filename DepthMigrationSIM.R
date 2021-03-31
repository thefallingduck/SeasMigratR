#' Title
#'
#' @param Days 
#' @param GrowthRate 
#' @param SampleWidth 
#' @param depth 
#' @param rate 
#' @param spacing 
#' @param WOAdata 
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
  WOAdata = ''
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
  #   
  #
  # Output:         d18Oaragonite per sample distance
  
  Output <- data.frame()
  
  #### Calculate water column temperature values through a year by day ####
  
  Temperatures <- WaterTemperature(WOAdata = WOAdata)
  
  DayTempsDF <- Temperatures
  
  
  #### Calculate migration depth pattern by hour ####
  
  HourlyRW <- BoundRW(N = Days*24, UB = depth, LB = 0, rate = rate)
  
  Day <- vector()
  Hour <- vector()
  for(i in 1:Days){
    
    Day <- append(Day, rep(i, times = 24))
    Hour <- append(Hour, 1:24)
  }
  
  DepthTimeDF <- as.data.frame(cbind(Day, Hour, HourlyRW))
  
  #### Growth distance and d18O ####
  
  Distance <- (1:nrow(DepthTimeDF))*(GrowthRate/24)
  Hourlyd18O <- DepthTimeDF$HourlyRW*0
  
  for(i in 1:nrow(DepthTimeDF)){
    
    Hourlyd18O[i] <- DayTempsDF[round(DepthTimeDF$HourlyRW[i]+.5),DepthTimeDF$Day[i]]
    
  }
  
  Hourlyd18O <- (((17.88*(1000/(Hourlyd18O+273.4))-31.14)+0)-30.91)/1.03091
  
  
  
  x <- cbind(Distance, Hourlyd18O)
  
  #### Apply SIMS filter to dataset ####
  
  SimsResults <- SIMSFilter(x, width = SampleWidth, spacing = spacing)
  
  Output<-rbind(Output, SimsResults)
}