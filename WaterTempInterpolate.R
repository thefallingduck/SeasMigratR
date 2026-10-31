#' Title
#'
#' @param WOAdata 
#'
#' @return
#' @export
#'
#' @examples

WaterTemperature <- function(WOAdata = '') {
  if (is.null(WOAdata) || nrow(WOAdata) == 0) {
    stop("WOAdata is required to calculate water temperatures.")
  }
  
  #### Args: ####
  
  #       data = dataframe in the format where columns are depths and rows are months
  
  #### Return : ####
  
  #       WaterTemps = dataframe where rows are depth, columns are days
  
  
  MonthInterp <- matrix(nrow = max(unique(WOAdata$Depth)), ncol = 12)
  
  for(i in 1:12){
    
    new <- approx(x=WOAdata$Depth[WOAdata$Month==i],WOAdata$Temp[WOAdata$Month==i], xout = 1:max(WOAdata$Depth))
    MonthInterp[,i] <- new$y
    
  }
  
  DayInterp <- matrix(nrow = max(WOAdata$Depth), ncol = 365)
  
  for(k in 1:nrow(MonthInterp)){
    
    DayInterp[k,]<-approx(MonthInterp[k,1:12],n=365)$y
    
  }
  
  return(DayInterp)
  
}

#' GenerateSyntheticWOA
#'
#' @param maxDepth Maximum depth for the simulation
#' @param shallowSummerHigh Surface temperature in summer for shallow areas
#' @param shallowWinterLow Surface temperature in winter for shallow areas
#' @param deepSummerHigh Deep temperature in summer
#' @param deepWinterLow Deep temperature in winter
#'
#' @return A synthetic WOA-like dataset
#' @export
#'
#' @examples
GenerateSyntheticWOA <- function(maxDepth = 400, 
                                 shallowSummerHigh = 30, 
                                 shallowWinterLow = 2, 
                                 deepSummerHigh = 10, 
                                 deepWinterLow = 4) {
  # Generate synthetic temperature data for the water column
  depths <- seq(0, maxDepth, by = 1)
  months <- 1:12
  tempProfile <- data.frame()
  
  for (month in months) {
    # Interpolate temperature between summer high and winter low
    seasonalFactor <- ifelse(month %in% c(6, 7, 8), 1, ifelse(month %in% c(12, 1, 2), -1, 0))
    shallowTemp <- (shallowSummerHigh + shallowWinterLow) / 2 + seasonalFactor * (shallowSummerHigh - shallowWinterLow) / 2
    deepTemp <- (deepSummerHigh + deepWinterLow) / 2 + seasonalFactor * (deepSummerHigh - deepWinterLow) / 2
    depthTemps <- shallowTemp - (shallowTemp - deepTemp) * (depths / maxDepth)
    tempProfile <- rbind(tempProfile, data.frame(Depth = depths, Month = month, Temp = depthTemps))
  }
  
  return(tempProfile)
}

#' WaterTempInterpolate
#'
#' @param WOAdata WOA dataset or synthetic dataset
#'
#' @return Interpolated temperature dataset
#' @export
#'
#' @examples
WaterTempInterpolate <- function(WOAdata) {
  if (is.null(WOAdata) || nrow(WOAdata) == 0) {
    # Generate synthetic WOA data if none is provided
    WOAdata <- GenerateSyntheticWOA()
  }
  
  # ...existing code to interpolate WOAdata...
}