#' Title
#'
#' @param WOAdata 
#'
#' @return
#' @export
#'
#' @examples

WaterTemperature <- function(WOAdata = ''){
  
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