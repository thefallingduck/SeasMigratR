#' Title
#'
#' @param x 
#' @param width 
#' @param spacing 
#' @param variation 
#' @param badProp 
#'
#' @return
#' @export
#'
#' @examples

TimeFilter <- function(x , width = 10, spacing = 15, variation = 2, badProp = .20){
  
  # Function to approximate SIMS pit time averaging (or any time averaging) with a
  #   component of variability in the placement of the midpoint
  #
  #   Args:
  #     x:          evenly sampled x with same scale as width, spacing, and variation
  #     width:      Width of time averaging
  #     spacing:    Space between middle points of the pits
  #     variation:  Variation in the placement of the mid point
  #     badProp:    Proportion of bad pits
  #   Return:
  #     out:        Three-column dataframe with average, spacing, and flag
  x <- as.data.frame(x)
  colnames(x) <- c("Distance" , "d18O")
  Distance <- seq(from = 10, to = max(x$Distance), by = spacing)
  Distance <- Distance + rnorm(length(Distance), sd = 2)
  
  BadFlag <- rbinom(length(Distance),1,badProp)
  
  d18O <- vector()
  
  for(i in 1:length(Distance)){
    
    set <- x$d18O[x$Distance>=(round(Distance[i]-(width/2)))&x$Distance<=(round(Distance[i]+(width/2)))]
    
    d18O <- append(d18O, mean(set))
    
  }
  
  out <- as.data.frame(cbind(d18O, Distance, BadFlag))
  
  return(out)
  
}