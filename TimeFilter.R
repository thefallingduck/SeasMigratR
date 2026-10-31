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

#' SIMSFilter
#'
#' @param x 
#' @param width 
#' @param spacing 
#'
#' @return
#' @export
#'
#' @examples
#' @export
SIMSFilter <- function(x, width = 10, spacing = 15) {
  # Function to apply SIMS filter to a dataset
  #
  # Args:
  #   x:        Two-column dataframe with Distance and d18O
  #   width:    Width of time averaging
  #   spacing:  Space between middle points of the pits
  #
  # Return:
  #   out:      Filtered dataframe with averaged d18O and corresponding distances
  
  x <- as.data.frame(x)
  colnames(x) <- c("Distance", "d18O")
  Distance <- seq(from = min(x$Distance), to = max(x$Distance), by = spacing)
  
  d18O <- sapply(Distance, function(d) {
    subset <- x$d18O[x$Distance >= (d - width / 2) & x$Distance <= (d + width / 2)]
    mean(subset, na.rm = TRUE)
  })
  
  out <- data.frame(Distance = Distance, d18O = d18O)
  return(out)
}