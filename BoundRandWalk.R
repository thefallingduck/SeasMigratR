#' Title
#'
#' @param N 
#' @param UB 
#' @param LB 
#' @param rate 
#'
#' @return
#' @export
#'
#' @examples
#' 
BoundRW <- function(N = 1000,
                    UB = 300,
                    LB = 0,
                    rate = 2) {
  # Function for creating a bound random walk time series
  #
  #   Args:
  #     N:    Number of steps to take
  #     UB:   Value of the upper bound
  #     LB:   Value of the lower bound
  #     rate: Value of 1 sd on rate of change
  #   Return:
  #     Path: Bound random walk path in one dimension
  #
  
  xdis = c(runif(1, min = LB, max = UB),rnorm(N-1, 0 , rate))
  
  xdis1 = rep(1, N)
  
  xdis1[1] <- abs(xdis[1])
  
  for (i in 1:(N - 1)) {
    if ((xdis1[i] + xdis[i + 1]) >= UB) {
      xdis1[i + 1] = UB
      
    } else{
      if ((xdis1[i] + xdis[i + 1]) <= LB) {
        xdis[i + 1] = LB
        
      } else{
        xdis1[i + 1] = xdis1[i] + xdis[i + 1]
        
      }
    }
  }
  
  return(xdis1)
  
}