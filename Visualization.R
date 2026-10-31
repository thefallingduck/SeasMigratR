VisualizeResults <- function(Distance, Averages, BoundingPolygons) {
  # Determine y-axis limits with a margin of 0.2
  y_min <- min(BoundingPolygons$`2_sd_lower`, na.rm = TRUE) - 0.2
  y_max <- max(BoundingPolygons$`2_sd_upper`, na.rm = TRUE) + 0.2
  
  plot(Distance, Averages, type = "l", col = "blue", lwd = 2, 
       xlab = "Distance", ylab = "d18O", main = "Simulation Results",
       ylim = c(y_min, y_max))  # Set y-axis limits
  
  # Add 1 SD bounding polygon
  polygon(c(Distance, rev(Distance)), 
          c(BoundingPolygons$`1_sd_upper`, rev(BoundingPolygons$`1_sd_lower`)), 
          col = rgb(0, 1, 0, 0.3), border = NA)
  
  # Add 2 SD bounding polygon
  polygon(c(Distance, rev(Distance)), 
          c(BoundingPolygons$`2_sd_upper`, rev(BoundingPolygons$`2_sd_lower`)), 
          col = rgb(1, 0, 0, 0.2), border = NA)
  
  legend("topright", legend = c("Average", "1 SD", "2 SD"), 
         col = c("blue", rgb(0, 1, 0, 0.3), rgb(1, 0, 0, 0.2)), 
         lty = c(1, NA, NA), pch = c(NA, 15, 15), pt.cex = 2, bty = "n")
}
