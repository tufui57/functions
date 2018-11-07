####################################################################
### Function to convert data frame to point object
####################################################################

points_from_dataframe <- function(data, # data frame
                                  colValue, # colname of point values
                                  coordinateCols = c("x","y") # colnames of coordinates
                                  ){
  
  # Extract target species occurrence records
  points <- data[, coordinateCols]
  # Set coordinates
  coordinates(points) <- data[,coordinateCols]
  # Put values in point object
  points$val <- data[, colValue]
  
  return(points)
}

