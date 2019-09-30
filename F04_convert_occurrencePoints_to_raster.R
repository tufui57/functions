

project_and_convert_occurrencePoints_to_raster <- function(data, # list object with occurrence data
                                               refWGS, # reference raster for coordinate system
                                               val, # Add raster value. This argument should be character string
                                               coordinateNames # 
){
    
  # Project coordinates from WGS84 to NZTM
  nztm <- project(as.matrix(data[, coordinateNames]), proj4string(refWGS)) %>% as.data.frame
  # Set coordinates
  points <- nztm[, coordinateNames]
  coordinates(points) <- nztm[, coordinateNames]
  # Put values in point object
  
  if(val == "occurrence" ){
    points$sp <- rep(1,nrow(data))  
  proj4string(points) <- proj4string(refWGS)
  
  # Project points. Original coordinate system of BIOCLIM variables is WGS84
  sp_raster <- rasterize(points, refWGS, field = 1)
  }else{
    points$value <- data[, val]
    
    proj4string(points) <- proj4string(refWGS)
    
    # Project points. Original coordinate system of BIOCLIM variables is WGS84
    sp_raster <- rasterize(points, refWGS, fun = mean)
  }

  return(sp_raster)
}
