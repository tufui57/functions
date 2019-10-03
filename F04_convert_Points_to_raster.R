convert_dataframe_to_raster <- function(data, # dataframe
                                        ref, # reference raster for coordinate system
                                        val, # Add raster value. This argument should be character string
                                        coordinateNames 
){
  
  # Set coordinates
  points <- data[, coordinateNames]
  coordinates(points) <- data[, coordinateNames]
  
  # Put values in point object
  points$value <- data[, val]
  
  proj4string(points) <- proj4string(ref)
  
  # create blank raster
  r <- raster(ext=extent(ref), res = res(ref))
  
  # Project points. Original coordinate system of BIOCLIM variables is WGS84
  sp_raster <- rasterize(points, ref, field="value", fun = mean)
  
  
  return(sp_raster)
}


project_and_convert_dataframe_to_raster <- function(data, # dataframe
                                               ref, # reference raster for coordinate system
                                               val, # Add raster value. This argument should be character string
                                               coordinateNames 
){
    
  # Project coordinates from WGS84 to NZTM
  dat1 <- project(as.matrix(data[, coordinateNames]), proj4string(ref)) %>% as.data.frame
  # Set coordinates
  points <- dat1[, coordinateNames]
  coordinates(points) <- dat1[, coordinateNames]
  
  # Put values in point object
  
  if(val == "occurrence" ){
    points$sp <- rep(1,nrow(data))  
    proj4string(points) <- proj4string(ref)
  
    # Project points. Original coordinate system of BIOCLIM variables is WGS84
    sp_raster <- rasterize(points, ref, field = 1)
    
  }else{
    
    points$value <- data[, val]
    
    proj4string(points) <- proj4string(ref)
    
    # Project points. Original coordinate system of BIOCLIM variables is WGS84
    sp_raster <- rasterize(points, ref, points$value, fun = mean)
  }

  return(sp_raster)
}

### If coordinates for cells in dataframe is unevenly gridded, i.e., the intervals between coordinates of roww are not consistent,
### the data cannot make raster or make wrong raster.
### The following function fixes the unevenly gridded data.

unevenly_gridded_dataframe_to_raster <- function(data, # dataframe
                                                 val, # colum name of raster values
                                                 coordinateNames, # column names of coodinates
                                                 geocoodingSystem, # CRS
                                                 resol # resolution of raster. Find the right resolution from he intervals between cell coordinates.
){
data.r <- as.matrix(data[, c(coordinateNames, val)])
colnames(data.r) <- c('X', 'Y', 'Z')
# set up an 'empty' raster, here via an extent object derived from your data
e <- extent(data.r[, c(1,2)])

r2 <- raster(crs = CRS(geocoodingSystem),
             ext = e, 
             resolution = resol
)

# you need to provide a function 'fun' for when there are multiple points per cell
x <- rasterize(data.r[, 1:2], r2, data.r[,3], fun=mean)
return(x)
}
