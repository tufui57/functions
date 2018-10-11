
##########################################
# Schonner's D dataframe formatting
##########################################

# Format the result object of schonerD_ecospat()

SchonnerDdataframeFormat <- function(scho, # List of correlated/uncorrelated Schoenner's D and I
                                     colname = c("ecospat.corrected.D", "ecospat.uncorrected.D"), # names of columns
                                     chooseIndex = "D" # choose overlap index from D and I
                                     ){

  scholist <- lapply(1:length(scho), function(i){
    
    ### If list has null objects
    
    if(scho[[i]] %>% length == 0){
      
      return(c(999, 999))
      
    }else{
      
      # Convert list to dataframe
      x <- data.frame(scho[[i]])
      
      # Add colnames
      colnames(x) <- colname
      
      # Get the index you need
      return(x[chooseIndex, ])
      
    }
    }
  )
  
  # Add column in schonner's D dataframe showing clade numbers
  scholist <- do.call(rbind, scholist)
  
  # Row names
  rownames(scholist) <- names(scho)
  
  return(scholist)
  
}
