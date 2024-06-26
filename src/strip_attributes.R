strip_attributes <- function(data){
  library(dplyr)
  library(labelled)
  d <- data
  for (i in 1:ncol(d)) {
    
    d[[i]] <- as.vector(d[[i]])
    
  }
  return(d)
}