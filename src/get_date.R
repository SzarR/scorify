get_date <- function(city){
  
  # Extract a proper date from the six
  # digit format that we typically see.
  
  # Check for doa in vector string or stop.
  if(!grepl('doa', city)){
    stop("no DOA specified in vector string.")
  }
  
  # Extract location of doa in vector string.
  DOA_Location <- gregexpr(pattern="doa",
                           ignore.case = TRUE,
                           city)[[1]][1] + 4
  
  # Date of Administration
  xrx <- str_sub(city, 
                         start = DOA_Location,
                         end = (DOA_Location + 5))  
  
  # Conversion
  #DateOfAdmin <- lubridate::mdy(DateOfAdmin)
  
  # Customize Format
  xrx <- format(xrx, format = "%B %d, %Y")
  
  return(xrx)
}