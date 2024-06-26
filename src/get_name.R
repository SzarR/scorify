get_name <- function(city){
  
  # Function purpose is to make a reportable
  # city name variable for printing on official
  # documents
  
  # Step one: remove numbers from vector.
  xrx <- gsub('[[:digit:]]+', '', city)
  
  # Step two: remove doa, sav, and dashes.
  # spaces matter here!
  xrx <- gsub(c("doa|.sav|-"),"", xrx,
                     ignore.case=TRUE)
  
  # Step three: Exclude whatever follows "{"
  xrx <- str_sub(string = xrx,
                         start=1,
                         end=as.numeric(gregexpr(pattern="\\{",
                                                   xrx))[[1]]) 
  
  # # Step four: More trimming.
  # File_Name2 <- gsub(c("\\{"), "",File_Name2,
  #                    ignore.case = TRUE) #trims
  
  # Step five: Trimming.
  xrx <- trimws(xrx, which="both")
  
  return(xrx)
}