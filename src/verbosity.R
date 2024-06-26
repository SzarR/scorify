verbosity <- function(df) {
  if (Verbose == TRUE) {
    return(df)
    
  } else {
    return(df[, abbrev_df])
  }
  
}