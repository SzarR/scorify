get_name_abbrev <- function(city){
  
  # Ensure city is a character vector.
  if(!is.character(city)) {
    stop("Vector must be numeric.")
  }
  
  # Extract abbreviations and remove numbers.
  Name <- str_c(unlist(c(
    str_extract_all(city, "[A-Z]+"),
    str_extract(city, "[0-9]{6}")
  )),
  collapse = "")
  
  # Check for Common Exam Acronyms.
  Name <- gsub(
    c(
      "LST|
      SBSA|
      FST|
      SAV|
      NCJOSI2|
      FF|
      LEO|
      I2|
      NGLE|
      LSTA|
      LSTB|
      LSTC|
      LSTD|
      FSTA|
      FSTB|
      FSTC|
      FSTD|
      NCJOSI|
      NCJ|
      SBSAA|
      FORM A|
      FORM B| 
      FORM C|
      FORM D|
      NGFF|
      NG FF|
      NGFF|
      NGLE"
    ),
    "",
    Name,
    ignore.case = TRUE
  )
  return(Name)
}