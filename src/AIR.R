AIR <- function(data, minoritycode, majoritycode = 6) {
  Min_Passrate <-
    (
      sum(data$Race == minoritycode &
            data$Status == 1, na.rm = TRUE) / sum(data$Race == minoritycode, na.rm = TRUE)
    )
  Maj_Passrate <-
    (
      sum(data$Race == majoritycode &
            data$Status == 1, na.rm = TRUE) / sum(data$Race == majoritycode, na.rm = TRUE)
    )
  
  Min_Passrate / Maj_Passrate
}