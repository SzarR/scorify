SMD <- function(data, score, minoritycode, majoritycode = 6) {
  Min_Testing <- sum(data$Race == minoritycode, na.rm = TRUE)
  Maj_Testing <- sum(data$Race == majoritycode, na.rm = TRUE)
  Min_Mean   <-
    sapply(data[data$Race == minoritycode, score], mean, na.rm = TRUE)
  Maj_Mean   <-
    sapply(data[data$Race == majoritycode, score], mean, na.rm = TRUE)
  Min_SD   <-
    sapply(data[data$Race == minoritycode, score], sd, na.rm = TRUE)
  Maj_SD   <-
    sapply(data[data$Race == majoritycode, score], sd, na.rm = TRUE)
  
  (Maj_Mean - Min_Mean) / sqrt((((Min_Testing - 1) * (Min_SD ^ 2)) + ((Maj_Testing - 1)
                                                                      * (Maj_SD ^ 2))) / (Min_Testing + Maj_Testing - 2))
}