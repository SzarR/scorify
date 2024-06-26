demo_recoder <- function(df) {
  # Function that takes three columns: Race,
  # Gender, Status from character/string to
  # numeric or vice-versa with assumptions:
  # Race 6=White, 1=AA, 3=Hispanic
  # Gender 1=Male, 2=Female
  # Status 1=Pass 0=Did Not Pass

  # Recode Race from char -> numeric.
  if ("Race" %in% colnames(df) &
      is.character(df$Race)) {
    df$Race <-
      ifelse(
        df$Race == "Black" |
          df$Race == "African American" |
          df$Race == "Afr. American" |
          df$Race == "Black - Non-Hispanic",
        1,
        ifelse(
          df$Race == "Asian",
          2,
          ifelse(
            df$Race == "Hispanic",
            3,
            ifelse(
              df$Race == "Nat American",
              4,
              ifelse(
                df$Race == "Pac Island" |
                  df$Race == "Pac Islander" |
                  df$Race == "Pacific Islander",
                5,
                ifelse(
                  df$Race == "White" |
                    df$Race == "Caucasian" |
                    df$Race == "White - Non-Hispanic",
                  6,
                  ifelse(df$Race == "Two or More", 7, NA)
                )
              )
            )
          )
        )
      )

  }

  # Recode Race from numeric -> character.
  else if ("Race" %in% colnames(df) &
           is.numeric(df$Race)) {
    df$Race <-
      ifelse(df$Race == 1,
             "African American/Black",
             ifelse(
               df$Race == 2,
               "Asian",
               ifelse(
                 df$Race == 3,
                 "Hispanic",
                 ifelse(
                   df$Race == 4,
                   "American Indian/Native Alaskan",
                   ifelse(
                     df$Race == 5,
                     "Pacific Islander/Native Hawaiian",
                     ifelse(
                       df$Race == 6,
                       "White",
                       ifelse(df$Race == 7,
                              "Two or More Races",
                              NA)
                     )
                   )
                 )
               )
             ))

      }

  # Recode Gender from char -> numeric.
  if ("Gender" %in% colnames(df) &
      is.character(df$Gender)) {
    df$Gender <- ifelse(df$Gender == "Male",
                        1,
                        ifelse(df$Gender == "Female", 2, NA))

  }
  # Recode Gender from numeric -> char.
  else if ("Gender" %in% colnames(df) &
           is.numeric(df$Gender)) {
    df$Gender <- ifelse(df$Gender == 1,
                        "Male",
                        ifelse(df$Gender == 2,
                               "Female",
                               NA))

  }
  
  # Recode Status from char -> numeric.
  if ("Status" %in% colnames(df) &
      is.character(df$Status)) {
    df$Status <- ifelse(df$Status == "Did Not Pass",
                        0,
                        ifelse(df$Status == "Pass", 1, NA))
    
  }
  
  # Recode Status from numeric -> char.
  else if ("Status" %in% colnames(df) &
           is.numeric(df$Status)) {
    df$Status <- ifelse(df$Status == 0,
                        "Did Not Pass",
                        ifelse(df$Status == 1,
                               "Pass",
                               NA))

  }
  return(df)
}
