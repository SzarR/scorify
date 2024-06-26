
###############Global Settings###############
cog_items <- paste0("c", 1:10)

if(Examination == "Aviator") {
  abbrev_df <<-
    c("LName",
      "FName",
      "Race",
      "Gender",
      "ID",
      "Final_Score",
      "Final_Passing")
}

if(Form == "Form A"){
  TOAST_FA <- datum()
} else if(Form == "Form B"){
  TOAST_FB <- datum()
}


########################################################Cognitive Scoring#########################################################
# Datum gets re-assigned TOAST_FA, TOAST_FB, TOAST_FC, TOAST_FD in the server.R page, when the user selects
# from the various drop down menus the exam, form and SP version.
##############################################################FORM A##############################################################
if(exists("TOAST_FA")){
  TOAST_FA <- as_tibble(TOAST_FA)
  TOAST_FA <- as_tibble(TOAST_FA)
  TOAST_FA[,"Form"] <- as.character("A")
  
  #Recode Missing as 0 and Create Raw Cog & Noncog Items
  TOAST_FA[cog_items][is.na(TOAST_FA[cog_items])] <- 0
  
  #Form A scoring key
  cog.key <- c(1, 4, 3, 2, 4, 3, 1, 4, 2, 3)
  
  cog.scored <- score.multiple.choice(cog.key, TOAST_FA[cog_items], score = FALSE)
  
  TOAST_FA %<>% rename_at(., .vars = cog_items, .funs = funs(paste0(., "_raw")))
  TOAST_FA <- cbind(TOAST_FA, cog.scored)
  
  #Compute Overall Cognitive Score
  TOAST_FA %<>% mutate(Cog_Sum = c1 + c2 + c3 + c4 + c5 + c6 + c7 +c8 + c9 + c10)
  TOAST_FA %<>% mutate(Final_Score = Cog_Sum * 10)
  
  TOAST <- TOAST_FA
}

if(exists("TOAST_FB")){
  TOAST_FB <- as_tibble(TOAST_FB)
  TOAST_FB <- as_tibble(TOAST_FB)
  TOAST_FB[,"Form"] <- as.character("B")
  
  #Recode Missing as 0 and Create Raw Cog & Noncog Items
  TOAST_FB[cog_items][is.na(TOAST_FB[cog_items])] <- 0
  
  #Form A scoring key
  cog.key <- c(1, 4, 4, 3, 4, 4, 3, 4, 2, 3)
  
  cog.scored <- score.multiple.choice(cog.key, TOAST_FB[cog_items], score = FALSE)
  
  TOAST_FB %<>% rename_at(., .vars = cog_items, .funs = funs(paste0(., "_raw")))
  TOAST_FB <- cbind(TOAST_FB, cog.scored)
  
  #Compute Overall Cognitive Score
  TOAST_FB %<>% mutate(Cog_Sum = c1 + c2 + c3 + c4 + c5 + c6 + c7 +c8 + c9 + c10)
  TOAST_FB %<>% mutate(Final_Score = Cog_Sum * 10)
  
  TOAST <- TOAST_FB
}


##############################################################FORM B##############################################################

TOAST %<>% mutate(Final_Passing = ifelse(Final_Score > 70, 1, 0))

TOAST[,"Final_Passing"] <- recode(TOAST$Final_Passing, '1' = 'Pass', '0' = 'Did Not Pass')

TOAST %<>%
  arrange(desc(Final_Score))

# if(Form == "Form A/A"|
#    Form == "Form B/A"|
#    Form == "Form C/A"|
#    Form == "Form D/A"){
#   LST_Data <<- TOAST
# }


  