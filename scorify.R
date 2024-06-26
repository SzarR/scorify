# Global Settings ---------------------------------------------------------
options(java.parameters = "-Xmx8g")
list.of.packages <- c("shiny","haven","DT","shinythemes","dplyr","xlsx","rJava","stringdist", "magrittr",
                      "tabulizer","lubridate","tm","stringr","tools","labelled", "knitr", "kableExtra",
                      "extrafont", "RODBC", "DBI", "dbplyr", 'RODBC', "RSQLite", "psych", 'tidyverse')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); sapply(list.of.packages,require,character.only=TRUE, quietly = TRUE)
rm(list.of.packages, new.packages)

# Read in Folder of Custom Functions --------------------------------------
for (i in list.files('./src/')) {
  source(paste0('./src/',i))
  rm(i)
}

# Import product offerings ------------------------------------------------
exam_table <- read.csv(file = "data/config/exam_table.csv")  

# Launch the app ----------------------------------------------------------
runApp(appDir = getwd(),
	   launch.browser = TRUE)

