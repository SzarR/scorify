server <- function(input, output, session) {
  
  list.of.packages <- c("shiny","haven","DT","shinythemes","dplyr","xlsx","rJava","stringdist", "magrittr",
                        "tabulizer","lubridate","tm","stringr","tools","labelled", "knitr", "kableExtra",
                        "extrafont", "RODBC", "DBI", "dbplyr", 'RODBC', "RSQLite", "psych", 'tidyverse')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages); sapply(list.of.packages,require,character.only=TRUE, quietly = TRUE)
  rm(list.of.packages, new.packages)
  
  library(rJava)
  setwd('/Users/robs/r-lang/ExamineR')
  # Read in Folder of Custom Functions --------------------------------------
  for (i in list.files('./src/')) {
    source(paste0('./src/',i))
    rm(i)
  }
  
  # Import product offerings ------------------------------------------------
  exam_table <- read.csv(file = "./supp/exam_table.csv")  
  
  #Larger dataset upload limit rule.
  options(shiny.maxRequestSize = 30 * 1024 ^ 2) 
  
# # Automate exam selection based on uploaded file --------------------------
#   observeEvent(input$TOAST, {
#     
#     #Auto-Detect NFSI
#     if (all(paste0("c", 1:105) %in% names(datum())) &
#         all(paste0("p", 1:50) %in% names(datum()))) {
#       updateSelectInput(session,
#                         'Exam',
#                         choices = unique(exam_table$Exam),
#                         selected = "NFSI")
#     }
# 
#     #Auto-Detect NCJOSI2
#     if (all(paste0("c", 1:80) %in% names(datum())) &
#         all(paste0("i", 1:120) %in% names(datum()))) {
#       updateSelectInput(session,
#                         'Exam',
#                         choices = unique(exam_table$Exam),
#                         selected = "NCJOSI2")
#     }
# 
#     #Auto-Detect LST + SBSA Not Scored
#     if (all(paste0("c", 1:70) %in% names(datum())) &
#         all(paste0("b", 1:120) %in% names(datum())) &
#         "s011" %in% names(datum()) &
#         !"SBSA" %in% names(datum())) {
#       updateSelectInput(session,
#                         'Exam',
#                         choices = unique(exam_table$Exam),
#                         selected = "LST + SBSA")
#     }
#     
#     #Auto-Detect LST + SBSA Scored
#     if(all(paste0("c", 1:70) %in% names(datum())) &
#        all(paste0("b", 1:120) %in% names(datum())) &
#        "s011" %in% names(datum()) &
#        "SBSA" %in% names(datum())){
#       updateSelectInput(session, 'Exam', choices = unique(exam_table$Exam), selected = "LST + SBSA")
#     }
# 
#     #Auto-Detect LST Only
#     if(all(paste0("c", 1:70) %in% names(datum())) &
#        all(paste0("b", 1:120) %in% names(datum())) &
#        !"s011" %in% names(datum())){
#       updateSelectInput(session, 'Exam', choices = unique(exam_table$Exam), selected = "LST")
#     }
#     
#     #Auto-Detect FST Only
#     if (all(paste0("c", 1:80) %in% names(datum())) &
#         all(paste0("b", 1:120) %in% names(datum())) &
#         !"s011" %in% names(datum())) {
#       updateSelectInput(session,
#                         'Exam',
#                         choices = unique(exam_table$Exam),
#                         selected = "FST")
#     }
#     
#     #Auto-Detect NGLE
#     if (all(paste0("c", 1:107) %in% names(datum())) &
#         !"c108" %in% names(datum())) {
#       updateSelectInput(session,
#                         'Exam',
#                         choices = unique(exam_table$Exam),
#                         selected = "NG LE")
#     }
# 
#     #Auto-Detect NGFF
#     if (all(paste0("c", 1:118) %in% names(datum())) &
#         !"c119" %in% names(datum())) {
#       updateSelectInput(session,
#                         'Exam',
#                         choices = unique(exam_table$Exam),
#                         selected = "NG FF")
#     }
#     
#     #Auto-Detect FST + SBSA
#     if (all(paste0("c", 1:80) %in% names(datum())) &
#         all(paste0("b", 1:120) %in% names(datum())) &
#         "s011" %in% names(datum())) {
#       updateSelectInput(session,
#                         'Exam',
#                         choices = unique(exam_table$Exam),
#                         selected = "FST + SBSA")
#     }
# 
#     #Auto-Detect SBSA LEO
#     if (grepl("LEO", input$TOAST[1]) &
#         !"b1" %in% names(datum())) {
#       updateSelectInput(session,
#                         'Exam',
#                         choices = unique(exam_table$Exam),
#                         selected = "SBSA LEO")
#     }
#   }) #auto-detect ender.

    #Returns only forms for the examination selected in first drop-down menu.
  observeEvent(input$Exam, {
    updateSelectInput(session, 'Form',
                      choices = unique(exam_table$Form[exam_table$Exam == input$Exam]))
  })

    #Returns scoring programs for the form selected in second drop-down menu.
  observeEvent(input$Form, {
    updateSelectInput(session, 'SP',
                      choices = unique(exam_table$SP[exam_table$Exam == input$Exam &
                                                       exam_table$Form == input$Form]))
  })
  
  #Returns value that is 100 - the weight that the user selected for SBSA_weight.
  observeEvent(input$LST_weight, {
    updateSliderInput(session, 'SBSA_weight', value = (100 - input$LST_weight))
  })

  #Returns a value that is 100 - the weight that the user selected for LST_weight.
  observeEvent(input$SBSA_weight, {
    updateSliderInput(session, 'LST_weight', value = (100 - input$SBSA_weight))
  })

# Data Read In Code -------------------------------------------------------
  datum <- reactive({
    
      # Do when nothing uploaded.
      if(is.null(input$TOAST)) return(NULL)
    
      # Read in SPSS file from Browse.
      haven::read_sav(input$TOAST$datapath) %>% 
      
      # Recode demographics
      demo_recoder() %>%
      
      # Coerce ID.
      mutate(ID = as.character(ID)) %>%
      
      # Coerce Names.  
      mutate(LName = toupper(LName),
             FName = toupper(FName))
  })
  
  psr_data <- reactive({
    
    # Do when nothing uploaded.
    if (is.null(input$PSR_ADMIN)) return(NULL)
    
    # Read in XLSX file.
    xlsx::read.xlsx(
      input$PSR_ADMIN$datapath,
      sheetIndex = 1,
      colClasses = "character",
      header = TRUE
    ) %>%
      
      # Coerce ID.
      mutate(ID = as.character(ID)) %>%
      
      # Coerce Names.
      mutate(LastName = toupper(LastName),
             FirstName = toupper(FirstName))
  })
  
# Rendering Various Tables ------------------------------------------------

  # Raw Data Table.
  output$table_raw <-
    DT::renderDataTable({
      datum()
    }, 
    style = "bootstrap", 
    server = TRUE, 
    options = list(pageLength = 25, 
                   autoWidth = TRUE))

  # SQL FDLE Table on FDLE Tab.
  output$table_raw_FDLE <-
    DT::renderDataTable({
      req(input$FDLE_Fetch)
      Cobra()
    }, style = "bootstrap",
    server = TRUE,
    options = list(pageLength = 25, 
                   autoWidth = TRUE))
  
  # Table 2 on SQL FDLE Tab.
  output$table_scored_FDLE <-
    DT::renderDataTable({
      req(input$FDLE_Validate)
      Eagle()
    },
    style = "bootstrap", 
    server = TRUE, 
    options = list(pageLength = 25, 
                   autoWidth = TRUE))  
  
  # Scored Data Table.
  output$table_scored <-
    DT::renderDataTable({
      Gorilla()
    },
    style = "bootstrap", 
    server = TRUE, 
    options = list(pageLength = 25, 
                   autoWidth = TRUE))
  
  # Merged Table.
  output$table_merged <-
    DT::renderDataTable({
      Zebra()
    }, 
    style = "bootstrap", 
    server = TRUE, 
    options = list(pageLength = 25, 
                   autoWidth = TRUE))
 
# Name/PW/Date Manipulation ---------------------------------------------------

  # Set Names/Passwords/DateofAdmin Variables.
  # Auto-runs after SPSS file upload on first tab.
  observeEvent(input$TOAST,{
    
      # City is full original filename with .sav ext.
      city <<- paste(input$TOAST)[1]
      
      # Extract city acronym for password setting.
      Name <<- get_name_abbrev(city)

      updateTextInput(session,
                      inputId = "PW",
                      label = "Set Password for Excel",
                      value = Name)
      
      # Write PW to clipboard.
      clipr::write_clip(content = Name)
      
      # Agency Name for Reporting.
      File_Name2 <<- get_name(city)
      
      # Extract Date of Administration.
      DateOfAdmin <<- get_date(city)
})

# Data Download Parameters ------------------------------------------------
  # Result from clicking the Download File.
  output$downloadData <- downloadHandler(
    filename = function() {
      switch(
        input$ExporterFormat,
        SPSS = paste0(gsub(
          c(".sav"), "", input$TOAST, ignore.case = TRUE
        ), ".sav"),
        XLSX = paste0(gsub(
          c(".sav"), "", input$TOAST, ignore.case = TRUE
        ), ".xlsx"),
        CSV = paste0(gsub(
          c(".sav"), "", input$TOAST, ignore.case = TRUE
        ), ".csv")
      )
    },
    
    content = function(file) {
      switch(
        input$ExporterFormat,
        
        SPSS = {
          if (is.null(psr_data())) {
            outs <- Gorilla()
          }
          if (!is.null(psr_data())) {
            outs <- Zebra()
          }
          haven::write_sav(data = outs, path = file)
        },
        
        CSV = {
          if (is.null(psr_data())) {
            outs <- Gorilla()
          }
          if (!is.null(psr_data())) {
            outs <- Zebra()
          }
          write.csv(x = outs, file = file, sep = ",")
        },
        
        XLSX = {
          if (is.null(psr_data())) {
            outs <- Gorilla()
          }
          if (!is.null(psr_data())) {
            outs <- Zebra()
          }
          Scores_Workbook <- xlsx::createWorkbook(type = "xlsx")
          
          # Styles for the data table row/column names
          TABLE_ROWNAMES_STYLE <-
            CellStyle(Scores_Workbook) + Font(Scores_Workbook, isBold = TRUE) +
            Alignment(horizontal = "ALIGN_CENTER") + Border(
              color = "black",
              position = c("TOP", "BOTTOM", "LEFT", "RIGHT"),
              pen = c("BORDER_THIN")
            )
          
          TABLE_COLNAMES_STYLE <-
            CellStyle(Scores_Workbook) + Fill(foregroundColor = "dodgerblue4") +
            Font(
              Scores_Workbook,
              isBold = TRUE,
              name = "Calibri",
              color = "azure"
            ) +
            Alignment(wrapText = FALSE, horizontal = "ALIGN_CENTER") +
            Border(
              color = "lightgrey",
              position = c("TOP", "BOTTOM", "LEFT", "RIGHT"),
              pen = c(
                "BORDER_THIN",
                "BORDER_THICK",
                "BORDER_THIN",
                "BORDER_THIN"
              )
            )
          
          ROWS <-
            CellStyle(Scores_Workbook) + Font(wb = Scores_Workbook,
                                              name = "Calibri",
                                              heightInPoints = 10) +
            Alignment(horizontal = "ALIGN_CENTER",
                      wrapText = FALSE,
                      vertical = "VERTICAL_CENTER") +
            Border(
              color = "black",
              position = c("TOP", "LEFT", "RIGHT", "BOTTOM"),
              pen = c("BORDER_THIN")
            )
          
          Results <-
            xlsx::createSheet(wb = Scores_Workbook, sheetName = "Results")
          
          dfColIndex           <- rep(list(ROWS), dim(outs)[2])
          names(dfColIndex)    <- seq(1, dim(outs)[2], by = 1)
          
          addDataFrame(
            x = outs,
            sheet = Results,
            startRow = 1,
            startColumn = 1,
            colStyle = dfColIndex,
            showNA = FALSE,
            colnamesStyle = TABLE_COLNAMES_STYLE,
            rownamesStyle = TABLE_ROWNAMES_STYLE
          )
          
          createFreezePane(sheet = Results,
                           colSplit = 1,
                           rowSplit = 2)
          autoSizeColumn(sheet = Results, colIndex = 1:ncol(outs))
          
          if (input$PW != "") {
            saveWorkbook(wb = Scores_Workbook,
                         file = file,
                         password = input$PW)
            
          } else {
            saveWorkbook(wb = Scores_Workbook, file = file)
          }
        }
      )
    }
  )

# Merge Error Check Code -----------------------------------------------------

  # Run if nrow mismatch.
  output$QC_nrow <- renderText({
    if (is.null(psr_data()))
      return(NULL)
    if (is.null(Gorilla()))
      return(NULL)
    if (nrow(psr_data()) != nrow(Gorilla())) {
      paste(
        "QC Check 1: The SPSS data set has a total of",
        nrow(Gorilla()),
        "rows.",
        "The PSR Admin data set has a total of",
        nrow(psr_data()),
        "rows."
      )
    }
  })

# Run if ID not unique in XLSX.
  output$QC_unique_Demo <- renderText({
    if (is.null(psr_data()))
      return(NULL)
    if (is.null(Gorilla()))
      return(NULL)
    if (nrow(psr_data() %>% count(ID) %>% filter(n > 1)) != 0) {
      c(
        paste(
          "QC Check 2: PSR data set contains",
          nrow(psr_data() %>% count(ID) %>% filter(n > 1)),
          "duplicate ID(s):"
        ),
        psr_data() %>% count(ID) %>% filter(n > 1) %>% pull(as.numeric(ID))
      )
    }
  })

# Run if ID not unique in SPSS.
  output$QC_unique_SPSS <- renderText({
    if (is.null(psr_data()))
      return(NULL)
    if (is.null(Gorilla()))
      return(NULL)
    if (nrow(Gorilla() %>% count(ID) %>% filter(n > 1)) != 0) {
      c(
        paste(
          "QC Check 2: SPSS data set contains",
          nrow(Gorilla() %>% count(ID) %>% filter(n > 1)),
          "duplicate ID(s):"
        ),
        Gorilla() %>% count(ID) %>% filter(n > 1) %>% pull(as.numeric(ID))
      )
    }
  })

# Run if ID mis-match with PSR as master.
  output$QC_NoMatch_Demo <- renderText({
    if (is.null(psr_data()))
      return(NULL)
    if (is.null(Gorilla()))
      return(NULL)
    if (nrow(psr_data() %>% anti_join(Gorilla(), by = "ID")) != 0) {
      paste(
        "QC Check 3: PSR data set contains",
        nrow(psr_data() %>% anti_join(Gorilla(), by = "ID")),
        "ID numbers that do not match with the SPSS data set. These are:",
        anti_join(Gorilla(), psr_data()) %>% pull(ID)
      )
    }
  })

# Run if ID mis-match with SPSS as master.
output$QC_NoMatch_SPSS <- renderText({
  if (is.null(psr_data()))
    return(NULL)
  if (is.null(Gorilla()))
    return(NULL)
  if (nrow(Gorilla() %>% anti_join(psr_data(), by = "ID")) != 0) {
    paste(
      "QC Check 3: SPSS data set contains",
      nrow(Gorilla() %>% anti_join(psr_data(), by = "ID")),
      "ID numbers that do not match with the PSR data set. These are",
      anti_join(psr_data(), Gorilla()) %>% pull(ID)
    )
  }
})

# Run if Missing IDs in PSR data.
output$QC_NA_Demo <- renderText({
  if (is.null(psr_data()))
    return(NULL)
  if (is.null(Gorilla()))
    return(NULL)
  if (sum(is.na(psr_data()[, "ID"])) > 0) {
    paste("QC Check 4: The PSR data set contains",
          sum(is.na(psr_data()[, "ID"])),
          "missing ID numbers.")
  }
})

# Run if Missing IDs in PSR data.
output$QC_NA_SPSS <- renderText({
  if (is.null(psr_data()))
    return(NULL)
  if (is.null(Gorilla()))
    return(NULL)
  if (sum(is.na(Gorilla()[, "ID"])) > 0) {
    paste("QC Check 4: The SPSS data set contains",
          sum(is.na(Gorilla()[, "ID"])),
          "missing ID numbers.")
  }
})

# Create Exam/Form Global Variables -------
observeEvent(input$Exam, {
  Examination <<- input$Exam
})

observeEvent(input$Form, {
  Form <<- input$Form
})

observeEvent(input$Verbose, {
  Verbose <<- input$Verbose
})

# Merging PSR + WE Data Together ------------------------------------------
#Zebra is the resulting df from merging spss and excel data for PSR administration purposes.
  Zebra <- eventReactive(input$MergeIt, {
    Demographics <<- psr_data() #turning it to be non-reactive
    ScoredData <<- Gorilla() #turning it to be non-reactive

    #check if IDs are unique
    DemoUnique <- Demographics %>% count(ID) %>% filter(n>1)
    DataUnique <- ScoredData %>% count(ID) %>% filter(n>1)

    #anti_join drops all observations in x that have a match in y.
    NoMatchDemo <- Demographics %>% anti_join(ScoredData, by = "ID")
    NoMatchData <- ScoredData %>% anti_join(Demographics, by = "ID")

    #check for missing IDs
    NADemos <- sum(is.na(Demographics[,"ID"]))
    NAData <- sum(is.na(ScoredData[,"ID"]))

    if(nrow(DemoUnique) == 0 &
       nrow(DataUnique) == 0 &
       nrow(NoMatchDemo) == 0  &
       nrow(NoMatchData) == 0 &
       NADemos == 0 &
       NAData == 0 &
       input$Booster == FALSE) {
      left_join(Demographics, ScoredData, by = input$Merger_Var) %>%
        arrange_at(.funs = desc, vars(as.vector(exam_table$Final_Score[exam_table$Exam == input$Exam & exam_table$Form == input$Form & exam_table$SP == input$SP])))
    }

    else if (nrow(DemoUnique) == 0 &
        nrow(DataUnique) == 0 &
        nrow(NoMatchDemo) == 0  &
        nrow(NoMatchData) == 0 & NADemos == 0 & NAData == 0
        & input$Booster == TRUE) {
      left_join(Demographics, ScoredData, by = input$Merger_Var) %>%
        mutate(dist_LName = stringdist::stringdist(LastName, LName),
               dist_FName = stringdist::stringdist(FirstName, FName)) %>%
        arrange_at(.funs = desc, vars(as.vector(exam_table$Final_Score[exam_table$Exam == input$Exam & exam_table$Form == input$Form & exam_table$SP == input$SP])))
    }
  })

# Scoring -----------------------------------------------------------------

# Gorilla is df of scored dataset.
  Gorilla <- eventReactive(input$ScoreIt, {
    req(input$TOAST)

    withProgress(message = 'Importing Dataset',value = 0, {
    
    # NCJOSI^2.
    if (input$Exam=="NCJOSI2" & input$SP=="2019") {
      source("./supp/NCJ2_2019.R", local=TRUE)
      print_scored(TOAST)
    }

    # NFSI
    else if (input$Exam=="NFSI" & input$SP == "2019") {
      source("./supp/NFSI_2019.R", local=TRUE)
      print_scored(TOAST)
    }

    # LST.
    else if (input$Exam=="LST") {
      source("./supp/LST.R", local=TRUE)
      print_scored(TOAST)
    }

    # FST.
    else if (input$Exam=="FST") {
      source("./supp/FST.R", local=TRUE)
      print_scored(TOAST)
    }

    # LST+SBSA Scored.
    else if (input$Exam=="LST + SBSA" & input$Form=="A/B/C/D"){
      source("./supp/LST+SBSA_Scored.R", local=TRUE)
      print_scored(TOAST)
    }

    # FST+SBSA Scored.
    else if (input$Exam=="FST + SBSA" & input$Form=="A/B/C/D"){
      source("./supp/FST+SBSA_Scored.R", local=TRUE)
      print_scored(TOAST)
    }

    # SBSA-FF.
    else if (input$Exam=="SBSA FF"){
      source("./supp/SBSA_FF.R", local=TRUE)
      print_scored(TOAST)
    }

    # SBSA-LEO.
    else if (input$Exam=="SBSA LEO"){
      source("./supp/SBSA_LEO.R", local=TRUE)
      print_scored(TOAST)
    }

    # NG LE.
    else if (input$Exam=="NG LE"){
      source("./supp/NGLEv2.R", local=TRUE)
      print_scored(TOAST)
    }

    # NCJOSI2 + SBSA Scored.
    else if (input$Exam=="NCJOSI2 + SBSA"){
      source("./supp/NCJ2+SBSA_Scored.R", local=TRUE)
      print_scored(TOAST)
    }

    # LST + SBSA Scored.
    else if (input$Exam=="LST + SBSA" & input$Form != "A/B/C/D"){
      source("./supp/LST.R", local=TRUE)
      source("./supp/SBSA_LEO.R", local=TRUE)
      source("./supp/LST+SBSA_Scored.R", local=TRUE)
      print_scored(TOAST)
    }

    # FST + SBSA Scored.  
    else if (input$Exam=="FST + SBSA" & input$Form != "A/B/C/D"){
      source("./supp/FST.R", local=TRUE)
      source("./supp/SBSA_FF.R", local=TRUE)
      source("./supp/FST+SBSA_Scored.R", local=TRUE)
      print_scored(TOAST)
    }
    
    # NG FF.  
    else if (input$Exam == "NG FF" & input$Form == "Form A") {
      source("./supp/NGFFv2.R", local = TRUE)
      print_scored(TOAST)
    }
}) # Progress closer.
  }) # Gorilla closer. 

# Creating Impact Reports -------------------------------------------------
# Reporting module - This section of code will be responsible for 
# all aspects associated with running a PDF air report in shiny.

output$DFR_IR = downloadHandler(
  filename = paste(File_Name2, "Impact Report", DateOfAdmin,".pdf"),
  content = function(file) {
    TOAST <<- Gorilla()
    #TOAST <<- rename_(TOAST, Shiny_Status = input$Status_Var)
    #TOAST <<- rename_(TOAST, Shiny_Final_Score = input$SMD_Var)
    out = knit2pdf('G:\\IOSolutions\\Research and Development\\ExamineR\\supp\\AIR_report.Rnw', clean = TRUE, envir = globalenv())
    file.rename(out, file)
  },
  contentType = 'application.pdf'
)

# FDLE SQL Code -------------------------------------------------------
Cobra <- eventReactive(eventExpr = input$FDLE_Fetch,{
  source("G:\\IOSolutions\\Research and Development\\ExamineR\\supp\\sql_connection.R",local = TRUE)
  datum_FDLE
})

Eagle <- eventReactive(eventExpr = input$FDLE_Validate,{
  source("G:\\IOSolutions\\Research and Development\\ExamineR\\supp\\CJBAT_v2.R",local = TRUE)
  
  datum_FDLE_Scored <- datum_FDLE_Scored %>%
    select(RegistrationID, Form, ExamSeriesCode,ExamDate,Race,Gender, cog_items, integ_items, Score_PS3, Score_R, Status_PS3, Status_R) %>%
    mutate(Score_Diff = abs(round(Score_R - Score_PS3,digits = 3)))
  
  datum_FDLE_Scored
})

output$SQL_Summary <-
  renderTable({
    df <- data.frame(matrix(NA,nrow=2, ncol = 2))
    names(df) <- c("Description","Value")
    df[1,1]   <- paste("Number of Candidates in Dataset")
    df[1,2]   <- nrow(Eagle())
    df[2,1]   <- paste("Number of Score Mismatches")
    df[2,2]   <- Eagle() %>% filter(Score_Diff > 0) %>% nrow()
    df[3,1]   <- paste("Number of Pass Status Mismatches")
    df[3,2]   <- Eagle() %>% filter(Status_PS3 != Status_R) %>% nrow()
    df
  })

# Creating FDLE Reports -------------------------------------------------
#Reporting module - This section of code will be responsible for all aspects associated with running a PDF air report in shiny.
output$AIR_Report_FDLE = downloadHandler(
  filename = paste("FDLE", "Impact Report", Sys.Date(),".pdf"),
  content = function(file) {
    TOAST <<- Cobra()
    out = knit2pdf('G:\\IOSolutions\\Research and Development\\ExamineR\\supp\\FDLE_Air_Reporting.Rnw', clean = TRUE,envir = globalenv())
    file.rename(out, file)
  },
  contentType = 'application.pdf'
)

# Data Download Parameters ------------------------------------------------
# Result from clicking the Download File on FDLE Tab.
output$FDLE_Export_XLSX <-   downloadHandler(
  filename = function() { "FDLE_Data.xlsx"},
  content = function(file) {
    fdle_outs <- Eagle()
    fdle_raw_outs <- Cobra()
    xlsx::write.xlsx(x = fdle_outs,file = file,sheetName = "Score Check")
    xlsx::write.xlsx(x = fdle_raw_outs, file = file, sheetName = "Raw Data",append = TRUE)}
)

# SQL Interaction for PS3 Data Upload --------------------------------------

# Connect to SQL for PS3 Customer File Upload Purposes
observeEvent(eventExpr = input$SQL_Connector, {
  con2 <<- dbConnect(odbc::odbc(),
                   driver = "{SQL Server}",
                   server = "172.24.16.19",
                   database = "PS3QA",
                   uid = "rszarek",
                   pwd = "fDAD4yz#")
  
  PS3_ClientList <<-
    tbl(con2, in_schema(schema = "dbo", table = "Survey_Customer")) %>%
    select(ID, Name) %>% 
    filter(ID != 1014 & ID != 1228 & ID != 2664 & ID != 2635) %>% #problem ID
    collect() 
})

# Populate client list after connecting with SQL database
observeEvent(input$SQL_Connector, {
    updateSelectInput(session, inputId = 'PS3ClientName', choices = PS3_ClientList$Name)
  })

# Executor after clicking on the "Upload Data to PS3" Button on Exporting.
observeEvent(eventExpr = input$UploadTo_PS3, {

  # Extract Client Customer ID as SurveyID from the table above.
  # 42 I/O In House Testing
  TOAST_SQL[,"CustID"] <- PS3_ClientList %>% filter(Name == input$PS3ClientName) %>% select(ID) %>% pull()
 
  # Set source to E to tell IT/PS3 that this was a ShinyR Upload.
  TOAST_SQL[,"Source"] <- c("E")

  DBI::dbWriteTable(conn = con2, name = "Survey_User", value = TOAST_SQL, append =TRUE)
})

} #ender for server.