server <- function(input, output, session) {
  
  #Returns only forms for the examination selected in first drop-down menu.
  observeEvent(input$Exam, {
    updateSelectInput(session, 'Form', choices = unique(exam_table$Form[exam_table$Exam == input$Exam]))
  })
  
  #Returns scoring programs for the form selected in second drop-down menu.
  observeEvent(input$Form, {
    updateSelectInput(session, 'SP', choices = unique(exam_table$SP[exam_table$Exam == input$Exam &
                                                                      exam_table$Form == input$Form]))
  })
  
  #Returns value that is 100 - the weight that the user selected for SBSA_weight.
  observeEvent(input$LST_weight, {
    updateSliderInput(session, 'PERS_Weight', value = (100 - input$Aviator_weight))
  })
  
  #Returns a value that is 100 - the weight that the user selected for LST_weight.
  observeEvent(input$SBSA_weight, {
    updateSliderInput(session, 'Aviator_weight', value = (100 - input$PERS_weight))
  })
  
# Data Read In Code -------------------------------------------------------
  datum <- reactive({
    
      # Do when nothing uploaded.
      if(is.null(input$TOAST)) return(NULL)
    
      readr::read_csv(input$TOAST$datapath) |> 
      
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
    
    # Aviator
    if (input$Exam=="Aviator" & input$SP=="2019") {
      source("./scoring/Aviator_2019.R", local=TRUE)
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

}) # Progress closer.
  }) # Gorilla closer. 

} #ender for server.