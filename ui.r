ui <- navbarPage(title = tags$strong("Scorify"), selected = "Importing", windowTitle = "Scorify",
                 tabPanel(tags$strong(h3("Dataset Upload Module")), title = "Importing",
                          sidebarLayout(
                            sidebarPanel("Please upload a valid file below. 
                                         Shiny will pre-fill the drop-down
                                         menus based upon the file that was 
                                         uploaded. If ExamineR fails to do 
                                         so after uploading a file, please 
                                         select the applicable examination
                                         and form from the drop down menus.", 
                              rep(br(), 2),
                              fileInput(inputId = "TOAST", 
                                        label = "", 
                                        accept = ".csv",
                                        width = 475, 
                                        placeholder = "Upload a Raw CSV file"),
                              selectInput(inputId = "Exam", 
                                          label=h5("Select Examination:"), 
                                          choices = unique(exam_table$Exam), 
                                          selected = NULL, 
                                          width = 190, 
                                          selectize = TRUE),
                              selectInput(inputId = "Form", 
                                          label=h5("Select Form:"), 
                                          choices = NULL, 
                                          width = 120)
                            ), 
                            mainPanel(DT::dataTableOutput("table_raw"))
                          )),
                 
                 tabPanel(tags$strong(h3("Scoring Module")), title="Scoring",
                          sidebarLayout(
                            sidebarPanel("Please select the appropriate 
                                          scoring program from the drop 
                                          down list below. If an examination 
                                          has more than one valid scoring program, 
                                         it will be present in the drop down box. 
                                         Not all examinations have the same 
                                         scoring programs available.",
                              selectInput(inputId = "SP", 
                                          label=h5("Scoring Program:"), 
                                          choices = NULL,
                                          width = 120
                                          ),
                              conditionalPanel(condition = "input.Exam == 'Aviator + Personality' |
                                                            input.Exam == 'HeliTest + Personality'",
                              sliderInput(inputId = "Aviator_weight",
                                          label = h5("Set Aviator/HeliTest Weight:"), 
                                          min = 10, 
                                          max = 90, 
                                          value = 60, 
                                          step = 5),
                              sliderInput(inputId = "PERS_weight",
                                          label = h5("Set Personality Weight:"), 
                                          min = 10, 
                                          max = 90, 
                                          value = 40, 
                                          step = 5)), 
                              sliderInput(inputId = "cut_point",
                                          label = h5("Set Cut Point:"),
                                          min = 50, 
                                          max = 90, 
                                          value = 70, 
                                          step = 1),
                              checkboxInput(inputId = "Verbose", 
                                            label = h5(("Output all variables?")), 
                                            value = FALSE,
                                            width = 1200),
                              conditionalPanel(condition = "input.Exam == 'NG FF'",
                              checkboxInput(inputId = "Run_Report", 
                                            label = h5(("Auto-run reporting?")))),
                              actionButton(inputId = "ScoreIt",
                                           label = "Run Scoring",
                                           icon("paper-plane"), 
                                           width = 150,
                                           style="color: #fff; background-color: #df691a; border-color: #2e6da4")
                            ),
                            mainPanel(DT::dataTableOutput("table_scored"))
                          )),
                 
                 tabPanel(tags$strong(h3("Merging Module")), title="Merging",
                          sidebarLayout(
                            sidebarPanel("Typically for PSR scoring, a merge with an external excel file is needed before scores can be delivered to the client. In such circumstances, this module
                                         will help the user perform an accurate merge. Certain assumptions must be met before the merge will complete. These are outlined in the steps below.",br(),br(),
                                         textInput(inputId = "Merger_Var",label = h5("Variable to Merge On:"),value = "ID",width = 170),
                                         fileInput(inputId = "PSR_ADMIN", label = "", accept = ".xlsx", width = 450,placeholder = "Upload a valid XLSX file"),
                                         h4("Merging Criteria Assumptions"),
                                         "Several parameters need to be met concerning both of the data files in order for a successful merge to occur. These include:", br(), br(),
                                         "> Both data files must contain the same number of rows.",br(),
                                         "> Neither data file can have any missing values (NA) on the key merging variable.", br(),
                                         "> Neither data file can have an ID number that occurs more than once.", br(),
                                         "> Each ID number must possess a positive match in the other data file.", br(), br(),
                                         "If all the above criteria have been met, a merge will occur by clicking the button below resulting in a data table 
                                         appearing on the right side of this page.", 
                                         br(),
                                         h4("Boosting your Join"),
                                         "Boosting is a form of using last and first names to ascertain the degree of similarity
                                         between the two datasets. By clicking on the icon below, the algorithm will evaluate
                                         the first and last names (if applicable) in both datasets and provide a dist_LName and
                                         dist_FName column in your dataset. A value of 0 indicates a perfect match between the
                                         name variable in both data sets.", br(),
                                         checkboxInput(inputId = "Booster", label = "Boost joining by First/Last names",value = FALSE),
                                         actionButton(inputId = "MergeIt",label = "Merge Data Files", width = 175,
                                                      style="color: #fff; background-color: #df691a; border-color: #2e6da4")
                                         
                            ),
                            mainPanel(DT::dataTableOutput("table_merged"),
                                      h4(textOutput("QC_nrow")),
                                      h4(textOutput("QC_unique_Demo")),
                                      h4(textOutput("QC_unique_SPSS")),
                                      h4(textOutput("QC_NoMatch_Demo")),
                                      h4(textOutput("QC_NoMatch_SPSS")),
                                      h4(textOutput("QC_NA_Demo")),
                                      h4(textOutput("QC_NA_SPSS")))
                          )),
                 
                 tabPanel(title = "Reporting", tags$strong(h3("Impact Reporting Module")), 
                          br(),
                          "The reporting module provides the user with the ability to generate a PDF formatted impact report that is exam irrelevant.",br(),br(),
                          "Since it is exam irrelevant, the user must specify two critical variables below, the Passing Variable and the Final Score variable.",br(),
                          selectInput(inputId = "Status_Var",label = h5("Specify Pass/Fail Variable:"),choices = NULL,multiple = FALSE,width = 225),
                          selectInput(inputId = "SMD_Var",label = h5("Specify Score Variable:"),choices = NULL, multiple = FALSE, width = 225),
                          "Several assumptions concerning the dataset must be met before a PDF report can be compiled. These include:",br(),
                          "1. A Race variable where 6 = White.",br(),
                          "2. A Passing variable where 1 = Pass, and 0 = Did Not Pass.",br(),
                          "3. Ensure you have set the proper variables in the drop down menus above.", 
                          br(), br(),
                          downloadButton('DFR_IR', 'Generate Impact Report',
                                         style="color: #fff; background-color: #df691a; border-color: #2e6da4", width = 190),
                          br(), br()
                 ),

                 tabPanel(title = "Exporting", tags$strong(h3("Exporting Module")), 
                          br(),
                          "This module provides an interface to be able to download a client-ready data file containing the scoring procedures that",br(),"
                          were conducted in the previous tabs of this application. Also, this module provides an interface with the PS3 SQL server", br(),"
                          to allow clients to access score results from their individual PS3 account.", 
                          br(), br(),
                          h4("Download Scored File to Your Computer"),
                          textInput(inputId = "PW",label = h5("Set Password for Excel"),value = NULL, width = 170),
                          selectInput(inputId = "ExporterFormat", label="Select File Format", choices = c("SPSS","CSV", "XLSX"), selected = "XLSX", width = 150),
                          downloadButton('downloadData', 'Download File',
                                         style="color: #fff; background-color: #df691a; border-color: #2e6da4", width = 125), br(), br()
                          ),
                 
                 tabPanel(title = "Updates", tags$strong(h3("ExamineR Changes and Updates")), 
                          br(),
                          "This module provides the user with updates concerning this web application.", 
                          br(), br(),
                          "February 11, 2020", br(),
                          "Re-developed code for exam scoring. Added CWH NG FF v2 scoring, proofed by Reya. Also, feedback reporting and SQL integration complete with NGFFv2.",
                          br(), br(),
                          "November 21, 2019", br(),
                          "Optimized the NFSI + NCJOSI2 Scoring Programs. De-activated Analysis tab. Implemented SQL PS3QA upload of data for NFSI/NCJOSI2 exams.",
                          br(), br(),
                          "October 14, 2019", br(),
                          "For PSR files that were merged, implemented automatic sort on Final Score variable for that file.",
                          br(), br(),
                          "September 9, 2019", br(),
                          "NFSI Forms 1 - 5 scoring have been updated and finalized."
                 ),
                 inverse=TRUE, theme= shinythemes::shinytheme("superhero")
)
