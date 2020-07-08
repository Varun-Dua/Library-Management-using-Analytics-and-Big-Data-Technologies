library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyBS)
library(tools)
library(shinyalert)
library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(rpart)
library(rpart.plot)
library(caret)
library(caTools)
library(shinyWidgets)


# Define UI for application
ui <- dashboardPage(
  
  dashboardHeader(title = "Library Management", titleWidth = 300,
                  tags$li(a(href = 'http://www.library.nuigalway.ie/',
                            img(src = 'NUIG_Logo.png',
                                title = "Library Home", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  
  dashboardSidebar(width = 300, collapsed = TRUE, 
   sidebarMenu(
    id = "sidebarmenu-tabs",
    menuItem("Model Creation & Training - New Data", tabName = "modeltab-new", icon = icon("stream")),
    menuItem("Model Creation & Training - Existing Data", tabName = "modeltab_exist", icon = icon("folder-open")),
    menuItem("Demand Categorisation", tabName = "demandtab", icon = icon("dashboard")),
    menuItem("Dynamic Loan Period", tabName = "dynamicloantab", icon = icon("calendar"))
    )
  ),
  
  dashboardBody(
    
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "demandtab",
              h2("Demand-based Categorisation"),
              box(width=12,
                  tabBox(width=12,id="tabBox_demand",
                         tabPanel("Primary Demand",
                                  p("Library asset categorisation based on Requests & Recalls"),
                                  fluidRow(
                                    # High Demand
                                    column(4, align = "center",
                                           actionBttn("highdem_primary", label = "High Demand", icon = icon("book-reader"), style = "bordered", color = "success", size = "lg", block = TRUE)
                                           ),
                                    bsModal("highprim_modal", "High Demand Assets (based on Requests & Recalls)", "highdem_primary", size = "large",
                                            DT::dataTableOutput("highprim_table")),
                                    
                                    # Moderate Demand
                                    column(4, align = "center",
                                           actionBttn("moddem_primary", label = "Moderate Demand", icon = icon("book-open"), style = "bordered", color = "warning", size = "lg", block = TRUE)
                                    ),
                                    bsModal("modprim_modal", "Moderate Demand Assets (based on Requests & Recalls)", "moddem_primary", size = "large",
                                            DT::dataTableOutput("modprim_table")),
                                    
                                    #Low Demand
                                    column(4, align = "center",
                                           actionBttn("lowdem_primary", label = "Low Demand", icon = icon("book"), style = "bordered", color = "danger", size = "lg", block = TRUE)
                                    ),
                                    bsModal("lowprim_modal", "Low Demand Assets (based on Requests & Recalls)", "lowdem_primary", size = "large",
                                            DT::dataTableOutput("lowprim_table"))
                                  )
                         ),
                         tabPanel("Secondary Demand",
                                  p("Library asset categorisation based on Renewals & Auto-renewals"),
                                  fluidRow(
                                    # High Demand
                                    column(4, align = "center",
                                           actionBttn("highdem_sec", label = "High Demand", icon = icon("book-reader"), style = "bordered", color = "success", size = "lg", block = TRUE)
                                    ),
                                    bsModal("highsec_modal", "High Demand Assets (based on Renewals & Auto-renewals)", "highdem_sec", size = "large",
                                            DT::dataTableOutput("highsec_table")),
                                    
                                    # Moderate Demand
                                    column(4, align = "center",
                                           actionBttn("moddem_sec", label = "Moderate Demand", icon = icon("book-open"), style = "bordered", color = "warning", size = "lg", block = TRUE)
                                    ),
                                    bsModal("modsec_modal", "Moderate Demand Assets (based on Renewals & Auto-renewals)", "moddem_sec", size = "large",
                                            DT::dataTableOutput("modsec_table")),
                                    
                                    #Low Demand
                                    column(4, align = "center",
                                           actionBttn("lowdem_sec", label = "Low Demand", icon = icon("book"), style = "bordered", color = "danger", size = "lg", block = TRUE)
                                    ),
                                    bsModal("lowsec_modal", "Low Demand Assets (based on Renewals & Auto-renewals)", "lowdem_sec", size = "large",
                                            DT::dataTableOutput("lowsec_table"))
                                  )
                         )
                  )
              ),
              fluidRow(
                
                box(width = 12, align = "center",
                    DT::dataTableOutput("demandtable"),
                    tags$hr(),
                    downloadButton("downloaddemandreport", "Download Report")
                    )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "dynamicloantab",
              h2("Dynamic Loan Period for Library Asset"),
              box(width=12,
                  tabBox(width=12,id="tabBox_demand",
                         tabPanel("Requests & Recalls",
                                  uiOutput("dynamicslider_req"),
                                  DT::dataTableOutput("dynamictable_req")
                                  ),
                         tabPanel("Renewals & Auto-Renewals",
                                  uiOutput("dynamicslider_ren"),
                                  DT::dataTableOutput("dynamictable_ren")
                                  )
                  ),
                  tags$hr(),
                  downloadButton("downloadloanreport", "Download Report"), align = "center"
              )
      ),
      
      # Third tab content
      tabItem(tabName = "modeltab-new",
              shinyjs::useShinyjs(),
              
              # Tab header, info button & info button modal/pop-up
              fluidRow(column(12, div(style="display: inline-block;",tags$h2("Model Creation & Training")),
                              actionButton("infobutton", label = icon("info"), style = " color: #000000; background-color: #ffffff; border-color: #ffffff")
              )),
              
              bsModal("modalExample", "Why Model Creation & Training?", "infobutton", size = "large",
                      uiOutput("pdfview")),
              
              div(id = "new_data",
                  #Area for perform data merge and manipulation
                  div(id = "manipbox",
                      box(title = "Data Upload, Manipulation & Merge", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          fluidRow(
                            column(6,
                                   # Input: Select a file ----
                                   fileInput("file1", "Choose loan datasets XLSX File (multiple files of identical structure accepted):",
                                             multiple = TRUE,
                                             accept = ".xlsx")
                            ),
                            
                            column(6,
                                   fileInput("file2", "Choose requests dataset XLSX File (multiple files of identical structure accepted):",
                                             multiple = TRUE,
                                             accept = ".xlsx")
                            )
                            
                          ),
                          
                          fluidRow(
                            useShinyalert(),
                            shinyjs::useShinyjs(),
                            actionButton("performdataworks", "Perform Data Merge & Manipulations",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            align="center"
                          ),
                          
                          tags$hr(),    
                          
                          fluidRow(
                            column(12, align = "center",
                                   downloadButton("downloadmanipdata", "Download Manipulated & Merged Dataset")
                            )
                          ))),
                  
                  
                  # Horizontal line ----
                  tags$hr(),
                  
                  # Area for initiating clustering activity details
                  div(id = "clustering",
                      box(title = "Clustering", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          column(6,
                                 h5("WSS Elbow Chart for Requests & Recalls:"),
                                 withSpinner(plotOutput("plts_req"),6),
                                 column(6, selectInput("clustercount_req", h4("Select optimum number of clusters from elbow chart"), 
                                                       list(3,4,5,6,7,8,9,10,11,12,13,14)))
                          ),
                          
                          column(6, 
                                 h5("WSS Elbow Chart for Renewals & Auto-Renewals:"),
                                 withSpinner(plotOutput("plts_ren"),6),
                                 column(6, selectInput("clustercount_ren", h4("Select optimum number of clusters from elbow chart"), 
                                                       list(3,4,5,6,7,8,9,10,11,12,13,14)))
                          ),
                          
                          fluidRow(
                            actionButton("clusterbutton", "Run clustering algorithms",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),align="center",
                            useShinyalert(),
                            shinyjs::useShinyjs()
                          )
                      )),
                  
                  # Displaying cluster details
                  div(id = "cluster_assign",
                      box(title = "Category Assignment", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          column(6, align = "center",
                                 h5("Cluster Centres - Requests & Recalls:"),
                                 tableOutput("clusterdets_req"),
                                 tags$hr(),
                                 h5("Select demand category:"),
                                 uiOutput("assignment_req_demand"),
                                 tags$hr(),
                                 h5("Select dynamic loan level:"),
                                 uiOutput("assignment_req_dynamicloan")
                          ),
                          column(6, align = "center",
                                 h5("Cluster Centres - Renewals & Auto-Renewals:"),
                                 tableOutput("clusterdets_ren"),
                                 tags$hr(),
                                 h5("Select demand category:"),
                                 uiOutput("assignment_ren_demand"),
                                 tags$hr(),
                                 h5("Select dynamic loan level:"),
                                 uiOutput("assignment_ren_dynamicloan")
                          ),
                          tags$hr(),
                          fluidRow(
                            actionButton("assignmentbutton", "Perform category assignment",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),align="center",
                            useShinyalert(),
                          )
                          
                      )),
                  
                  # Area for uploading new dataset and performing classification ops
                  div(id = "classification",
                      box(title = "Classification", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          
                          useShinyjs(),
                          useShinyalert(),
                          
                          fluidRow(
                            actionButton("classificationbutton", "Run asset classification",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),align="center"
                          ),
                          tags$hr(),
                          useShinyjs(),
                          column(12,
                                 actionLink("viewdttrees", "View Generated Decision Trees"),
                                 align="center"
                          ),
                          
                          bsModal("dtmodal", 
                                  "Generated Decision Trees through Classification",
                                  "viewdttrees",
                                  size = "large",
                                  h5("Decision Tree for Requests and Recalls - Demand Categorisation"),
                                  withSpinner(plotOutput("plt_Dt1"),6),
                                  h5("Decision Tree for Renewals and Auto-Renewals - Demand Categorisation"),
                                  withSpinner(plotOutput("plt_Dt2"),6),
                                  h5("Decision Tree for Requests and Recalls - Dynamic Loan Period"),
                                  withSpinner(plotOutput("plt_Dt3"),6),
                                  h5("Decision Tree for Renewals and Auto-Renewals - Dynamic Loan Period"),
                                  withSpinner(plotOutput("plt_Dt4"),6)
                          )
                          
                      )
                  ),
                  
                  div(id = "labelassign",
                      box(title = "Assign Labels to New Data", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          fluidRow(
                            column(6,
                                   # Input: Select a file ----
                                   fileInput("finalfile1", "Choose loan dataset XLSX File (multiple files of same structure are permitted):",
                                             multiple = TRUE,
                                             accept = ".xlsx")
                            ),
                            
                            column(6,
                                   fileInput("finalfile2", "Choose request dataset XLSX File (multiple files of same structure are permitted):",
                                             multiple = TRUE,
                                             accept = ".xlsx")
                            )
                          ),
                          fluidRow(
                            useShinyalert(),
                            shinyjs::useShinyjs(),
                            actionButton("assignlabels", "Assign demand & dynamic loan labels",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            align="center"
                          )
                      )
                  )
                  
              )
            
              ),
      
      # Fourth tab content
      tabItem(tabName = "modeltab_exist",
              shinyjs::useShinyjs(),
              
                # Area for uploading new dataset and performing classification ops
                div(id = "classification_exist",
                    box(title = "Classification", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        
                        useShinyjs(),
                        useShinyalert(),
                        
                        fluidRow(
                          actionButton("classificationbutton_exist", "Run asset classification",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),align="center"
                        ),
                        tags$hr(),
                        useShinyjs(),
                        column(12,
                               actionLink("viewdttrees_exist", "View Generated Decision Trees"),
                               align="center"
                        ),
                        
                        bsModal("dtmodal-exist", 
                                "Generated Decision Trees through Classification",
                                "viewdttrees_exist",
                                size = "large",
                                h5("Decision Tree for Requests and Recalls - Demand Categorisation"),
                                withSpinner(plotOutput("plt_Dt1_exist"),6),
                                h5("Decision Tree for Renewals and Auto-Renewals - Demand Categorisation"),
                                withSpinner(plotOutput("plt_Dt2_exist"),6),
                                h5("Decision Tree for Requests and Recalls - Dynamic Loan Period"),
                                withSpinner(plotOutput("plt_Dt3_exist"),6),
                                h5("Decision Tree for Renewals and Auto-Renewals - Dynamic Loan Period"),
                                withSpinner(plotOutput("plt_Dt4_exist"),6)
                        )
                        
                    )
                ),
                
                div(id = "labelassign_exist",
                    box(title = "Assign Labels to New Data", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        fluidRow(
                          column(6,
                                 # Input: Select a file ----
                                 fileInput("finalfile1_exist", "Choose loan dataset XLSX File (multiple files of same structure accepted):",
                                           multiple = TRUE,
                                           accept = ".xlsx")
                          ),
                          
                          column(6,
                                 fileInput("finalfile2_exist", "Choose request dataset XLSX File (multiple files of same structure accepted):",
                                           multiple = TRUE,
                                           accept = ".xlsx")
                          )
                        ),
                        fluidRow(
                          useShinyalert(),
                          shinyjs::useShinyjs(),
                          actionButton("assignlabels_exist", "Assign demand & dynamic loan labels",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          align="center"
                        )
                    )
                )
              
              )
          )
  )
)
