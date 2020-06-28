options(shiny.maxRequestSize = 30*1024^2, browser = "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")
#runApp(list(ui = ui, server = server), launch.browser = TRUE)

# Define server logic
server <- function(input, output,session) {
  
  #Hide view on app launch
  shinyjs::hide("new_data")
  
  # Modal dialogue to select new or existing model
  data_modal <- modalDialog(
    title = "How do you want to run the category assignment operations?",
    selectInput('input_query','Select option',choices = c("Run Assignment on New Data" = "newdata", "Run Assignment on Existing Data" = "existingdata")),
    align = "center",
    easyClose = F,
    footer = tagList(
      actionButton("run", "Run")
    )
  )
  
  # Show the model on start up ...
   showModal(data_modal)
  
  # Creating Reactive values
  rv <- reactiveValues()
  rv$clusterdets1_DF <- NULL
  rv$clusterdets2_DF <- NULL
  
  # deactive elements from UI
  shinyjs::disable("downloadmanipdata")
  shinyjs::disable("clustering")
  shinyjs::hide("cluster_assign")
  shinyjs::hide("classification")
  shinyjs::hide("labelassign")
  
  # Show view on button click
  observeEvent(input$run,{
    removeModal()
    
    #Check entry
    if(input$input_query == "newdata"){
      
      shinyjs::show("new_data")
      
    } else {
      
      updateTabItems(session, "sidebarmenu-tabs", "modeltab_exist")
      shinyjs::show("new_data")
      shinyjs::hide("labelassign_exist")
      shinyjs::hide("viewdttrees_exist")
    }
  })
  
  # FAQ document view
  observeEvent(input$infobutton,{
    output$pdfview <- renderUI({
      pdf("ModelCreationInfoDoc1.pdf")
      tags$iframe(style="height:600px; width:100%", src="ModelCreationInfoDoc1.pdf")
    })
  })
  
  # Manipulation button after file uploads
  AggregateData <- eventReactive(input$performdataworks, {
    
    # Disabling button to prevent re-click
    shinyjs::disable("performdataworks")
    
    #Check if files are empty
    LoanFile <- input$file1
    ReqFile <- input$file2
    
    if (is.null(LoanFile) | is.null(ReqFile)) {
      
      shinyalert("Oops!", "Empty files", type = "error")
      shinyjs::enable("performdataworks")
      
    } else {
      
      withProgress(message = "Please wait", detail = "Uploading and transforming data files", value = 5, {
        
        LibraryLoanData_list = list()
        LibraryRequestData_list = list()
        
        for(i in 1:length(input$file1[,1])){
          LibraryLoanData_list[[i]] <- read.xlsx(LoanFile[[i, 'datapath']], fillMergedCells = TRUE, colNames = TRUE)
        }
        
        for(j in 1:length(input$file2[,1])){
          LibraryRequestData_list[[j]] <- read.xlsx(ReqFile[[j, 'datapath']], fillMergedCells = TRUE, colNames = TRUE)
        }
        
        # Binding data from different files to form one dataframe for further use
        LibraryLoanData <- bind_rows(LibraryLoanData_list)
        LibraryRequestData <- bind_rows(LibraryRequestData_list)
        
        ## LOAN DATASET ##
        # Changing format of date columns
        LibraryLoanData$Loan.Date <- as.Date(LibraryLoanData$Loan.Date, origin = "1899-12-30")
        LibraryLoanData$Due.Date <- as.Date(LibraryLoanData$Due.Date, origin = "1899-12-30")
        LibraryLoanData$Return.Date <- as.Date(LibraryLoanData$Return.Date, origin = "1899-12-30")
        
        # Removing NA entries in Patron Group and Loan Date
        LibraryLoanData <- LibraryLoanData %>% drop_na(Patron.Group)
        LibraryLoanData <- LibraryLoanData %>% drop_na(Loan.Date)
        
        # Recording month of loan for each entry in dataset
        LibraryLoanData$LoanMonth <- strftime(LibraryLoanData$Loan.Date, "%B")
        LibraryLoanData$DueMonth <- strftime(LibraryLoanData$Due.Date, "%B")
        
        # Subsetted dataset for further use
        SubsetLoanData <- select(LibraryLoanData, c(Title, Renewals, Recalls, Auto.Renewals, LoanMonth))
        
        ## REQUEST DATASET ##
        # Changing format of Date
        LibraryRequestData$Request.Date <- as.Date(LibraryRequestData$Request.Date, origin = "1899-12-30")
        
        # Recording month of Request
        LibraryRequestData$RequestMonth <- strftime(LibraryRequestData$Request.Date, "%B")
        
        # Subset of data
        SubsetRequestData <- select(LibraryRequestData, c(Title, "#.of.requests", RequestMonth))
        
        ## MONTHLY AGGREGATION ##
        # Declaring month names in a vector
        MonthNames <- c("January","February","March","April","May","June","July","August","September","October","November","December")
        
        AggregateData <- data.frame(Title = character(),
                                    Renewals = numeric(),
                                    Recalls = numeric(),
                                    Auto.Renewals = numeric(),
                                    Requests = numeric(),
                                    Month = character(),
                                    stringsAsFactors = FALSE)
        count = 1
        while (count <= 12){
          # Filter data according to month
          MonthlyLoanData <- filter(SubsetLoanData, LoanMonth == MonthNames[count])
          MonthlyRequestData <- filter(SubsetRequestData, RequestMonth == MonthNames[count])
          
          # Merge Loan and Request data based on Title
          MergedData<- merge(MonthlyLoanData, MonthlyRequestData, by.x = "Title", by.y = "Title", all.x = TRUE, all.y = TRUE)
          MergedData[is.na(MergedData)] <- 0
          
          # Drop Month Columns
          FinalData <- select(MergedData, c(Title, Renewals, Recalls, Auto.Renewals, "#.of.requests"))
          FinalData <- rename(FinalData, Requests = "#.of.requests")
          
          GroupedData <- FinalData %>% group_by(Title) %>% summarise(Renewals = sum(Renewals), Recalls = sum(Recalls), Auto.Renewals = sum(Auto.Renewals), Requests = sum(Requests), Month = MonthNames[count])
          AggregateData <- rbind(AggregateData, GroupedData)
          count <- count + 1
        }
        
      })
      
      shinyalert("Success!", "Files have been uploaded and data transformation steps have been completed", type = "success")
      
      shinyjs::enable("downloadmanipdata")
      
      shinyjs::enable("clustering")
      
    }
    
    AggregateData
    
  })
  
  #Output WSS Elbow chart for loans
  output$plts_req <- renderPlot({
      
      wss <- numeric(15)
      for(k in 1:15)
        wss[k] <- sum(kmeans(AggregateData()[,c(3,5)],centers=k,nstart=25)$withinss)
      plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")
    
  })
  
  #Output WSS Elbow chart for requests
  output$plts_ren <- renderPlot({
    
    wss <- numeric(15)
    for(k in 1:15)
      wss[k] <- sum(kmeans(AggregateData()[,c(2,4)],centers=k,nstart=25)$withinss)
    plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")
    
  })
  
  #Download Manipulated Data
  output$downloadmanipdata <- downloadHandler(
    
    filename = function() {
      paste("MergedManipulatedData-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(AggregateData(), file)
    }
  )
  
  # Run clustering algorithms
  observeEvent(input$clusterbutton,{
    
    #Disabling button to prevent re-clicks
    shinyjs::disable("clusterbutton")
    
    withProgress(message = "Please wait", detail = "Performing clustering operations", value = 5, {
      AggregateData_Clustering <- AggregateData()
      set.seed(1234)
      
      rv$clcnt_req <- as.numeric(input$clustercount_req)
      rv$clcnt_ren <- as.numeric(input$clustercount_ren)
      
      cluster1 <- kmeans(AggregateData_Clustering[,c(3,5)],rv$clcnt_req, nstart = 20)
      AggregateData_Clustering$Cluster1 <- cluster1$cluster
      clusterdets1 <- cluster1[["centers"]]
      clusterdets1_DF_Inter <- as.data.frame(clusterdets1)
      clusterdets1_DF_Inter$ClusterNo <- seq.int(nrow(clusterdets1_DF_Inter))
      rv$clusterdets1_DF <- clusterdets1_DF_Inter
      
      
      cluster2 <- kmeans(AggregateData_Clustering[,c(2,4)],rv$clcnt_ren, nstart = 20)
      AggregateData_Clustering[,"Cluster2"] = cluster2$cluster
      clusterdets2 <- cluster2[["centers"]]
      clusterdets2_DF_Inter <- as.data.frame(clusterdets2)
      clusterdets2_DF_Inter$ClusterNo <- seq.int(nrow(clusterdets2_DF_Inter))
      rv$clusterdets2_DF <- clusterdets2_DF_Inter
      
      # Dynamically creating assignment content - Requests & Recalls - Demand
      rv$demand_list_req <- list()
      for (i in 1:rv$clcnt_req){
        rv$demand_list_req[[i]] <- radioButtons(paste0("demandselect_req",i), label = paste0("Cluster ",i," - Demand"),choices = list("High", "Moderate", "Low"), inline = TRUE )
      }
      
      # Dynamically creating assignment content - Requests & Recalls - Dynamic Loan Period
      rv$dynamic_list_req <- list()
      
      for (k in 1:rv$clcnt_req){
        rv$dynamic_list_req[[k]] <- selectInput(paste0("dynamicselect_req",k), label = paste0("Cluster ",k," - Dynamic Loan Period"),choices = rep(paste0("Level-", 1:rv$clcnt_req)))
      }
      
      
      # Dynamically creating assignment content - Renewals & Auto-renewals - Demand
      rv$demand_list_ren <- list()
      
      for (m in 1:rv$clcnt_ren){
        rv$demand_list_ren[[m]] <- radioButtons(paste0("demandselect_ren",m), label = paste0("Cluster ",m," - Demand"),choices = list("High", "Moderate", "Low"), inline = TRUE )
      }
      
      # Dynamically creating assignment content - Renewals & Auto-renewals - Dynamic Loan Period
      rv$dynamic_list_ren <- list()
      
      for (n in 1:rv$clcnt_ren){
        rv$dynamic_list_ren[[n]] <- selectInput(paste0("dynamicselect_ren",n), label = paste0("Cluster ",n," - Dynamic Loan Period"),choices = rep(paste0("Level-", 1:rv$clcnt_ren)))
      }
      
    })
    
    shinyalert("Success!", "Clustering algorithms have been executed", type = "success")
    
    shinyjs::show("cluster_assign")
    
    rv$AggregateData_Clustered <- AggregateData_Clustering
    
  })
  
  # Render table with Requests & Recalls cluster details
  output$clusterdets_req <- renderTable({
    
    rv$clusterdets1_DF
    
  })
  
  # Render table with Renewals & Auto-renewals cluster details
  output$clusterdets_ren <- renderTable({
    
    rv$clusterdets2_DF
    
  })
  
  # Render category assignment radiobuttons and dropdowns for Requests & Recalls
  output$assignment_req_demand <- renderUI(rv$demand_list_req)
  output$assignment_req_dynamicloan <- renderUI(rv$dynamic_list_req)
  
  # Render category assignment radiobuttons and dropdowns for Renewals & Auto-renewals
  output$assignment_ren_demand <- renderUI(rv$demand_list_ren)
  output$assignment_ren_dynamicloan <- renderUI(rv$dynamic_list_ren)
  
  # Event that assigns demand category and dynamic loan period level to Aggregate Data
  observeEvent(input$assignmentbutton,{
    
    #Disabling button to prevent reclick
    shinyjs::disable("assignmentbutton")
    
    demand_vec_req <- vector()
    dynamic_vec_req <- vector()
    demand_vec_ren <- vector()
    dynamic_vec_ren <- vector()
    
    # Creating vector of demand categories and dynamic loan period levels
    for(a in 1:rv$clcnt_req){
      demand_vec_req[[a]] <- input[[paste0("demandselect_req", a)]]
    }
    
    for(b in 1:rv$clcnt_req){
      dynamic_vec_req[[b]] <- input[[paste0("dynamicselect_req", b)]]
    }
    
    for(c in 1:rv$clcnt_ren){
      demand_vec_ren[[c]] <- input[[paste0("demandselect_ren", c)]]
    }
    
    for(d in 1:rv$clcnt_ren){
      dynamic_vec_ren[[d]] <- input[[paste0("dynamicselect_ren", d)]]
    }
    
    # Include selected demand category and dynamic loan period level to Aggregate Dataset
    rv$AggregateData_Clustered$Demand1 <- factor(rv$AggregateData_Clustered$Cluster1, levels = c(1:rv$clcnt_req), labels = demand_vec_req)
    rv$AggregateData_Clustered$LoanPeriod1 <- factor(rv$AggregateData_Clustered$Cluster1, levels = c(1:rv$clcnt_req), labels = dynamic_vec_req)
    
    rv$AggregateData_Clustered$Demand2 <- factor(rv$AggregateData_Clustered$Cluster2, levels = c(1:rv$clcnt_ren), labels = demand_vec_ren)
    rv$AggregateData_Clustered$LoanPeriod2 <- factor(rv$AggregateData_Clustered$Cluster2, levels = c(1:rv$clcnt_ren), labels = dynamic_vec_ren)
    
    # Download tagged dataset to www folder
    tryCatch({
      
      write.csv(rv$AggregateData_Clustered,"www//Clustered_Data.csv", row.names = FALSE)
      shinyalert("Success!", "Demand category and dynamic loan level assignment activities have been completed", type = "success")
      
    }, err = function(e){
      
      shinyalert("Note", "Processing has been completed. Assignment CSV file could not be saved as it was open", type = "info")
      
    }, warning = function(w){
      
      shinyalert("Note", "Processing has been completed. Assignment CSV file could not be saved as it was open", type = "info")
      
    })
    
    shinyjs::show("classification")
    shinyjs::hide("viewdttrees")
    
  })
  
  # Event to classify Demand and Loan Periods
  observeEvent(input$classificationbutton,{
    
    #Disabling button to prevent re-clicks
    shinyjs::disable("classificationbutton")
    
    withProgress(message = "Please wait", detail = "Running classification algorithms", value = 5, {
      
      rv$classdata <- rv$AggregateData_Clustered
      
      set.seed(123)
      split = sample.split(rv$classdata$Demand1, SplitRatio = 0.8)
      train = subset(rv$classdata, split==TRUE)
      test = subset(rv$classdata, split==FALSE)
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3333)
      rv$dtree_fit1 <- train(Demand1 ~ Requests + Recalls, data = train, method = "rpart",
                          parms = list(split = "information"),
                          trControl=trctrl,
                          tuneLength = 10)
      
      split = sample.split(rv$classdata$Demand2, SplitRatio = 0.8)
      train = subset(rv$classdata, split==TRUE)
      test = subset(rv$classdata, split==FALSE)
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3333)
      rv$dtree_fit2 <- train(Demand2 ~ Renewals + Auto.Renewals, data = train, method = "rpart",
                          parms = list(split = "information"),
                          trControl=trctrl,
                          tuneLength = 10)
      
      split = sample.split(rv$classdata$LoanPeriod1, SplitRatio = 0.8)
      train = subset(rv$classdata, split==TRUE)
      test = subset(rv$classdata, split==FALSE)
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3333)
      rv$dtree_fit3 <- train(LoanPeriod1 ~ Requests + Recalls, data = train, method = "rpart",
                          parms = list(split = "information"),
                          trControl=trctrl,
                          tuneLength = 10)
      
      split = sample.split(rv$classdata$LoanPeriod2, SplitRatio = 0.8)
      train = subset(rv$classdata, split==TRUE)
      test = subset(rv$classdata, split==FALSE)
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3333)
      rv$dtree_fit4 <- train(LoanPeriod2 ~ Renewals + Auto.Renewals, data = train, method = "rpart",
                          parms = list(split = "information"),
                          trControl=trctrl,
                          tuneLength = 10)
    })
    
    shinyalert("Success!", "Demand category and dynamic loan level classification have been completed", type = "success")
    shinyjs::show("viewdttrees")
    shinyjs::show("labelassign")
    
  })
  
  output$plt_Dt1 <- renderPlot({
    req(rv$dtree_fit1$finalModel)
    prp(rv$dtree_fit1$finalModel, box.palette = "Reds", tweak = 1.2)
  })
  
  output$plt_Dt2 <- renderPlot({
    req(rv$dtree_fit2$finalModel)
    prp(rv$dtree_fit2$finalModel, box.palette = "Reds", tweak = 1.2)
  })
  
  output$plt_Dt3 <- renderPlot({
    req(rv$dtree_fit3$finalModel)
    prp(rv$dtree_fit3$finalModel, box.palette = "Reds", tweak = 1.2)
  })
  
  output$plt_Dt4 <- renderPlot({
    req(rv$dtree_fit4$finalModel)
    prp(rv$dtree_fit4$finalModel, box.palette = "Reds", tweak = 1.2)
  })
  
  
  observeEvent(input$assignlabels,{
    
    #Disabling button to prevent re-clicks
    shinyjs::disable("assignlabels")
    
    #Check if files are empty
    LoanFile_new <- input$finalfile1
    ReqFile_new <- input$finalfile2
    
    if (is.null(LoanFile_new) | is.null(ReqFile_new)) {
      
      shinyalert("Oops!", "Empty files", type = "error")
      shiny::enable("assignlabels")
      
    } else {
      
      withProgress(message = "Please wait", detail = "Uploading, transforming and tagging data files", value = 5, {
        
        LibraryLoanData_newlist = list()
        LibraryRequestData_newlist = list()
        
        for(i in 1:length(input$finalfile1[,1])){
          LibraryLoanData_newlist[[i]] <- read.xlsx(LoanFile_new[[i, 'datapath']], fillMergedCells = TRUE, colNames = TRUE)
        }
        
        for(j in 1:length(input$finalfile2[,1])){
          LibraryRequestData_newlist[[j]] <- read.xlsx(ReqFile_new[[j, 'datapath']], fillMergedCells = TRUE, colNames = TRUE)
        }
        
        # Binding data from different files to form one dataframe for further use
        LibraryLoanData_new <- bind_rows(LibraryLoanData_newlist)
        LibraryRequestData_new <- bind_rows(LibraryRequestData_newlist)
        
        ## LOAN DATASET ##
        # Changing format of date columns
        LibraryLoanData_new$Loan.Date <- as.Date(LibraryLoanData_new$Loan.Date, origin = "1899-12-30")
        LibraryLoanData_new$Due.Date <- as.Date(LibraryLoanData_new$Due.Date, origin = "1899-12-30")
        LibraryLoanData_new$Return.Date <- as.Date(LibraryLoanData_new$Return.Date, origin = "1899-12-30")
        
        # Removing NA entries in Patron Group and Loan Date
        LibraryLoanData_new <- LibraryLoanData_new %>% drop_na(Patron.Group)
        LibraryLoanData_new <- LibraryLoanData_new %>% drop_na(Loan.Date)
        
        # Recording month of loan for each entry in dataset
        LibraryLoanData_new$LoanMonth <- strftime(LibraryLoanData_new$Loan.Date, "%B")
        LibraryLoanData_new$DueMonth <- strftime(LibraryLoanData_new$Due.Date, "%B")
        
        # Subsetted dataset for further use
        SubsetLoanData_new <- select(LibraryLoanData_new, c(Title, Renewals, Recalls, Auto.Renewals, LoanMonth))
        
        ## REQUEST DATASET ##
        # Changing format of Date
        LibraryRequestData_new$Request.Date <- as.Date(LibraryRequestData_new$Request.Date, origin = "1899-12-30")
        
        # Recording month of Request
        LibraryRequestData_new$RequestMonth <- strftime(LibraryRequestData_new$Request.Date, "%B")
        
        # Subset of data
        SubsetRequestData_new <- select(LibraryRequestData_new, c(Title, "#.of.requests", RequestMonth))
        
        ## MONTHLY AGGREGATION ##
        # Declaring month names in a vector
        MonthNames <- c("January","February","March","April","May","June","July","August","September","October","November","December")
        
        AggregateData_new <- data.frame(Title = character(),
                                    Renewals = numeric(),
                                    Recalls = numeric(),
                                    Auto.Renewals = numeric(),
                                    Requests = numeric(),
                                    Month = character(),
                                    stringsAsFactors = FALSE)
        count = 1
        while (count <= 12){
          # Filter data according to month
          MonthlyLoanData_new <- filter(SubsetLoanData_new, LoanMonth == MonthNames[count])
          MonthlyRequestData_new <- filter(SubsetRequestData_new, RequestMonth == MonthNames[count])
          
          # Merge Loan and Request data based on Title
          MergedData_new <- merge(MonthlyLoanData_new, MonthlyRequestData_new, by.x = "Title", by.y = "Title", all.x = TRUE, all.y = TRUE)
          MergedData_new[is.na(MergedData_new)] <- 0
          
          # Drop Month Columns
          FinalData_new <- select(MergedData_new, c(Title, Renewals, Recalls, Auto.Renewals, "#.of.requests"))
          FinalData_new <- rename(FinalData_new, Requests = "#.of.requests")
          
          GroupedData_new <- FinalData_new %>% group_by(Title) %>% summarise(Renewals = sum(Renewals), Recalls = sum(Recalls), Auto.Renewals = sum(Auto.Renewals), Requests = sum(Requests), Month = MonthNames[count])
          AggregateData_new <- rbind(AggregateData_new, GroupedData_new)
          count <- count + 1
        }
        
      })

      # Classification
      test_pred1 <- predict(rv$dtree_fit1, newdata = AggregateData_new)
      test_pred2 <- predict(rv$dtree_fit2, newdata = AggregateData_new)
      test_pred3 <- predict(rv$dtree_fit3, newdata = AggregateData_new)
      test_pred4 <- predict(rv$dtree_fit4, newdata = AggregateData_new)
      
      rv$Tagged_Data <- AggregateData_new
      
      rv$Tagged_Data$Demand_RequestRecall <- as.character(test_pred1)
      rv$Tagged_Data$Demand_RenewalAutoRen <- as.character(test_pred2)
      rv$Tagged_Data$LoanPeriod_RequestRecall <- as.character(test_pred3)
      rv$Tagged_Data$LoanPeriod_RenewalAutoRen <- as.character(test_pred4)
      
      # Modal dialogue to select which tab to proceed to
      switchtab_modal <- modalDialog(
        title = "Which section do you want to view?",
        align = "center",
        easyClose = F,
        actionButton("demandtabselect", "Demand-based Categorisation", style = " color: #FFFFFF; background-color: #5D5CED; border-color: #22324E"),
        actionButton("dynamictabselect", "Dynamic Loan Period", style = " color: #FFFFFF; background-color: #EC157A; border-color: #22324E")
      )
      
      output$dynamicslider_req <- renderUI({
        selectInput("dynamicselect_req", label = "Select level for Recalls & Requests",choices = unique(rv$Tagged_Data$LoanPeriod_RequestRecall))
      })
      output$dynamicslider_ren <- renderUI({
        selectInput("dynamicselect_ren", label = "Select level for Renewals & Auto-renewals",choices = unique(rv$Tagged_Data$LoanPeriod_RenewalAutoRen))
      })
      
      
      # Show the model on start up ...
      showModal(switchtab_modal)
      
    }
    
  })
  
  # Switch tabs
  observeEvent(input$demandtabselect,{
    removeModal()
    updateTabItems(session, "sidebarmenu-tabs", "demandtab")
  })
  
  observeEvent(input$dynamictabselect,{
    removeModal()
    
    updateTabItems(session, "sidebarmenu-tabs", "dynamicloantab")
  })
  
  #####################################
  # TAGGING DATASET ON EXISTING MODEL##
  #####################################
  
  observeEvent(input$classificationbutton_exist,{
    
    tryCatch({
      
      #Disabling button to prevent re-clicks
      shinyjs::disable("classificationbutton_exist")
      
      ClusteredData_Exist <- read.csv("www//Clustered_Data.csv", header = TRUE)
      
      withProgress(message = "Please wait", detail = "Running classification algorithms", value = 5, {
        
        rv$classdata <- ClusteredData_Exist
        
        set.seed(123)
        split = sample.split(rv$classdata$Demand1, SplitRatio = 0.8)
        train = subset(rv$classdata, split==TRUE)
        test = subset(rv$classdata, split==FALSE)
        
        trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
        set.seed(3333)
        rv$dtree_fit1 <- train(Demand1 ~ Requests + Recalls, data = train, method = "rpart",
                               parms = list(split = "information"),
                               trControl=trctrl,
                               tuneLength = 10)
        
        split = sample.split(rv$classdata$Demand2, SplitRatio = 0.8)
        train = subset(rv$classdata, split==TRUE)
        test = subset(rv$classdata, split==FALSE)
        
        trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
        set.seed(3333)
        rv$dtree_fit2 <- train(Demand2 ~ Renewals + Auto.Renewals, data = train, method = "rpart",
                               parms = list(split = "information"),
                               trControl=trctrl,
                               tuneLength = 10)
        
        split = sample.split(rv$classdata$LoanPeriod1, SplitRatio = 0.8)
        train = subset(rv$classdata, split==TRUE)
        test = subset(rv$classdata, split==FALSE)
        
        trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
        set.seed(3333)
        rv$dtree_fit3 <- train(LoanPeriod1 ~ Requests + Recalls, data = train, method = "rpart",
                               parms = list(split = "information"),
                               trControl=trctrl,
                               tuneLength = 10)
        
        split = sample.split(rv$classdata$LoanPeriod2, SplitRatio = 0.8)
        train = subset(rv$classdata, split==TRUE)
        test = subset(rv$classdata, split==FALSE)
        
        trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
        set.seed(3333)
        rv$dtree_fit4 <- train(LoanPeriod2 ~ Renewals + Auto.Renewals, data = train, method = "rpart",
                               parms = list(split = "information"),
                               trControl=trctrl,
                               tuneLength = 10)
      })
      
      shinyalert("Success!", "Demand category and dynamic loan level classifications have been completed", type = "success")
      shinyjs::disable("classificationbutton_exist")
      shinyjs::show("viewdttrees_exist")
      shinyjs::show("labelassign_exist")
      
    }, err = function(e){
      
      shinyalert("Please check!", "Could not process existing model as Clustered_Data CSV file was open", type = "error")
      shinyjs::enable("classificationbutton_exist")
      
    }, warning = function(w){
      
      shinyalert("Please check!", "Could not process existing model as Clustered_Data CSV file was open", type = "error")
      shinyjs::enable("classificationbutton_exist")
      
    })
    
  })
  
  output$plt_Dt1_exist <- renderPlot({
    req(rv$dtree_fit1$finalModel)
    prp(rv$dtree_fit1$finalModel, box.palette = "Reds", tweak = 1.2)
  })
  
  output$plt_Dt2_exist <- renderPlot({
    req(rv$dtree_fit2$finalModel)
    prp(rv$dtree_fit2$finalModel, box.palette = "Reds", tweak = 1.2)
  })
  
  output$plt_Dt3_exist <- renderPlot({
    req(rv$dtree_fit3$finalModel)
    prp(rv$dtree_fit3$finalModel, box.palette = "Reds", tweak = 1.2)
  })
  
  output$plt_Dt4_exist <- renderPlot({
    req(rv$dtree_fit4$finalModel)
    prp(rv$dtree_fit4$finalModel, box.palette = "Reds", tweak = 1.2)
  })
  
  # Upload new data and run tagging exercise
  observeEvent(input$assignlabels_exist,{
    
    #Disabling button to prevent re-clicks
    shinyjs::disable("labelassign_exist")
    
    #Check if files are empty
    LoanFile_exist <- input$finalfile1_exist
    ReqFile_exist <- input$finalfile2_exist
    
    if (is.null(LoanFile_exist) | is.null(ReqFile_exist)) {
      
      shinyalert("Oops!", "Empty files", type = "error")
      shiny::enable("labelassign_exist")
      
    } else {
      
      withProgress(message = "Please wait", detail = "Uploading, transforming and tagging data files", value = 5, {
        
        LibraryLoanData_existlist = list()
        LibraryRequestData_existlist = list()
        
        for(i in 1:length(input$finalfile1_exist[,1])){
          LibraryLoanData_existlist[[i]] <- read.xlsx(LoanFile_exist[[i, 'datapath']], fillMergedCells = TRUE, colNames = TRUE)
        }
        
        for(j in 1:length(input$finalfile2_exist[,1])){
          LibraryRequestData_existlist[[j]] <- read.xlsx(ReqFile_exist[[j, 'datapath']], fillMergedCells = TRUE, colNames = TRUE)
        }
        
        # Binding data from different files to form one dataframe for further use
        LibraryLoanData_exist <- bind_rows(LibraryLoanData_existlist)
        LibraryRequestData_exist <- bind_rows(LibraryRequestData_existlist)
        
        ## LOAN DATASET ##
        # Changing format of date columns
        LibraryLoanData_exist$Loan.Date <- as.Date(LibraryLoanData_exist$Loan.Date, origin = "1899-12-30")
        LibraryLoanData_exist$Due.Date <- as.Date(LibraryLoanData_exist$Due.Date, origin = "1899-12-30")
        LibraryLoanData_exist$Return.Date <- as.Date(LibraryLoanData_exist$Return.Date, origin = "1899-12-30")
        
        # Removing NA entries in Patron Group and Loan Date
        LibraryLoanData_exist <- LibraryLoanData_exist %>% drop_na(Patron.Group)
        LibraryLoanData_exist <- LibraryLoanData_exist %>% drop_na(Loan.Date)
        
        # Recording month of loan for each entry in dataset
        LibraryLoanData_exist$LoanMonth <- strftime(LibraryLoanData_exist$Loan.Date, "%B")
        LibraryLoanData_exist$DueMonth <- strftime(LibraryLoanData_exist$Due.Date, "%B")
        
        # Subsetted dataset for further use
        SubsetLoanData_exist <- select(LibraryLoanData_exist, c(Title, Renewals, Recalls, Auto.Renewals, LoanMonth))
        
        ## REQUEST DATASET ##
        # Changing format of Date
        LibraryRequestData_exist$Request.Date <- as.Date(LibraryRequestData_exist$Request.Date, origin = "1899-12-30")
        
        # Recording month of Request
        LibraryRequestData_exist$RequestMonth <- strftime(LibraryRequestData_exist$Request.Date, "%B")
        
        # Subset of data
        SubsetRequestData_exist <- select(LibraryRequestData_exist, c(Title, "#.of.requests", RequestMonth))
        
        ## MONTHLY AGGREGATION ##
        # Declaring month names in a vector
        MonthNames <- c("January","February","March","April","May","June","July","August","September","October","November","December")
        
        AggregateData_exist <- data.frame(Title = character(),
                                        Renewals = numeric(),
                                        Recalls = numeric(),
                                        Auto.Renewals = numeric(),
                                        Requests = numeric(),
                                        Month = character(),
                                        stringsAsFactors = FALSE)
        count = 1
        while (count <= 12){
          # Filter data according to month
          MonthlyLoanData_exist <- filter(SubsetLoanData_exist, LoanMonth == MonthNames[count])
          MonthlyRequestData_exist <- filter(SubsetRequestData_exist, RequestMonth == MonthNames[count])
          
          # Merge Loan and Request data based on Title
          MergedData_exist <- merge(MonthlyLoanData_exist, MonthlyRequestData_exist, by.x = "Title", by.y = "Title", all.x = TRUE, all.y = TRUE)
          MergedData_exist[is.na(MergedData_exist)] <- 0
          
          # Drop Month Columns
          FinalData_exist <- select(MergedData_exist, c(Title, Renewals, Recalls, Auto.Renewals, "#.of.requests"))
          FinalData_exist <- rename(FinalData_exist, Requests = "#.of.requests")
          
          GroupedData_exist <- FinalData_exist %>% group_by(Title) %>% summarise(Renewals = sum(Renewals), Recalls = sum(Recalls), Auto.Renewals = sum(Auto.Renewals), Requests = sum(Requests), Month = MonthNames[count])
          AggregateData_exist <- rbind(AggregateData_exist, GroupedData_exist)
          count <- count + 1
        }
        
      })
      
      # Classification
      test_pred1_exist <- predict(rv$dtree_fit1, newdata = AggregateData_exist)
      test_pred2_exist <- predict(rv$dtree_fit2, newdata = AggregateData_exist)
      test_pred3_exist <- predict(rv$dtree_fit3, newdata = AggregateData_exist)
      test_pred4_exist <- predict(rv$dtree_fit4, newdata = AggregateData_exist)
      
      rv$Tagged_Data <- AggregateData_exist
      
      rv$Tagged_Data$Demand_RequestRecall <- as.character(test_pred1_exist)
      rv$Tagged_Data$Demand_RenewalAutoRen <- as.character(test_pred2_exist)
      rv$Tagged_Data$LoanPeriod_RequestRecall <- as.character(test_pred3_exist)
      rv$Tagged_Data$LoanPeriod_RenewalAutoRen <- as.character(test_pred4_exist)
      
      # Modal dialogue to select which tab to proceed to
      switchtabexist_modal <- modalDialog(
        title = "Which section do you want to view?",
        align = "center",
        easyClose = F,
        actionButton("demandtabselect", "Demand-based Categorisation", style = " color: #FFFFFF; background-color: #5D5CED; border-color: #22324E"),
        actionButton("dynamictabselect", "Dynamic Loan Period", style = " color: #FFFFFF; background-color: #EC157A; border-color: #22324E")
      )
      
      # Show the model on start up ...
      showModal(switchtabexist_modal)
      
      output$dynamicslider_req <- renderUI({
        selectInput("dynamicselect_req", label = "Select level for Recalls & Requests",choices = unique(rv$Tagged_Data$LoanPeriod_RequestRecall))
      })
      output$dynamicslider_ren <- renderUI({
        selectInput("dynamicselect_ren", label = "Select level for Renewals & Auto-renewals",choices = unique(rv$Tagged_Data$LoanPeriod_RenewalAutoRen))
      })
      
    }
    
  })
  
  ##################################
  # DEMAND-BASED CATEGORISATION TAB#
  ##################################
  
  output$demandtable <- DT::renderDataTable({
    
    req(rv$Tagged_Data)
    
    select(rv$Tagged_Data, Title, Recalls, Auto.Renewals, Requests, Month, Demand_RequestRecall, Demand_RenewalAutoRen)

  })
  
 output$highprim_table <- renderDataTable({
   
   req(rv$Tagged_Data)
   
   select(rv$Tagged_Data[rv$Tagged_Data$Demand_RequestRecall == "High", ],Title, Month, Demand_RequestRecall, Demand_RenewalAutoRen)
   
 })
 
 output$modprim_table <- renderDataTable({
   
   req(rv$Tagged_Data)
   
   select(rv$Tagged_Data[rv$Tagged_Data$Demand_RequestRecall == "Moderate", ],Title, Month, Demand_RequestRecall, Demand_RenewalAutoRen)
   
 })
 
 output$lowprim_table <- renderDataTable({
   
   req(rv$Tagged_Data)
   
   select(rv$Tagged_Data[rv$Tagged_Data$Demand_RequestRecall == "Low", ],Title, Month, Demand_RequestRecall, Demand_RenewalAutoRen)
   
 })
 
 output$highsec_table <- renderDataTable({
   
   req(rv$Tagged_Data)
   
   select(rv$Tagged_Data[rv$Tagged_Data$Demand_RenewalAutoRen == "High", ],Title, Month, Demand_RenewalAutoRen, Demand_RequestRecall)
   
 })
 
 output$modsec_table <- renderDataTable({
   
   req(rv$Tagged_Data)
   
   select(rv$Tagged_Data[rv$Tagged_Data$Demand_RenewalAutoRen == "Moderate", ],Title, Month, Demand_RenewalAutoRen, Demand_RequestRecall)
   
 })
 
 output$lowsec_table <- renderDataTable({
   
   req(rv$Tagged_Data)
   
   select(rv$Tagged_Data[rv$Tagged_Data$Demand_RenewalAutoRen == "Low", ],Title, Month, Demand_RenewalAutoRen, Demand_RequestRecall)
   
 })
 
 #Download Report
 output$downloaddemandreport <- downloadHandler(
   
   filename = function() {
     paste("TaggedDataset-", Sys.Date(), ".csv", sep="")
   },
   content = function(file) {
     write.csv(rv$Tagged_Data, file)
   }
 )
 
 #########################
 #DYNAMIC LOAN PERIOD TAB#
 #########################
 
 # Subset so that only the selected rows are in model.data
 dynamicdata_req <- reactive({
   req(rv$Tagged_Data)
   subset(rv$Tagged_Data, LoanPeriod_RequestRecall %in% input$dynamicselect_req)
 })
 
 dynamicdata_ren <- reactive({
   req(rv$Tagged_Data)
   subset(rv$Tagged_Data, LoanPeriod_RenewalAutoRen %in% input$dynamicselect_ren)
 })
 
 output$dynamictable_req <- DT::renderDataTable({
   
   req(dynamicdata_req())
   
   select(dynamicdata_req(), Title, Recalls, Auto.Renewals, Requests, Month, LoanPeriod_RequestRecall, LoanPeriod_RenewalAutoRen)
   
 })
 
 output$dynamictable_ren <- DT::renderDataTable({
   
   req(dynamicdata_ren())
   
   select(dynamicdata_ren(), Title, Recalls, Auto.Renewals, Requests, Month, LoanPeriod_RenewalAutoRen, LoanPeriod_RequestRecall)
   
 })
 
 # Download report
 output$downloadloanreport <- downloadHandler(
   
   filename = function() {
     paste("TaggedDataset-", Sys.Date(), ".csv", sep="")
   },
   content = function(file) {
     write.csv(rv$Tagged_Data, file)
   }
 )
  
}
