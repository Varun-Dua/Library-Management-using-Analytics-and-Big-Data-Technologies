
ETLLoan <- function(){
  # Accessing libraries
  
  library(openxlsx)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  
  # Reading base dataset on loan and returns
  LibraryLoanData <- read.xlsx(xlsxFile = "D:\\Business Analytics Project\\MSc. Business Analytics - Capstone Project Main Library Q1.xlsx", fillMergedCells = TRUE, colNames = TRUE)
  
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
  
  SubsetLoanData <- select(LibraryLoanData, c(Title, Renewals, Recalls, Auto.Renewals, LoanMonth))
  return (SubsetLoanData)
}



ETLRequest <- function(){
  # Reading data on Requests
  LibraryRequestData <- read.xlsx(xlsxFile = "D:\\Business Analytics Project\\MSc. Business Analytics - Capstone Project Requests Main Library.xlsx", fillMergedCells = TRUE, colNames = TRUE)
  
  # Changing format of Date
  LibraryRequestData$Request.Date <- as.Date(LibraryRequestData$Request.Date, origin = "1899-12-30")
  
  # Recording month of Request
  LibraryRequestData$RequestMonth <- strftime(LibraryRequestData$Request.Date, "%B")
  
  # Subset of data
  SubsetRequestData <- select(LibraryRequestData, c(Title, "#.of.requests", RequestMonth))
  return (SubsetRequestData)
}
  




MonthlyFilter <- function(LoanData, RequestData){
  # Declaring month names in a vector
  MonthNames <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  
  AggregateData <- data.frame(Title = character(),
                              Renewals = numeric(),
                              Recalls = numeric(),
                              Auto.Renewals = numeric(),
                              Requests = numeric(),
                              stringsAsFactors = FALSE)
  count = 1
  while (count <= 12){
    # Filter data according to month
    MonthlyLoanData <- filter(LoanData, LoanMonth == MonthNames[count])
    MonthlyRequestData <- filter(RequestData, RequestMonth == MonthNames[count])
    
    # Merge Loan and Request data based on Title
    MergedData<- merge(MonthlyLoanData, MonthlyRequestData, by.x = "Title", by.y = "Title", all.x = TRUE, all.y = TRUE)
    MergedData[is.na(MergedData)] <- 0
    
    # Drop Month Columns
    FinalData <- select(MergedData, c(Title, Renewals, Recalls, Auto.Renewals, "#.of.requests"))
    FinalData <- rename(FinalData, Requests = "#.of.requests")
    
    GroupedData <- FinalData %>% group_by(Title) %>% summarise(Renewals = sum(Renewals), Recalls = sum(Recalls), Auto.Renewals = sum(Auto.Renewals), Requests = sum(Requests))
    AggregateData <- rbind(AggregateData, GroupedData)
    count <- count + 1
  }
  return (AggregateData)
}




# WSS analysis to determine the number of cluster (K)

SSERequests <- function(cluster_data){
  wss <- numeric(15)
  for(k in 1:15)
    wss[k] <- sum(kmeans(cluster_data[,c(3,5)],centers=k,nstart=25)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")
}



# Clustering

clusterRequestData <- function(cluster_data){
  set.seed(1234)
  cluster <- kmeans(cluster_data[,c(3,5)],7, nstart = 20)
  cluster
  ggplot(cluster_data, aes(Auto.Renewals, Requests)) + geom_point(color = cluster$cluster)
  cluster_data[,"Cluster1"] = cluster$cluster
  cluster_data$Demand1<-factor(cluster_data$Cluster1, levels = c(1,2,3,4,5,6,7), labels = c("Moderate", "High", "Low", "High", "High", "Moderate", "High"))
  cluster_data$LoanPeriod1<-factor(cluster_data$Cluster1, levels = c(1,2,3,4,5,6,7), labels = c("6 Days", "3 Days", "7 Days", "1 Day", "2 Days", "5 Days", "4 Days"))
  return (cluster_data)
}




# WSS analysis to determine the number of cluster (K)

SSERenewals <- function(cluster_data){
  wss <- numeric(15)
  for(k in 1:15)
    wss[k] <- sum(kmeans(cluster_data[,c(2,4)],centers=k,nstart=25)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")
}



# Clustering

clusterRenewalData <- function(cluster_data){
  set.seed(1234)
  cluster <- kmeans(cluster_data[,c(2,4)],10, nstart = 20)
  cluster
  ggplot(cluster_data, aes(Auto.Renewals, Requests)) + geom_point(color = cluster$cluster)
  cluster_data[,"Cluster2"] = cluster$cluster
  cluster_data$Demand2<-factor(cluster_data$Cluster2, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("High", "Moderate", "High", "Moderate", "Low", "Moderate", "Low", "Moderate", "High", "High"))
  cluster_data$LoanPeriod2<-factor(cluster_data$Cluster2, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("2 Days", "3 Days", "2 Days", "3 Days", "6 Days", "5 Days", "7 Days", "5 Days", "2 Days", "1 Day"))
  return (cluster_data)
}





# Export Data to CSV
export <- function(cluster_data){
  write.csv(cluster_data, file = "Clustered_Data.csv")
}




# Splitting the data

split <- function(cluster_data){
  
}




# Decision Tree using Information Gain as the Splitting Criteria

DT_IG_D1 <- function(cluster_data){
  
  library(rpart)
  library(rpart.plot)
  library(caret)
  library(caTools)
  
  set.seed(123)
  split = sample.split(cluster_data$Demand1, SplitRatio = 0.8)
  train = subset(cluster_data, split==TRUE)
  test = subset(cluster_data, split==FALSE)
 
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  set.seed(3333)
  dtree_fit <- train(Demand1 ~ Requests + Recalls, data = train, method = "rpart",
                     parms = list(split = "information"),
                     trControl=trctrl,
                     tuneLength = 10)
  dtree_fit
  prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
  test_pred <- predict(dtree_fit, newdata = test)
  confusionMatrix(test_pred, test$Demand1)
}


DT_IG_D2 <- function(cluster_data){
  
  library(rpart)
  library(rpart.plot)
  library(caret)
  library(caTools)
  
  set.seed(123)
  split = sample.split(cluster_data$Demand2, SplitRatio = 0.8)
  train = subset(cluster_data, split==TRUE)
  test = subset(cluster_data, split==FALSE)
  
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  set.seed(3333)
  dtree_fit <- train(Demand2 ~ Renewals + Auto.Renewals, data = train, method = "rpart",
                     parms = list(split = "information"),
                     trControl=trctrl,
                     tuneLength = 10)
  dtree_fit
  prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
  test_pred <- predict(dtree_fit, newdata = test)
  confusionMatrix(test_pred, test$Demand2)
}

DT_IG_L1 <- function(cluster_data){
  
  library(rpart)
  library(rpart.plot)
  library(caret)
  library(caTools)
  
  set.seed(123)
  split = sample.split(cluster_data$LoanPeriod1, SplitRatio = 0.8)
  train = subset(cluster_data, split==TRUE)
  test = subset(cluster_data, split==FALSE)
  
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  set.seed(3333)
  dtree_fit <- train(LoanPeriod1 ~ Requests + Recalls, data = train, method = "rpart",
                     parms = list(split = "information"),
                     trControl=trctrl,
                     tuneLength = 10)
  dtree_fit
  prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
  test_pred <- predict(dtree_fit, newdata = test)
  confusionMatrix(test_pred, test$LoanPeriod1)
}

DT_IG_L2 <- function(cluster_data){
  
  library(rpart)
  library(rpart.plot)
  library(caret)
  library(caTools)
  
  set.seed(123)
  split = sample.split(cluster_data$LoanPeriod2, SplitRatio = 0.8)
  train = subset(cluster_data, split==TRUE)
  test = subset(cluster_data, split==FALSE)
  
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  set.seed(3333)
  dtree_fit <- train(LoanPeriod2 ~ Renewals + Auto.Renewals, data = train, method = "rpart",
                     parms = list(split = "information"),
                     trControl=trctrl,
                     tuneLength = 10)
  dtree_fit
  prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
  test_pred <- predict(dtree_fit, newdata = test)
  confusionMatrix(test_pred, test$LoanPeriod2)
}

# Decision Tree using Gini Index as the Splitting Criteria

DT_GI <- function(){
  library(rpart)
  install.packages("rpart.plot")
  library(rpart.plot)
  library(caret)
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  set.seed(3333)
  dtree_fit <- train(Cluster ~ Requests + Auto.Renewals + Recalls + Renewals, data = train, method = "rpart",
                     parms = list(split = "gini"),
                     trControl=trctrl,
                     tuneLength = 10)
  dtree_fit
  prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
  test_pred <- predict(dtree_fit, newdata = test)
  confusionMatrix(test_pred, test$Cluster)
}



SubsetLoanData = ETLLoan()
SubsetRequestData = ETLRequest()
AData <- MonthlyFilter(SubsetLoanData, SubsetRequestData)
SSERequests(AData)
SSERenewals(AData)
clusteredRequest <- clusterRequestData(AData)
ClusteredRenewals <- clusterRenewalData(clusteredRequest)
export(ClusteredRenewals)
DT_IG_D1(ClusteredRenewals)
DT_IG_D2(ClusteredRenewals)
DT_IG_L1(ClusteredRenewals)
DT_IG_L2(ClusteredRenewals)







