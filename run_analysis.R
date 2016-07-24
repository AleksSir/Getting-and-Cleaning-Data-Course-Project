run_analysis <- function() {
  if (!file.exists("data.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","data.zip",mode="wb")
    unzip("data.zip")
  }
  activityLabel <- read.table("UCI HAR Dataset\\activity_Labels.txt")
  features <- read.table("UCI HAR Dataset\\features.txt")
  
  trainSubject <- read.table("UCI HAR Dataset\\train\\subject_train.txt")
  trainData <- read.table("UCI HAR Dataset\\train\\x_train.txt")
  trainLabel <- read.table("UCI HAR Dataset\\train\\y_train.txt")
  
  testSubject <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
  testData <- read.table("UCI HAR Dataset\\test\\x_test.txt")
  testLabel <- read.table("UCI HAR Dataset\\test\\y_test.txt")
  
  rawdata <- rbind(trainData,testData)
  colnames(rawdata) <- features[,2]
  activity <- as.factor(rbind(trainLabel,testLabel)[[1]])
  levels(activity) <- activityLabel[,2]
  data <- rawdata[,grep("std|mean",colnames(rawdata))]
  data <- cbind(activity,rawdata)
  
  subject <- rbind(trainSubject,testSubject)
  colnames(subject) <- "subject"
  tempdata <- cbind(subject, data)
  templist <- lapply(split(tempdata,tempdata$activity),function(x){
    split(x,x$subject)
  })
  templist2 <- lapply(templist,function(k){
                lapply(k,function(j){
                  df <- j
                  y <- colMeans(subset(df,select = -c(subject,activity)))
                  df[1,3:length(df)] <- y
                  df <- df[1,]
                  df
    })
  })
  
  newdata <- do.call(rbind,lapply(templist2, function(p){
    do.call(rbind,p)
  
  }))


  
}