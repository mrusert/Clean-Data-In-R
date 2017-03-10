library(reshape2)

get_data <- function() {
  
  fileName <- "get_data.zip"
  
  ## Check if data has already been downloaded
  if(!file.exists(fileName)) {
    ## Download data set from remote site
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = fileName)
  }
  
  ## Check if data set has already been unzipped
  if(!file.exists("UCI HAR Dataset")) {
    unzip(fileName)
  }
  
}

tidy_data <- function() {
  
  if(dir.exists("UCI HAR Dataset/")) {
      
      ## read in activity names and features 
      activityNames <- read.table("UCI HAR Dataset/activity_labels.txt", colClasses = c("integer", "character"))
      features <- read.table("UCI HAR Dataset/features.txt", colClasses = c("integer", "character"), col.names = c("featureId", "featureName"))
      
      ## subset features dataframe for values that start with "mean" or "std" and clean up column names
      features <- features[grepl(".*mean.*|.*std.*", features[,2]),]  
      features[,2] <- gsub("mean", "Mean", features[,2])
      features[,2] <- gsub("std","Std", features[,2])
      features[,2] <- gsub("[-()]","", features[,2])
      
      ## read in test/train data sets and combine subjects, activities, and features into one data frame
      trainData <- read.table("UCI HAR Dataset/train/X_train.txt")[features$featureId]
      trainActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
      trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
      trainData <- cbind(trainSubjects, trainActivities, trainData)
      
      testData <- read.table("UCI HAR Dataset/test/X_test.txt")[features$featureId]
      testActivities <- read.table("UCI HAR Dataset/test/y_test.txt")
      testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
      testData <- cbind(testSubjects, testActivities, testData)
      
      ## combine train and test data sets
      allData <- rbind(trainData, testData)
      colnames(allData) <- c("subject", "activity", features$featureName)
      
      ## convert activity and subject columns to factor
      allData$activity <- factor(allData$activity, levels = activityNames[,1], labels = activityNames[,2])
      allData$subject <- as.factor(allData$subject)
      
      ## melt data set by subject and activity 
      meltData <- melt(allData, id.vars = c("subject", "activity"), measure.vars = c(features$featureName))
      
      ## cast data set to average each feature by activity
      meanData <- dcast(meltData, subject + activity ~ variable, mean)
      
      ## write data set to txt file
      write.table(meanData, "tidy_data.txt", row.names = FALSE, quote =  FALSE)
      
  } else {
      stop("No data downloaded yet. Run get_data() to retrieve dataset.")
  }
  
}