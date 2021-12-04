#**********************************************************************************
# Step 1: get needed libraries
#**********************************************************************************
library(data.table) # it helps to load fast large text file
library(dplyr)#load dplyr

#**********************************************************************************
# Step 2: create directories
#**********************************************************************************
if(!file.exists("./projectData")){
  dir.create("./projectData")
}

#**********************************************************************************
# Step 3: download the dataset
#         Get data from link
#         data might update, check if downloaded already, do if not
#         data is zipped, unzip
#**********************************************************************************
dataFile <- "CleaningData_Week4_PeerReview"
if (!file.exists(dataFile)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  print("downloading....")
  download.file(fileURL, destfile = "projectData/Dataset.zip", method="curl")
  print("done download")
}

if (!file.exists("UCI HAR Dataset")) { 
  print("unzipping file")
  unzip(zipfile = "projectData/Dataset.zip",exdir="./data")
  print("done unzip") #need to learn logging in R
}


#Setup Completed, respond to question

#*********************************************************************
#Step 3. a.Read the files and name appropriately (training, testing)
#        b.Extract features
#*********************************************************************
x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
print("test data setup")


x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt", col.names = "code")
subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
print("train data setup")

features <- read.table("data/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- tbl_df(read.table("data/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity")))
subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
print("features data setup")


#*********************************************************************
#Step 4.Merge train and  test data to create one data set.
#       View the data
#*********************************************************************

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

summary(Merged_Data)#result difficult to read
head(Merged_Data)
dim(Merged_Data)

#*********************************************************************
#Step 5.Get the mean and standard deviation for each measurement.
#       Descriptive activity names to name the activities in the data set.
#       Labels  data set with descriptive variable names.
#       
#*********************************************************************
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
TidyData$code <- activities[TidyData$code, 2]

names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))



##*********************************************************************
#Step 6.Create a second, independent tidy dataset with the average of 
#           each variable for each activity and each subject
#       see the result
#*********************************************************************
TidyData2 <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(TidyData2, "FinalData.txt", row.name=FALSE)


str(TidyData2)
TidyData2
