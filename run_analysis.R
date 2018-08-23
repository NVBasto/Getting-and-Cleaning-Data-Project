library(dplyr)
library(tidyr)
library(lubridate)

#setwd("C:/Users/nuno.basto/Documents/R/R Studio/Coursera/Getting and Cleaning Data/Project")

# # # # # # # # #
#               #
#   DOWNLOADS   #
#               #
# # # # # # # # #

if(!file.exists("./data")){
  dir.create("./data")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

unzip(zipfile="./data/Dataset.zip",exdir="./data")

# # # # # # # # #
#               #
#     PATHS     #
#               #
# # # # # # # # #

featuresFile <- "./data/UCI HAR Dataset/features.txt"
activitiyLabelsFile <- "./data/UCI HAR Dataset/activity_labels.txt"

trainSetFile <- "./data/UCI HAR Dataset/train/X_train.txt"
trainActivitiesFile <- "./data/UCI HAR Dataset/train/Y_train.txt"
trainSubjectsTrainingFile <- "./data/UCI HAR Dataset/train/subject_train.txt"

testSetFile <- "./data/UCI HAR Dataset/test/X_test.txt"
testActivitiesFile <- "./data/UCI HAR Dataset/test/Y_test.txt"
testSubjectsTrainingFile <- "./data/UCI HAR Dataset/test/subject_test.txt"

# # # # # # # # #
#               #
#     IMPORT    #
#               #
# # # # # # # # #

# Labels
features <- read.table(featuresFile, header = FALSE, sep = " ", col.names = c("featureID", "featureName"))
activityLabels <- read.table(activitiyLabelsFile, header = FALSE, sep = " ", col.names = c("activityID", "activityName"))


# Train
trainSet <- read.table(trainSetFile, header = FALSE, sep = "")
trainActivities <- read.table(trainActivitiesFile, header = FALSE, sep = "", col.names = c("activityID"))
trainSubjectsTraining <- read.table(trainSubjectsTrainingFile, header = FALSE, sep = "", col.names = c("subjectID"))


# Test
testSet <- read.table(testSetFile, header = FALSE, sep = "")
testActivities <- read.table(testActivitiesFile, header = FALSE, sep = "", col.names = c("activityID"))
testSubjectsTraining <- read.table(testSubjectsTrainingFile, header = FALSE, sep = "", col.names = c("subjectID"))


# # # # # # # # #
#               #
#   TRANSFORM   #
#               #
# # # # # # # # #

# Train
colnames(trainSet) <- features$featureName
trainSet$subjectID <- trainSubjectsTraining$subjectID
trainSet$activityID <- trainActivities$activityID
trainSet$set <- "Train"


# Test
colnames(testSet) <- features$featureName
testSet$subjectID <- testSubjectsTraining$subjectID
testSet$activityID <- testActivities$activityID
testSet$set <- "Test"

# Merge
fullSet <- rbind(trainSet, testSet)


# Extract only mean and sd for each measurement
fullSetColumnNames <- colnames(fullSet)

columns_wanted <- (grepl("activityID", fullSetColumnNames)
                     | grepl("subjectID", fullSetColumnNames)
                     | grepl("set", fullSetColumnNames) 
                     | grepl("mean..", fullSetColumnNames) 
                     | grepl("std..", fullSetColumnNames))


meanAndStdSet <- fullSet[,columns_wanted == TRUE]


# Add Activity Name
withActivityNameSet <- merge(meanAndStdSet, activityLabels, by = "activityID", all.x = TRUE)


# Tidy up
tidySet <- aggregate(withActivityNameSet, by = list(withActivityNameSet$subjectID, withActivityNameSet$activityName), FUN = mean)
tidySet <- select(tidySet, -activityID, -set, -activityName, subjectID)
colnames(tidySet)[colnames(tidySet)=="Group.1"] <- "subjectID"
colnames(tidySet)[colnames(tidySet)=="Group.2"] <- "activity"

tidySet <- tidySet[order(tidySet$subjectID, tidySet$activity),]


# Write output to file
write.table(tidySet, "Project.txt",row.names=FALSE)

# Produce codebook
library(knitr)
knit2html("codebook.Rmd")