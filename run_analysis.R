## script:      run_analysis.R
##
##              analysis of Human Activity Recognition Using Smartphones Dataset
## 
## usage:       run_analysis.R (run all)
## 
## changelog:
##              20160608 MF intial version
##


#Prereq

#packages
library(data.table)
library(dplyr)
library(reshape2)

#data dir
if (!file.exists("data")){
        dir.create("data")
}

#file download
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile ="./data/fuci.zip")

#data stuff
unzip("./data/fuci.zip", exdir = "./data")


################################################################
#step 1. Merges the training and the test sets to create one data set.
################################################################

#activity labels data
dtActivityLabelsTest <- fread("./data/UCI HAR Dataset/test/y_test.txt")
dtActivityLabelsTrain <- fread("./data/UCI HAR Dataset/train/y_train.txt")
dtActivityLabels <- rbind(dtActivityLabelsTrain, dtActivityLabelsTest)
setnames(dtActivityLabels, "V1", "activityID")

#activity data
dtActivityTest <- fread("./data/UCI HAR Dataset/test/X_test.txt")
dtActivityTrain <- fread("./data/UCI HAR Dataset/train/X_train.txt")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)

#subject data
dtSubjectTest <- fread("./data/UCI HAR Dataset/test/subject_test.txt")
dtSubjectTrain <- fread("./data/UCI HAR Dataset/train/subject_train.txt")
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subjectID")

#Merge Columns
dtSubject <- cbind(dtSubject, dtActivityLabels)
dtActivityFinal <- cbind(dtSubject, dtActivity)

################################################################
#step 2. Extracts only the measurements on the mean 
#        and standard deviation for each measurement.
################################################################

#Features
dtFeatures <- fread("./data/UCI HAR Dataset/features.txt")
setnames(dtFeatures, names(dtFeatures), c("featureID", "featureName"))
#only mean or std features
dtFeaturesFinal <- dtFeatures[grep("mean|std", dtFeatures$featureName), ]
dtFeaturesFinal$featureName <- gsub('-mean', 'Mean', dtFeaturesFinal$featureName)
dtFeaturesFinal$featureName <- gsub('-std', 'Std', dtFeaturesFinal$featureName)
dtFeaturesFinal$featureName <- gsub('[-()]', '', dtFeaturesFinal$featureName)

#Add V to Code for join with dtActivity
dtFeaturesFinal$featureCode <- dtFeaturesFinal[, paste0("V", featureID)]
head(dtFeaturesFinal)
head(dtActivityFinal)

################################################################
#step 3. Uses descriptive activity names 
#       to name the activities in the data set
################################################################

dtActivityLabels <- fread("./data/UCI HAR Dataset/activity_labels.txt")
setnames(dtActivityLabels, names(dtActivityLabels), c("activityID", "activityName"))


#add ActivityName
setkey(dtActivityFinal, subjectID, activityID)
dtActivityFinal <- merge(dtActivityFinal, dtActivityLabels, by= "activityID", all.x = TRUE)
setkey(dtActivityFinal, subjectID, activityID, activityName)

#subset final columns
dtActivityColumns <- c(key(dtActivityFinal), dtFeaturesFinal$featureCode)
dtActivityFinal <- dtActivityFinal[, dtActivityColumns, with = FALSE]

dtActivityTidy <- data.table(melt(dtActivityFinal, key(dtActivityFinal), variable.name = "featureCode"))

################################################################
#step 4. Appropriately labels the data set with descriptive variable names.
################################################################

dtActivityTidyFinal <- merge(dtActivityTidy, dtFeaturesFinal[, list(featureID, featureCode, featureName)], by = "featureCode", all.x = TRUE)

#use factors
dtActivityTidyFinal$activity <- factor(dtActivityTidyFinal$activityName)
dtActivityTidyFinal$feature <- factor(dtActivityTidyFinal$featureName)

################################################################
#step 5. From the data set in step 4, creates a second, 
#       independent tidy data set with the average 
#       of each variable for each activity and each subject.
################################################################
dtActivityMean <- dcast(dtActivityTidyFinal, subjectID + activityName ~ featureName, mean)

write.table(dtActivityMean, "./data/ActivityMean.txt", row.names = FALSE, quote = FALSE)
