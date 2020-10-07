

#dataDescription <- "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#Download zip archive
#download.file(dataUrl, destfile = "data.zip")

#Unzip
#unzip("data.zip")
#Set working directory to location "UCI HAR Dataset"




library(tidyr)
library(dplyr)
library(data.table)




#Read activity labels and rename them
#Read features
activityLabels <- read.table("activity_labels.txt")
colnames(activityLabels) <- c("activityId", "activityDescription")
features <- read.table("features.txt")

#Read test data
subjectTest <- read.table("test/subject_test.txt")
xTest <- read.table("test/X_test.txt")
yTest <- read.table("test/y_test.txt")

#Read train data
subjectTrain <- read.table("train/subject_train.txt")
xTrain <- read.table("train/X_train.txt")
yTrain <- read.table("train/y_train.txt")

#Assign feature names as column names to the testing and training data
colnames(xTest) <- features[ , 2]
colnames(xTrain) <- features[ , 2]

#Assign names to columns of activity and subject data for both the test data and the training data
colnames(yTest) <- "activityId"
colnames(subjectTest) <- "subjectId"
colnames(yTrain) <- "activityId"
colnames(subjectTrain) <- "subjectId"




################ MERGE THE TRAINING AND TEST SETS TO CREATE ONE DATA SET
#Combine the subject information, the activity information, and the testing data in both the test data and the training data
allTest <- cbind(subjectTest, yTest, xTest)
allTrain <- cbind(subjectTrain, yTrain, xTrain)

#Combine the training and test data
allData <- rbind(allTrain, allTest)




################ USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET
#Merge the activity labels with the full data set so that the activity labels are matched by activityId
allData <- merge(activityLabels, allData, by="activityId", all.x = TRUE)




################ EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT
#Select only the subjectId, activityId, activityDescription, and any column with mean() or std() in its name
allDataMSD <- select(allData, subjectId, activityId, activityDescription, contains("mean()") | contains("std()"))


################ CREATE TIDY DATA SET IN WHICH THE MEAN FOR EACH VARIABLE FOR EACH SUBJECT AND ACTIVITY IS CALCULATED
#Split the reduced data by subjectId and activityDescription
splitData <- split(allDataMSD, list(allDataMSD$subjectId, allDataMSD$activityDescription))

#Find the column means for all the numeric columns
result <- sapply(splitData, function(x){colMeans(x[ , -c(1,2,3)])})

#Transpose the resulting matrix because we want the columns to be the measurements, NOTE: Now we must add the subjectID and activityDescription back to the summarized data
result <- t(result)

#Add the subject and activity Ids back to the result by creating a data frame with subjectId and activityId and then using cbind()
rowNamesResult <- rownames(result)
subjActivityNames <- as.data.frame(rowNamesResult)
subjActivityDF<- separate(subjActivityNames, col=rowNamesResult, into=c("subject", "activity"), sep="\\.")
result2 <- cbind(subjActivityDF, result)

#Clear the rownames of the resulting tidy data
rownames(result2) <- c()

#Order by subject and activity and clean up row indexing
result2$subject <- as.numeric(result2$subject)
result2 <- result2[order(result2$subject, result2$activity), ]
rownames(result2) <- NULL




################ APPROPRIATELY LABEL THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES
# Modify the names of the columns to make them more descriptive
colnames(result2) <- gsub("^t", "time", colnames(result2))
colnames(result2) <- gsub("^f", "frequency", colnames(result2))
colnames(result2) <- gsub("Gyro", "Gyroscope", colnames(result2))
colnames(result2) <- gsub("Mag", "Magnitude", colnames(result2))
colnames(result2) <- gsub("Acc", "Accelerometer", colnames(result2))
colnames(result2) <- gsub("mean\\(\\)", "Mean", colnames(result2))
colnames(result2) <- gsub("std()", "Std", colnames(result2))
colnames(result2) <- gsub("-", "", colnames(result2))
colnames(result2) <- gsub("[()]", "", colnames(result2))




#Write the resulting tidy data to a file
write.table(result2, "tidyData.txt", row.names = FALSE)




