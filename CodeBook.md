
 # Code Book

### Week 4 Final Project – Getting and Cleaning Data
### Data Science Course Offered Through Coursera



#### 1.Merge the training and test data sets into one data set

The zip file containing the data was downloaded and unzipped.   
The files in the resulting directory were read and referenced using descriptive variable names.  
The subject information, activity labels, and measurements were combined for the test data. *(allTest)*. 
The subject information, activity labels, and measurements were combined for the training data. *(allTrain)*. 
The test and training data sets were combined together. *(allData)*. 



#### 2.Extract only the measurements on the mean and standard deviation for each measurement

A new data set was created in which measurement columns only on the mean and standard deviation were chosen by applying the select() function of the dplyr package to *(allData)*  
The result was the data set *(allDataMSD)*.

#### 3.Uses descriptive activity names to name the activities in the data set

This step occurred when the *(activityLabels)* data frame was merged with the *(allData)* data frame using the merge() function. 

#### 4.Appropriately labels the data set with descriptive variable names.

This step was completed after step 5.  
The column names for the tidy data set, *(result2)*, were modified using the gsub() function and some simple regular expressions so that the names met R standards and were descriptive.

#### 5.Creates a second, independent tidy data set with the average of each variable for each activity and subject
The data frame *(allDataMSD)* was split by *(subjectId)* and *(activityDescription)* to create *(splitData)*.  
Then the colMeans() function was applied inside of the sapply() function to calculate the column means for all measurement columns.  
The output was named *(result)*.  
*(result)* was a matrix that needed to be transposed so the measurement variables corresponded to columns.   
Finally, the *(subjectId)* and *(activityId)* columns had to be recombined with *(result)* to produce the desired tidy data set which was named *(result2)*.   
*(result2)* was written to a directory as a file named *tidyData.txt*.


#### Variables used in the run_Analysis.R script
dataDescription - source data description  
dataUrl - URL for zip archive of source data  

activityLabels - six activity codes and corresponding activity names  
features - feature names for xTest, xTrain  
subjectTest - test set subject number from 1 to 30  
xTest - test set feature measurements  
yTest - test set activity code from 1 to 6  
subjectTrain - train set subject number from 1 to 30  
xTrain - train set feature measurements  
yTrain - train set activity code from 1 to 6  
allTest - subjectTest, yTest, xTest combined by columns  
allTrain - subjectTrain, yTrain, xTrain combined by columns  
allData - allTest, allTrain combined by rows  
allDataMSD – allData with only columns about means and standard deviations  
splitData – allDataMSD split by subjectId and activityDescription variables  
result – the matrix with the column means for each measurement by subject and activity   
result2 – the final tidy data set with appropriate variable names with means for each measurement for each subject for each activity


#### Variables in the final tidy data set
* subject – the ID of the subject
* activity – the type of activity performed
* timeBodyAccelerometerMeanX
* timeBodyAccelerometerMeanY
* timeBodyAccelerometerMeanZ
* timeGravityAccelerometerMeanX
* timeGravityAccelerometerMeanY
* timeGravityAccelerometerMeanZ
* timeBodyAccelerometerJerkMeanX
* timeBodyAccelerometerJerkMeanY
* timeBodyAccelerometerJerkMeanZ
* timeBodyGyroscopeMeanX
* timeBodyGyroscopeMeanY
* timeBodyGyroscopeMeanZ
* timeBodyGyroscopeJerkMeanX
* timeBodyGyroscopeJerkMeanY
* timeBodyGyroscopeJerkMeanZ
* timeBodyAccelerometerMagnitudeMean
* timeGravityAccelerometerMagnitudeMean
* timeBodyAccelerometerJerkMagnitudeMean
* timeBodyGyroscopeMagnitudeMean
* timeBodyGyroscopeJerkMagnitudeMean
* frequencyBodyAccelerometerMeanX
* frequencyBodyAccelerometerMeanY
* frequencyBodyAccelerometerMeanZ
* frequencyBodyAccelerometerJerkMeanX
* frequencyBodyAccelerometerJerkMeanY
* frequencyBodyAccelerometerJerkMeanZ
* frequencyBodyGyroscopeMeanX
* frequencyBodyGyroscopeMeanY
* frequencyBodyGyroscopeMeanZ
* frequencyBodyAccelerometerMagnitudeMean
* frequencyBodyBodyAccelerometerJerkMagnitudeMean
* frequencyBodyBodyGyroscopeMagnitudeMean
* frequencyBodyBodyGyroscopeJerkMagnitudeMean
* timeBodyAccelerometerStdX
* timeBodyAccelerometerStdY
* timeBodyAccelerometerStdZ
* timeGravityAccelerometerStdX
* timeGravityAccelerometerStdY
* timeGravityAccelerometerStdZ
* timeBodyAccelerometerJerkStdX
* timeBodyAccelerometerJerkStdY
* timeBodyAccelerometerJerkStdZ
* timeBodyGyroscopeStdX
* timeBodyGyroscopeStdY
* timeBodyGyroscopeStdZ
* timeBodyGyroscopeJerkStdX
* timeBodyGyroscopeJerkStdY
* timeBodyGyroscopeJerkStdZ
* timeBodyAccelerometerMagnitudeStd
* timeGravityAccelerometerMagnitudeStd
* timeBodyAccelerometerJerkMagnitudeStd
* timeBodyGyroscopeMagnitudeStd
* timeBodyGyroscopeJerkMagnitudeStd
* frequencyBodyAccelerometerStdX
* frequencyBodyAccelerometerStdY
* frequencyBodyAccelerometerStdZ
* frequencyBodyAccelerometerJerkStdX
* frequencyBodyAccelerometerJerkStdY
* frequencyBodyAccelerometerJerkStdZ
* frequencyBodyGyroscopeStdX
* frequencyBodyGyroscopeStdY
* frequencyBodyGyroscopeStdZ
* frequencyBodyAccelerometerMagnitudeStd
* frequencyBodyBodyAccelerometerJerkMagnitudeStd
* frequencyBodyBodyGyroscopeMagnitudeStd
* frequencyBodyBodyGyroscopeJerkMagnitudeStd

#### Activity labels
* WALKING (1)
* WALKING_UPSTAIRS (2)
* WALKING_DOWNSTAIRS (3)
* SITTING (4)
* STANDING (5)
* LAYING (6)

