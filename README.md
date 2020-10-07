# README

### Week 4 Final Project – Getting and Cleaning Data
### Data Science Course Offered Through Coursera

#### Project Description
The focus of this project is downloading and cleaning data.  
The data comes from the UCE Machine Learning repository.  
The data can be found at https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip. 
A description of the data can be found at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#### Project Files
**Data Processing Script: run_analysis.R**

run_analysis.R reads several data sets and then combines them into one data set.  
The data set is then reduced to include only measurements pertaining to means or standard deviations.  
The script groups the data by subject and activity and calculates the mean.  
Finally, the script transforms the data so that it is tidy and the variable names meet R standards and are descriptive.

**Tidy Data Set: tidyDataset.txt**

tidyDataset.txt is the output from the run_analysis.R script.  
It has the subject identification number, the activity description and the mean of sixty-six measurement involving either means or standard deviations.

**Data Processing Steps Description and Variable Names: Codebook**

Codebook describes how the data was combined, transformed, and then output as a tidy data set.  
It also includes names and descriptions of variables used in the run_Analysis.R script as well as names and descriptions of variables in tidyData.txt.
