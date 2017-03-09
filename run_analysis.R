######################################################################################
# Code to create tidy data
# Author - Steven Desmarais
######################################################################################
#
# The development environment is RStudio 1.0.136 running under Windows 10.
#
######################################################################################
# READ DATA FOR TEST GROUP
#
# The raw data contains 561 variables with column names contained in features.txt.  This file
# is necessary to apply column names to the data file.  In opening the file, it was noted that
# only 477 of 561 feature names are unique (84 duplicate names).  R noted the issue but 
# there were no data errors so the warnings were ignored. Columns are named "row" and
# "varname".
#
library(readr)
features <- read_delim("C:/Users/desma/Google Drive/Coursera/Data Cleaning/Project/UCI HAR Dataset/features.txt", 
                            " ", escape_double = FALSE, col_names = c("row", "varname"), 
                            trim_ws = TRUE)
#
# Raw data were provided in text (txt) files. For the large data files, these were converted
# into Excel xlsx format for simpler conversion and reading into R Studio.  Other import  
# functions read the data as character rather than number.
#
library(readxl)
X_test <- read_excel("C:/Users/desma/Google Drive/Coursera/Data Cleaning/Project/UCI HAR Dataset/test/X_test.xlsx", 
                          col_names = FALSE)
#
# Assign column names from the features file. Remove "()" in every name to simplify column names
#
features$newname <- gsub("[()]","",features$varname)
colnames(X_test) <- features$newname
#
# REDUCE FILE TO COLUMNS RELATED TO EITHER MEAN OR STANDARD DEVIATION (STD) MEASURES
# Used grep function to identify columns with the word "mean" and store in rel_mean vector.
#
rel_mean <- grep("mean", features$varname)
#
# Used grep function to identify columns with the word "std" (standard deviation) and 
# store in rel_std vector
rel_std <- grep("std", features$varname)
#
# Combine rel_mean and rel_std into single rel_both vector and sort by column number.
rel_both <- append(rel_mean, rel_std)
rel_both <- sort(rel_both)
#
# Use rel_both file to subset the relevant columns of X_test into new file X_test_reduced
#
X_test_reduced <- X_test[,rel_both]
#
# APPEND COLUMNS TO IDENTIFY SUBJECT ID (1-30) AND ACTIVITY (LAY, WALK UPSTAIRS, ETC.)
#
# Raw data did not contain subject ID (subject_test.txt) and activty (y_test.txt) for each 
# row of data.  These are provided in separate files. Both have the correct number of rows
# to match data in X_test data file. For ease of processing, the files were converted in Excel to comma separated (csv)
# prior to reading into R. In addition, column names were added as the first row and the files
# were renamed to subject_id.csv and y_test_id.csv prior to importing.
#
subject_id <- read_csv("C:/Users/desma/Google Drive/Coursera/Data Cleaning/Project/UCI HAR Dataset/test/subject_id.csv")
y_test_id <- read_csv("C:/Users/desma/Google Drive/Coursera/Data Cleaning/Project/UCI HAR Dataset/test/y_test_id.csv")
#
# Combine subject and test ID information into a new file, subject_test. Then add group
# designation as "test".
#
subject_test <- cbind.data.frame(subject_id, y_test_id)
subject_test$group <- "test"
#
# Read activity_labels file, which contains spelled out activity names, and merge with the 
# subject_test file.
#
activity_labels <- read_delim("C:/Users/desma/Google Drive/Coursera/Data Cleaning/Project/UCI HAR Dataset/activity_labels.txt", 
                                  " ", escape_double = FALSE, col_names = c("test", "label"), 
                                  trim_ws = TRUE)
#
# Merge activity labels for ease of viewing data rather than only the numeric test ID.
#
subject_test2 <- merge.data.frame(subject_test, activity_labels, by.x = "test", by.y = "test")
#
# Create final dataset for the test group by combining subject and test information 
# (subject_test2) with the X_test_reduced data file that contains the 79 relevant variables.
#
X_test_complete <- cbind.data.frame(subject_test2, X_test_reduced)
#
######################################################################################
#
# Repeat process for Train group
#
######################################################################################
# READ DATA FOR TRAIN GROUP
#
# FEATURES FILE EXISTS FROM PRIOR PROCESS
#
# Raw data were provided in text (txt) files. For the large data files, these were converted
# into Excel xlsx format for simpler conversion and reading into R Studio.  Other import  
# functions read the data as character rather than number.
#
X_train <- read_excel("C:/Users/desma/Google Drive/Coursera/Data Cleaning/Project/UCI HAR Dataset/train/X_train.xlsx", 
                     col_names = FALSE)
#
# Assign column names from the features file. Remove "()" in every name to simplify column names
#
features$newname <- gsub("[()]","",features$varname)
colnames(X_train) <- features$newname
#
# REDUCE FILE TO COLUMNS RELATED TO EITHER MEAN OR STANDARD DEVIATION (STD) MEASURES
# Used grep function to identify columns with the word "mean" and store in rel_mean vector.
#
# USE REL_BOTH FILE CREATED FOR X_TEST GROUP
# Use rel_both file to subset the relevant columns of X_test into new file X_test_reduced
#
X_train_reduced <- X_train[,rel_both]
#
# APPEND COLUMNS TO IDENTIFY SUBJECT ID (1-30) AND ACTIVITY (LAY, WALK UPSTAIRS, ETC.)
#
# Raw data did not contain subject ID (subject_train.txt) and activty (y_train.txt) for each 
# row of data.  These are provided in separate files. Both have the correct number of rows
# to match data in X_train data file. For ease of processing, the files were converted in Excel to comma separated (csv)
# prior to reading into R. In addition, column names were added as the first row and the files
# were renamed to subject_id.csv and y_test_id.csv prior to importing.
#
subject_train_id <- read_csv("C:/Users/desma/Google Drive/Coursera/Data Cleaning/Project/UCI HAR Dataset/train/subject_train_id.csv")
y_train_id <- read_csv("C:/Users/desma/Google Drive/Coursera/Data Cleaning/Project/UCI HAR Dataset/train/y_train_id.csv")
#
# Combine subject and test ID information into a new file, subject_test.
#
subject_train <- cbind.data.frame(subject_train_id, y_train_id)
subject_train$group <- "train"
#
# Read activity_labels file, which contains spelled out activity names, and merge with the 
# subject_test file.  USE FILE FROM PROCESSING OF TEST GROUP
#
#
# Merge activity labels for ease of viewing data rather than only the numeric test ID.
#
subject_train2 <- merge.data.frame(subject_train, activity_labels, by.x = "test", by.y = "test")
#
# Create final dataset for the test group by combining subject and test information 
# (subject_test2) with the X_test_reduced data file that contains the 79 relevant variables.
#
X_train_complete <- cbind.data.frame(subject_train2, X_train_reduced)
#
######################################################################################
#
# Combine train and test data into single file - X_data_all
#
######################################################################################
#
X_data_all <- rbind.data.frame(X_train_complete,X_test_complete)
#
######################################################################################
#
# Create a summary file of results by subject and test (walking, sitting, etc.)
#
######################################################################################
#
library(dplyr)
X_data_groups <- group_by(X_data_all, subject, test) 
X_data_means <- summarize_each(X_data_groups, funs(mean))
#
# ASSIGNMENT COMPLETE
#
######################################################################################