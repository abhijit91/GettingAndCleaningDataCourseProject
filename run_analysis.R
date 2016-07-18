#setwd("~/R/Development/workspace/data/GettingAndCleaningData/UCI HAR Dataset/")

##This script assumes that it lies in the same directory as the data set.
##It expects to find the files features.txt & activity_labels.txt in the same folder.
##It also expects to find 2 subdirectories for the test & train data sets.

##0.1 Read Features & Activity Labels
features <- read.table(file = "features.txt")
activity_labels <- read.table(file = "activity_labels.txt")

##0.2 Read Training Data
subject_train <- read.table(file = "./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")

##0.3 Read Test Data
subject_test <- read.table(file = "./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")

##1. Merges the training and the test sets to create one data set.
X_combined <- rbind(X_train, X_test)
subject_combined <- rbind(subject_train, subject_test)
y_combined <- rbind(y_train, y_test)

##2. Extract mean and standard deviation for each measurement

##2.1 Extract all row numbers containing "mean()" or "std()" in 2nd column. 
##    This is because the features dataframe contains rows of tuples of
##    (column_number, column_name). This means, we need to find the 
##    column_numners where the column_name contains "mean()" or "std()".
##    Then we subset the X dataset based on the extracted column_numbers.
narrow_cols <-
        features[grep("mean\\()|std\\()", features[, 2]), ]

##2.2 Extract columne names for select row numbers.
##    This is later used to properly name the variables.

narrow_colnames <- narrow_cols[, 2]

##2.3 Extract Columns from X_combined data set and store them into a narrow data set.
##    The narrow data set at this point contains all the variables that had 
##    "mean()" or "std()" in their names.
narrow <- X_combined[as.numeric(narrow_cols[, 1])]

##3. Uses descriptive activity names to name the activities in the data set.
##   Levels are sorted since the labels are in sorted order.
##   If it is not sorted, levels & labels will not be oriented.
y_combined[, 1] <-
        factor(y_combined[, 1],
               levels = sort(unique(y_combined[, 1])),
               labels = as.character(activity_labels[, 2]))

##4. Appropriately labels the data set with descriptive variable names

##4.1 Combine narrow data set with activity and subject data sets.
narrow <- cbind(narrow, subject_combined, y_combined)

##4.2 Add Column names for observations, subject & activity
names(narrow) <-
        c(as.character(narrow_colnames), "subject", "activity")

##5 create a second, independent tidy data set with the average of 
##  each variable for each activity and each subject.

narrow_mean <-
        aggregate(. ~ subject + activity , data = narrow, mean)

write.table(narrow_mean,file="tidyData.txt")