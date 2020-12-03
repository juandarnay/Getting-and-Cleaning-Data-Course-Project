# You should create one R script called run_analysis.R that does the following.
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Loading and installing  packages
ifelse(!require("data.table"), install.packages("data.table"), require("data.table"))

ifelse(!require("dplyr"), install.packages("dplyr"), require("dplyr"))

#  Getting the data from internet
path <- getwd()

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(url, file.path(path, "dataset.zip"))

unzip(zipfile = "dataset.zip")

# Read features file
features <- 
  read.table(file.path(path, "UCI HAR Dataset/features.txt"), col.names=c("row_number","variable_name"))

# Filter the variables - mean() and std()
needed_variables <- 
  filter(features, grepl("mean\\(\\)|std\\(\\)", variable_name))

# Make the features readable
# Remove special characters, Convert to lower case

features <- 
  mutate(features, variable_name = gsub("-", "", variable_name),
         variable_name = gsub("\\(", "", variable_name),
         variable_name = gsub("\\)", "", variable_name),
         variable_name = tolower(variable_name))

# Make the needed_variables readable
# Remove special characters, Convert to lower case

needed_variables <- 
  mutate(needed_variables, variable_name = gsub("-", "", variable_name),
         variable_name = gsub("\\(", "", variable_name),
         variable_name = gsub("\\)", "", variable_name),
         variable_name = tolower(variable_name))

# Read activity_labels file
activity_labels <- 
  read.table(file.path(path, "UCI HAR Dataset/activity_labels.txt"), col.names=c("activity", "activity_description"))

# Read test data stats
test_values <- 
  read.table(file.path(path, "UCI HAR Dataset/test/X_test.txt"), col.names = features$variable_name)

test_needed_values <- 
  test_values[ , needed_variables$variable_name]

# Read test activities
test_activities <- 
  read.table(file.path(path, "UCI HAR Dataset/test/y_test.txt"), col.names=c("activity"))

# Read in test subjects
subject_test <- 
  read.table(file.path(path, "UCI HAR Dataset/test/subject_test.txt"), col.names=c("subject"))

# Adding descriptions to test_activities
test_activities_with_desc <- 
  merge(test_activities, activity_labels)

# Putting the test data together
# Assuming that the data is in the same order and all we need is cbind
# Combining values, activities, subjects
test_data <- cbind(test_activities_with_desc, subject_test, test_needed_values)

# Read train data stats
train_values <- 
  read.table(file.path(path, "UCI HAR Dataset/train/X_train.txt"), col.names = features$variable_name)

train_needed_values <- 
  train_values[ , needed_variables$variable_name]

# Read train activities
train_activities <- 
  read.table(file.path(path, "UCI HAR Dataset/train/y_train.txt"), col.names=c("activity"))

# Read in test subjects
subject_train <- 
  read.table(file.path(path, "UCI HAR Dataset/train/subject_train.txt"), col.names=c("subject"))

# Adding descriptions to train_activities
train_activities_with_desc <- 
  merge(train_activities, activity_labels)

# Putting the train data together
# Assuming that the data is in the same order and all we need is cbind
# Combining values, activities, subjects
train_data <- 
  cbind(train_activities_with_desc, subject_train, train_needed_values)


# Combining the test_data and train_data together
# Additionally make subject a factor
test_train_data <- 
  rbind(test_data, train_data) %>% select( -activity )
test_train_data <- 
  mutate(test_train_data, subject = as.factor(test_train_data$subject))

# Creating a new table
write.table(test_train_data, "Mean_And_StdDev_For_Act_Subj.txt")

# Create a second, independent tidy data set with the average of each variable for each activity and each subject.
# Group the data by activity, subject
new_independent_tidy <- 
  group_by(test_train_data, activity_description, subject)
# Get the average (mean) of each variable
summarised_data <- summarise_each(new_independent_tidy, funs(mean))

# Write the data in a new (a newest) table
write.table(summarised_data, "Average_Var_By_Act_Subj.txt", row.names = FALSE)