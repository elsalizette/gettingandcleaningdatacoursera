library(plyr)
library(httr)

#########################################################################
## Download data source zip file
#########################################################################


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
localfile <- "dataset.zip"
if(!file.exists(localfile))
{
   download.file(fileUrl, localfile, method="wget", extra=c("--no-check-certificate"))
}
unzip(localfile,  overwrite=TRUE, list=FALSE)

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE, stringsAsFactors = TRUE)

#########################################################################
## 1. Merges the training and the test sets to create one data set.
#########################################################################

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, strip.white = TRUE, colClasses = "numeric")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, strip.white = TRUE, colClasses = "numeric")

y_test <- read.csv("UCI HAR Dataset/test/y_test.txt", header = FALSE)
y_train <- read.csv("UCI HAR Dataset/train/y_train.txt", header = FALSE)

subject_test  <- read.csv("UCI HAR Dataset/test/subject_test.txt",   header = FALSE)
subject_train <- read.csv("UCI HAR Dataset/train/subject_train.txt", header = FALSE)


both_sets  <- rbind(x_test, x_train)
activities <- rbind(y_test, y_train)
subjects <- rbind(subject_test, subject_train)
names(subjects) <- c("Subject")

#########################################################################
## 2. Appropriately labels the data set with descriptive variable names.
#########################################################################

features <- read.csv("UCI HAR Dataset/features.txt", sep = " ", header = FALSE)
names(both_sets) <- as.vector(features[,2])

#########################################################################
## 3. Extracts only the measurements on the mean and standard deviation for each measurement.
#########################################################################

tidy_set <- both_sets[, grep("mean|std", names(both_sets))]

#########################################################################
## 4. Uses descriptive activity names to name the activities in the data set
#########################################################################

Activity <- factor(activities$V1, labels = activity_labels$V2)
tidy_set_with_activity <- cbind(tidy_set, Activity, subjects)

#########################################################################
## 5. From the data in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject
#########################################################################

custom_average  <-  function(df)  {
   colMeans(df[, grep("mean|std", names(df))])
}

second_data_set <- ddply(tidy_set_with_activity, .(Activity,Subject), custom_average)

#########################################################################
## 6. Save tidy data set into a file
#########################################################################

# write.csv(second_data_set, "tidy_dataset.csv")

write.table(second_data_set, "tidy_dataset.txt", sep = ",", row.names = FALSE)





