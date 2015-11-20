setwd("K:/Development/rstudio/_coursera/GettingAndCleaningData/UCI HAR Dataset")
library(plyr)

# 1. Merges the training and the test sets to create one data set.
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt") 
subject_train <- read.table("./train/subject_train.txt")
subject_test <- read.table("./test/subject_test.txt")
mergeData <- rbind(x_train, x_test)
mergeLabel <- rbind(y_train, y_test)
mergeSubject <- rbind(subject_train, subject_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("./features.txt")
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
mergeData <- mergeData[, meanStdIndices]
names(mergeData) <- gsub("\\(\\)", "", features[meanStdIndices, 2])
names(mergeData) <- gsub("mean", "Mean", names(mergeData))
names(mergeData) <- gsub("std", "Std", names(mergeData))
names(mergeData) <- gsub("-", "", names(mergeData))

# 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[mergeLabel[, 1], 2]
mergeLabel[, 1] <- activityLabel
names(mergeLabel) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names.
names(mergeSubject) <- "subject"
cleanData <- cbind(mergeSubject, mergeLabel, mergeData)
write.table(cleanData, "merged_data.txt")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
tidyData <- ddply(cleanData, .(subject, activity), limitedColMeans)
names(tidyData)[-c(1,2)] <- paste0("Mean", names(tidyData)[-c(1,2)])
write.table(tidyData, "tidy_data.txt", row.names = FALSE)