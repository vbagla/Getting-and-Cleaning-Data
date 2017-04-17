run_analysis <- function () {

# ------------------------
## Read all files from directory

# Training tables
x_train <- read.table("./UCI_HAR_Dataset/train/x_train.txt")
y_train <- read.table("./UCI_HAR_Dataset/train/y_train.txt")
subject_train <- read.table("./UCI_HAR_Dataset/train/subject_train.txt")

# Testing tables
x_test <- read.table("./UCI_HAR_Dataset/test/x_test.txt")
y_test <- read.table("./UCI_HAR_Dataset/test/y_test.txt")
subject_test <- read.table("./UCI_HAR_Dataset/test/subject_test.txt")

# Features table
features <- read.table("./UCI_HAR_Dataset/features.txt")

# Activity labels
activity_labels <- read.table("./UCI_HAR_Dataset/activity_labels.txt")

# ------------------------
# create column names for the training and test files 
colnames(x_train) <- features[,2]
colnames(y_train) <- "ActivityID"
colnames(subject_train) <- "SubjectID"	

colnames(x_test) <- features[,2]
colnames(y_test) <- "ActivityID"
colnames(subject_test) <- "SubjectID"	

colnames(activity_labels) <- c("ActivityID", "ActivityType")

# ------------------------
## 1. Merges the training and the test sets to create one data set.

all_train <- cbind(y_train, subject_train, x_train)
all_test <- cbind(y_test, subject_test, x_test)
all_data <- rbind(all_train, all_test)

# ------------------------
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

header_names <- colnames(all_data)

meanSTD <- (grepl ("ActivityID", header_names) | grepl ("SubjectID", header_names) | 
		grepl ("mean", header_names) | grepl ("std", header_names))

# ------------------------
## 3. Uses descriptive activity names to name the activities in the data set

meanSTD_sub <- all_data[, meanSTD==TRUE]

# ------------------------
## 4. Appropriately labels the data set with descriptive variable names.

activity_names <- merge(meanSTD_sub, activity_labels, by='ActivityID', all.x=TRUE)

# ------------------------
## 5. From the data set in step 4, creates a second, independent tidy data set 
## 	with the average of each variable for each activity and each subject.

tidydata <- aggregate(. ~SubjectID + ActivityID, activity_names, mean)
write.table (tidydata, "tidydata.txt")

}
