# Peoject work for Getting and Cleaning Data in Coursera.org

# The dataset was downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# as a .zip file.
# The required files were extracted to the working directory
# The created file (uning write.table()) was written to the working directory

# There are 5 parts in this R script:

# 1. Merges the training and the test sets to create one data set.

temp1 <- read.table("subject_train.txt")
temp2 <- read.table("subject_test.txt")
S <- rbind(temp1, temp2)

temp1 <- read.table("X_train.txt")
temp2 <- read.table("X_test.txt")
X <- rbind(temp1, temp2)

temp1 <- read.table("y_train.txt")
temp2 <- read.table("y_test.txt")
Y <- rbind(temp1, temp2)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

feature <- read.table("features.txt")
Selected_features <- grep("-mean\\(\\)|-std\\(\\)", feature[, 2])
X <- X[, Selected_features]
names(X) <- feature[Selected_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activities"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numactivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numactivities), ]

row = 1
    for (s in 1:numSubjects) {
        for (a in 1:numactivities) {
            result[row, 1] = uniqueSubjects[s]
            result[row, 2] = activities[a, 2]
            temp <- cleaned[cleaned$subject==s & cleaned$activities==activities[a, 2], ]
            result[row, 3:numCols] <- colMeans(temp[, 3:numCols])
            row = row+1
        }
}
write.table(result, "final_dataset.txt")