##create one R script called run_analysis.R that does the following. 

#1.Merges the training and the test sets to create one data set.
data_x1<-read.table("UCI HAR Dataset/train/X_train.txt")
data_x2<-read.table("UCI HAR Dataset/test/X_test.txt")
X<-rbind(data_x1, data_x2)

data_s1<-read.table("UCI HAR Dataset/train/subject_train.txt")
data_s2<-read.table("UCI HAR Dataset/test/subject_test.txt")
S<-rbind(data_s1, data_s2)

data_y1<-read.table("UCI HAR Dataset/train/y_train.txt")
data_y2<-read.table("UCI HAR Dataset/test/y_test.txt")
Y<-rbind(data_y1, data_y2)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("UCI HAR Dataset/features.txt")
featuresWeWant <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, featuresWeWant]
names(X) <- features[featuresWeWant, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "CleanData.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

Uniq_Subjects <- unique(S)[,1]
No_Subjects <- length(unique(S)[,1])
No_Activities <- length(activities[,1])
No_Cols <- dim(cleaned)[2]
result <- cleaned[1:(No_Subjects*No_Activities), ]

row = 1
for (s in 1:No_Subjects) {
  for (a in 1:No_Activities) {
    result[row, 1] = Uniq_Subjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:No_Cols] <- colMeans(tmp[, 3:No_Cols])
    row = row+1
  }
}
write.table(result, "DataSet_Average.txt")