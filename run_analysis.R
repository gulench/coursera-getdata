#
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation 
#    for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
library(plyr)

merge.training.test.datasets = function() {
    data.test.X <- read.table("data/UCI HAR Dataset/test/X_test.txt")
    data.test.y <- read.table("data/UCI HAR Dataset/test/y_test.txt")
    data.test.s <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
    
    data.train.X <- read.table("data/UCI HAR Dataset/train/X_train.txt")
    data.train.y <- read.table("data/UCI HAR Dataset/train/y_train.txt")
    data.train.s <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
    
    data.merged.X <- rbind(data.test.X, data.train.X)
    data.merged.y <- rbind(data.test.y, data.train.y)
    data.merged.s <- rbind(data.test.s, data.train.s)
    
    list(X=data.merged.X, y=data.merged.y, s=data.merged.s)
}

extract.mean.stddev = function(df) {
    features <- read.table("data/UCI HAR Dataset/features.txt")
    mean.cols <- sapply(features[, 2], function(f) grepl("mean()", f, fixed = TRUE))
    stddev.cols <- sapply(features[, 2], function(f) grepl("std()", f, fixed = TRUE))
  
    extracted.df <- df[, (mean.cols | stddev.cols)]
    colnames(extracted.df) <- features[(mean.cols | stddev.cols), 2]
    
    extracted.df
}

name.activities = function(df) {
    
    activities <- read.table("data/UCI HAR Dataset/activity_labels.txt")

    colnames(df) <- "activity"
    #data.test.y$activity<-mapvalues(data.test.y$activity, from = activities[, 1], to = activities[, 2])
    df$activity<-mapvalues(df$activity, from = activities[, 1], to = activities[, 2])
    message("name done!")
    df
}

main = function() {
    message("1. Merging training and test datasets ...")
    data.merged <- merge.training.test.datasets()
    
    message("2. Extract mean and stddev columns ...")
    X <- extract.mean.stddev(data.merged$X)
    
    message("3. Name activities ...")
    y <- name.activities(data.merged$y)
    
    colnames(data.merged$s) <- c("subject")
    
    message("4. Combining ...")
    data.combined <- cbind(X, y, data.merged$s)
    
    message("5. means by subject and activity ...")
    varcount = ncol(X)
    data.tidy <- ddply(data.combined, .(subject, activity), function(d) colMeans(d[, 1:varcount]))
    
    write.csv(data.tidy, "data/UCI_HAR_tidy.csv", row.names=FALSE)
}
