read_data <- function() {
        activity_labels <- NULL
        features <- NULL
        X_train <- NULL
        y_train <- NULL
        subject_train  <- NULL
        X_test <- NULL
        y_test <- NULL
        subject_test <- NULL
        setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset")
        get_activitylables <- function() {
                setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset")
                activity_labels <<- read.table("activity_labels.txt")
        }
        get_features <- function() {
                setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset")
                features <<- read.table("features.txt")
        }
        
        get_Xtrain <- function() {
                setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train")
                X_train <<- read.table("X_train.txt")
                
        }
        get_ytrain <- function() {
                setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train")
                y_train <<- read.table("y_train.txt")
        }
        get_subjecttrain <- function() {
                setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train")
                subject_train <<- read.table("subject_train.txt")
                
        }
        
        
        get_Xtest <- function() {
                setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test")
                X_test <<- read.table("X_test.txt")
                
        }
        get_ytest <- function() {
                setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test")
                y_test <<- read.table("y_test.txt")
        }
        get_subjecttest <- function() {
                setwd("C:\\Coursera\\Getting\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test")
                subject_test <<- read.table("subject_test.txt")
        }
        #colnames(activity_labels) <-c("activity_num", "activity_name") 
        #a <- get_Xtest()
        #a
        #1. Merger
        #X_train$activity_num <-  y_train 
        #X_train$activity_num
        list(
                get_activitylables=get_activitylables,
                get_features=get_features,
                get_Xtrain=get_Xtrain,
                get_ytrain=get_ytrain,
                get_subjecttrain =get_subjecttrain,
                get_Xtest=get_Xtest,
                get_ytest=get_ytest,
                get_subjecttest=get_subjecttest
        )
}

run_analysis <- function(x = read_data ()) {
        activity_labels <- x$get_activitylables()
        features <- x$get_features()
        X_train <- x$get_Xtrain()
        y_train <- x$get_ytrain()
        subject_train  <- x$get_subjecttrain()
        X_test <- x$get_Xtest()
        y_test <- x$get_ytest()
        subject_test <- x$get_subjecttest()
        
        #1.Merge
        X_train$activity_num <- y_train$V1
        X_train$subject_num<- subject_train$V1
        X_test$activity_num <- y_test$V1
        X_test$subject_num <- subject_test$V1
        merger <- rbind(X_train, X_test)
        
        #2 Extracts only the measurements on the mean and standard deviation for each measurement. 
        colnames(merger) <- c(as.vector(features$V2),"activity_num", "subject_num")
        extract <- merger[,grepl("[mM]ean\\(\\)|[sS]td\\(\\)|activity|subject",colnames(merger))]
        
        #3 Uses descriptive activity names to name the activities in the data set
        colnames(activity_labels) <-c("activity_num", "activity_name")
        activity <- merge(extract, activity_labels, by.x="activity_num", by.y="activity_num")
        
        #4 Appropriately labels the data set with descriptive variable names.
        colnames(activity) <- gsub(pattern="\\(|\\)|-|,","",colnames(activity))
        
        #5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
        average <- aggregate(activity, by=list(activity$activity_name, activity$subject_num), FUN="mean")
        write.table(average,"average.txt")
}
run_analysis(cacheData)



