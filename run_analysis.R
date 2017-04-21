library(dplyr)

act_label <- function(x){
   if(x ==1)
      "WALKING"
   else if(x ==2)
      "WALKING_UPSTAIRS"
   else if(x ==3)
      "WALKING_DOWNSTAIRS"
   else if(x ==4)
      "SITTING"
   else if(x ==5)
      "STANDING"
   else if(x ==6)
      "LAYING"
}

features <- read.table("features.txt")

# Load test data
test_data <- read.table("test/X_test.txt", header = F, sep = "", colClasses = "numeric")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")
names(test_data) <-features[,2]
names(y_test) <- "activity"
names(subject_test) <- "subject"
test_data <- cbind(test_data,y_test, subject_test)

# Load train data
train_data <- read.table("train/X_train.txt", header = F, sep = "", colClasses = "numeric")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")
names(train_data) <-features[,2]
names(y_train) <- "activity"
names(subject_train) <- "subject"
train_data <- cbind(train_data,y_train, subject_train)

# Combine test and train data
data0 <- rbind(test_data, train_data)

# Only keep fields with mean or std dev
mean_or_std_cols <- grep("mean\\(|std\\(", features[,2])
data <- data0[,c(mean_or_std_cols,562,563)]

# Add activity labels
data_formatted <- cbind(data[1:66],sapply(data$activity,act_label), data$subject)
names(data_formatted)[c(67,68)] <- c("activity","subject")

# Form tidy data set with the average of each variable for each activity and each subject.
results <- data_formatted %>%
           group_by_(.dots=c(as.symbol("activity"),as.symbol("subject"))) %>%
           summarise_each(funs(mean(., na.rm=TRUE)))

write.table(results,"GettingAndCleaningDataFinalProject.txt",row.name=FALSE)
