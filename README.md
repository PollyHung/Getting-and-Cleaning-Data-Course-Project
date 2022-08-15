# Getting-and-Cleaning-Data-Course-Project

##loading the data into each variables: 
activity_labels <- activity_labels.txt
col_names <- features.txt
test_set <- X_test.txt
test_label <- y_test.txt
subject_test <- subject_test.txt
train_set <- X_train.txt
train_label <- y_train.txt
subject_train <- subject_train.txt

##changed the columns names 
colnames(test_set) <- col_names$V2
colnames(test_label) <- "label" 
colnames(subject_test) <- "subject"
colnames(train_set) <- col_names$V2
colnames(train_label) <- "label"
colnames(subject_train) <- "subject"


##Merges the data in 2 steps 
#merge all the test data, and add a head column explaining that these are test data 
#merge all the train data, and add a head column explaining that these are train data 
test_set <- add_column(test_set, test_label, 
                       .before = "tBodyAcc-mean()-X", .name_repair = "minimal")
test_set <- add_column(test_set, subject_test, 
                       .before = "tBodyAcc-mean()-X", .name_repair = "minimal")
test_or_train <- rep("test", times = 2947)
test_set <- add_column(test_set, test_or_train, 
                       .before = "label", .name_repair = "minimal")
train_set <- add_column(train_set, train_label, 
                        .before = "tBodyAcc-mean()-X", .name_repair = "minimal")
train_set <- add_column(train_set, subject_train, 
                        .before = "tBodyAcc-mean()-X", .name_repair = "minimal")
test_or_train <- rep("train", times = 7352)
train_set <- add_column(train_set, test_or_train, 
                        .before = "label", .name_repair = "minimal")

##replace the test_labels and train_set with real names in activity_labels 
x <- c(1, 2, 3, 4, 5, 6)
for (i in x){
  test_set["label"][test_set["label"] == i] <- activity_labels$V2[i]
}
x <- c(1, 2, 3, 4, 5, 6)
for (i in x){
  train_set["label"][train_set["label"] == i] <- activity_labels$V2[i]
}

##merge two data frames vertically 
complete <- rbind(test_set, train_set)

##Extracts only the measurements on the mean and standard deviation for each measurement and store them in a new table called mean_and_std
average <- grep("[Mm][Ee][Aa][Nn]", names(complete), value=TRUE)
standard_dev <- grep("[Ss][Tt][Dd]", names(complete), value = TRUE)
mean_and_std <- select(complete, c(label, subject, all_of(average), all_of(standard_dev)))

##Uses descriptive activity names to name the activities in the data set
summarise <- mean_and_std %>%
  group_by(subject, label) %>%
  summarise_if(is.numeric, mean)
