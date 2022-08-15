#Getting and Cleaning Data Course Project
#The purpose of this project is to demonstrate your ability to collect, work with, 
#and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 
        #1) a tidy data set as described below, 
        #2) a link to a Github repository with your script for performing the analysis
        #3) a code book that describes the variables, the data, and any transformations 
              #or work that you performed to clean up the data called CodeBook.md. 
              #You should also include a README.md in the repo with your scripts. 
              #This repo explains how all of the scripts work and how they are connected.

#One of the most exciting areas in all of data science right now is wearable computing - 
#see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to 
#develop the most advanced algorithms to attract new users. The data linked to from the 
#course website represent data collected from the accelerometers from the Samsung Galaxy S 
#smartphone. A full description is available at the site where the data was obtained:
        #http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#Here are the data for the project:
        #https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  

#You should create one R script called run_analysis.R that does the following. 
      #Merges the training and the test sets to create one data set.
      #Extracts only the measurements on the mean and standard deviation for each measurement. 
      #Uses descriptive activity names to name the activities in the data set
      #Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.


##Preparations...
#library the necessary packages 
library(dplyr)
library(tibble)
library(tidyr)

#create a folder in computer to store the data 
if(!file.exists("data")){ 
  dir.create("data")
}

#download the data from the internet 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/samsung_galaxy.zip", method = "curl")
list.files("./data")

#unzip the data 
dir.create("./data/samsung_galaxy") #we create an empty folder for the files 
out_dir <- "./data/samsung_galaxy" #the directory we want to extract our files to 
unzip("./data/samsung_galaxy.zip", exdir = out_dir) #unzipping and extracting......


##loading data: 
#load activity labels 
activity_labels <- read.table("./data/samsung_galaxy/UCI HAR Dataset/activity_labels.txt")

#load data column names 
col_names <- read.table("./data/samsung_galaxy/UCI HAR Dataset/features.txt")

#load test sets 
test_set <- read.table("./data/samsung_galaxy/UCI HAR Dataset/test/X_test.txt")
test_label <- read.table("./data/samsung_galaxy/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/samsung_galaxy/UCI HAR Dataset/test/subject_test.txt")

#load train sets 
train_set <- read.table("./data/samsung_galaxy/UCI HAR Dataset/train/X_train.txt")
train_label <- read.table("./data/samsung_galaxy/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/samsung_galaxy/UCI HAR Dataset/train/subject_train.txt")

#add column names 
colnames(test_set) <- col_names$V2
colnames(test_label) <- "label" ##keeping consistent labels so easy for vertical binding 
colnames(subject_test) <- "subject"
colnames(train_set) <- col_names$V2
colnames(train_label) <- "label"
colnames(subject_train) <- "subject"


##Merges the training and the test sets to create one data set:
#merge test_label and subject_test with test_set 
test_set <- add_column(test_set, test_label, 
                       .before = "tBodyAcc-mean()-X", .name_repair = "minimal")
test_set <- add_column(test_set, subject_test, 
                       .before = "tBodyAcc-mean()-X", .name_repair = "minimal")
test_or_train <- rep("test", times = 2947)
test_set <- add_column(test_set, test_or_train, 
                       .before = "label", .name_repair = "minimal")

#merge train_label and subject_train with train_set 
train_set <- add_column(train_set, train_label, 
                        .before = "tBodyAcc-mean()-X", .name_repair = "minimal")
train_set <- add_column(train_set, subject_train, 
                        .before = "tBodyAcc-mean()-X", .name_repair = "minimal")
test_or_train <- rep("train", times = 7352)
train_set <- add_column(train_set, test_or_train, 
                        .before = "label", .name_repair = "minimal")

#Remove unnecessary objects 
rm(subject_test)
rm(subject_train)
rm(test_label)
rm(train_label)
rm(col_names)
rm(test_or_train)

#replace the test_labels and train_set with real names in activity_labels 
x <- c(1, 2, 3, 4, 5, 6)
for (i in x){
  test_set["label"][test_set["label"] == i] <- activity_labels$V2[i]
}
x <- c(1, 2, 3, 4, 5, 6)
for (i in x){
  train_set["label"][train_set["label"] == i] <- activity_labels$V2[i]
}

#merge two data frames vertically 
names(test_set) == names(train_set) ##to see if two data frames have same variable name 
complete <- rbind(test_set, train_set)


##Extracts only the measurements on the mean and standard deviation for each measurement. 
#mean(), std()
average <- grep("[Mm][Ee][Aa][Nn]", names(complete), value=TRUE)
standard_dev <- grep("[Ss][Tt][Dd]", names(complete), value = TRUE)

#new table 
mean_and_std <- select(complete, c(label, subject, all_of(average), all_of(standard_dev)))
names(mean_and_std)


##Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
summarise <- mean_and_std %>%
  group_by(subject, label) %>%
  summarise_if(is.numeric, mean)

#to get your data to local disc
install.packages("writexl")
library("writexl")
write_xlsx(summarise,"./data/samsung_galaxy\\summarise.xlsx")
