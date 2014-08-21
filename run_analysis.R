## Course Project

## Getting and Cleaning Data

## André Miguel Monteiro

## verifies if the content of the data is not in the working directory.
## If not, downloads the zip file and unzip it
if(!file.exists("UCI HAR Dataset")){
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
                  ("getdata-projectfiles-UCI HAR Dataset.zip"))
    unzip("getdata-projectfiles-UCI HAR Dataset.zip")
}

## set the working directory to UCI HAR Dataset
setwd("./UCI HAR Dataset")

## set the working directory to test
setwd("./test")

## read the txt files in the directory and stores that in different variables
X_test <- read.table("X_test.txt")
Y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

## set the working directory to UCI HAR Dataset (the parent directory)
setwd("..")

## set the working directory to test
setwd("./train")

## read the txt files in the directory and stores that in different variables
X_train <- read.table("X_train.txt")
Y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")

## set the working directory to UCI HAR Dataset (the parent directory)
setwd("..")

## aggregate by column the test variables and train variables. After that, 
## aggreagates train and test by rows
test <- cbind(subject_test, Y_test, X_test)
train <- cbind(subject_train, Y_train, X_train)
data_set <- rbind(test, train)

## names the first and second column of data_set
colnames(data_set)[1] <- "Subject_ID"
colnames(data_set)[2] <- "Activity"

## reads the file "features.txt" and stores it in variable "features"
features <- read.table("features.txt")

## get the indexes of the variables in the data_set that corresponds to mean and
## std
## Observation: for mean I did not include meanFreq, just literaly "mean"
indMeanStd <- grep("\\<[Mm]ean\\>|\\<[Ss][Tt][Dd]\\>", as.character(features[,2]))


## transform the index values for "features" into the indexes of the "data_set"
ind_data_set <- sorted_ind_features+2

## extracts only the first two columns of "data_set" and the columns with "mean"
## and "std"
values_extracted <- data_set[, c(1,2,ind_data_set)]

## stores the labels of "features.txt" that were stored in features in the
## variable "labels_unprocessed"
labels_unprocessed <- features[indMeanStd,2]

## stores the information from "labels_unprocessed" as character in the variable
## "labels_unprocessed2"
labels_unprocessed2 <- as.character(labels_unprocessed)

## processes the raw names of the labels in:
## 1- eliminate "()"
## 2,3,4,5- transforms "t", "f", "Acc", "Gyro" to more descriptive names as
## time and adding _ at the end or at the beginnig, depending of when the names
## appear in the labels' name
labels_processed <- gsub("\\()","", labels_unprocessed2)
labels_processed <- gsub("^t","time_", labels_processed)
labels_processed <- gsub("^f","frequency_", labels_processed)
labels_processed <- gsub("Acc","_Accelerometer", labels_processed)
labels_processed <- gsub("Gyro","_Gyroscope", labels_processed)

## names from the 3rd to the last column with the labels that were obtained in
## previous steps
colnames(values_extracted)[3:length(values_extracted)] <- labels_processed

## read "activity_labels.txt" and stores the data in the variable 
## "activity_names"
activity_names <- read.table("activity_labels.txt")

## stores the data from the variable "values_extracted" in the variable
## "tidy_data_set1"
tidy_data_set1 <- values_extracted

## order the tidy_data_set1 by subject and activity
## Observation: the number of the rows are the ones before ordering the data set
tidy_data_set1 <- tidy_data_set1[with(tidy_data_set1, order(Subject_ID, 
                                                            Activity)),]


## names the Activity column of tidy_data_set1 with the activity labels 
## stored in the second column of the variable "activity_names"  
for(i in 1:length(activity_names[,2])){
    tidy_data_set1[which(tidy_data_set1$Activity == i),2] <- 
        as.character(activity_names[i,2])
}



#### creation of the first data set is finalized



## Creation of a second, independent tidy data set (step5)

## initialization of variables
ind <- numeric()
sets <- numeric()

## stores the indexes of the rows for each subjects and activity in the variable
## "ind"
## stores the number of rows for each subject and activity in the variable 
## "sets" by making use of the length from before and after catching the indexes

## Observation: I used the variable "values_extracted", because it doesn't have
## the activity named, Activity is only defined as numbers
k <- 1
for(i in 1:30){
    for (j in 1:6){
        len1 <- length(ind)
        ind <- append(ind,which(values_extracted$Activity == j 
                                & values_extracted$Subject_ID == i))
        len2 <- length(ind)
        sets <- append(sets, len2 - len1)
        k <- k + 1
    }
}

## stores the subject and the activity in the variable "subject_activity"
subject_activity <- numeric()
## the variable "k" is used to access the index of the variable 
## "subject_activity"
k <- 1
for(i in 1:30){
    for (j in 1:6){
        subject_activity[k] <- i
        k <- k + 1
        subject_activity[k] <- j
        k <- k + 1
    }
}

## creates a data frame for the second data set with 180 as number of rows
## (30 subjects * 6 activities) and the length of the "values_extracted" as the
## number of columns
tidy_data2 <- data.frame(matrix(nrow = 180, ncol = length(values_extracted)))

## initialization of variables
j <- 1
k <- 1
l <- 1
i <- 1
rows <- numeric()
for(k in 1:length(sets)){
    ## appends the rows indexes to the variable "rows" while the variable "j" is
    ## not bigger than the number of the value in "k" position of the variable 
    ## "sets"
    while(j <= sets[k]){
        rows <- append(rows, ind[i])    ## appends the rows index to the 
                                        ## variable rows                                    
        j <- j + 1
        i <- i + 1
    }
    ## stores in the "k" row of the data frame tidy_data2 the number 
    tidy_data2[k,] <- c(subject_activity[l], subject_activity[l+1], 
                        colMeans(data_set[rows, 3:length(data_set)]))
    l <- l + 2
    ## reinitializes the variables j and rows
    j <- 1
    rows <- numeric()
}

## names the columns
colnames(tidy_data2)[1:length(values_extracted)] <- c("Subject_ID", "Activity",
                                                      labels_processed)

## names the Activity column of tidy_data_set1 with the activity labels 
## stored in the second column of the variable "activity_names"
for(i in 1:length(activity_names[,2])){
    tidy_data2[which(tidy_data2$Activity == i),2] <- 
        as.character(activity_names[i,2])
}

## set the working directory to the main directory
setwd("..")

## write the table to a txt file called tidydata2.txt
write.table(tidy_data2, "./tidydata2.txt", row.names = FALSE)
