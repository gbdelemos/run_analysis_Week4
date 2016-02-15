rm(list=ls(all=T))

##Dowloading file
# url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# filename <- "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# destfile <- paste0("C:/Users/GabrielBruno/Desktop/Specialization_Data Science/Getting and Cleaning Data/",filename)
# download.file(url,destfile)

##Loading database
library("data.table")
filepath <- "C:/Users/GabrielBruno/Desktop/Specialization_Data Science/Getting and Cleaning Data/UCI HAR Dataset/"

X_train <- read.table(paste0(filepath,"train/X_train.txt"), quote="\"", comment.char="")
Y_train <- read.table(paste0(filepath,"train/y_train.txt"), quote="\"", comment.char="")

X_test <- read.table(paste0(filepath,"test/X_test.txt"), quote="\"", comment.char="")
Y_test <- read.table(paste0(filepath,"test/y_test.txt"), quote="\"", comment.char="")

subject_test <- read.table(paste0(filepath,"test/subject_test.txt"), quote="\"", comment.char="")
subject_train <- read.table(paste0(filepath,"train/subject_train.txt"), quote="\"", comment.char="")

activity_labels <- read.table(paste0(filepath,"activity_labels.txt"), quote="\"", comment.char="")
features <- read.table(paste0(filepath,"features.txt"), quote="\"", comment.char="")

##Doing some first checks
table(Y_test)
table(subject_test)

table(Y_train)
table(subject_train)

## Step 1. Merge datasets
X_all <- dplyr::bind_rows(X_test,X_train)
subject_all <- dplyr::bind_rows(subject_test,subject_train)
names(subject_all) <- "subject"
labels_all <- dplyr::bind_rows(Y_test,Y_train)

##Step 4. Labeling variable names
names(labels_all) <- "labels"
names(X_all) <- features$V2

##Step 2. Finding variables related with mean and std
mean.variables <- grep("mean",features$V2)
std.variables <- grep("std",features$V2)

X_all <- X_all[,c(mean.variables,std.variables)]

df <- dplyr::bind_cols(labels_all,subject_all,X_all)
temp <- merge.data.frame(x = df,y = activity_labels,by.x = "labels",by.y ="V1")

##Step 3. Labeling activities
df$activity_labels <- temp$V2
df <- dplyr::select(df,labels,activity_labels,subject,3:81)

## Step 5. Creating the tidy data set
agg.function <- function(i){
  aggregate(df[,i],by=list(df$activity_labels,df$subject),FUN=mean)
}

tidy.dataset <- data.frame(lapply(X = list(4:82),FUN = agg.function))
names(tidy.dataset)[1:2] <- names(df)[1:2]
names(tidy.dataset)[3:81] <- paste0("Avg.of.",names(df))[4:82]

write.table(tidy.dataset,file="tidy.dataset.txt",sep=" ",col.names=TRUE)


