##------------------------------------Getting and Cleaning Data Course Project------------------------------------##
#
#This R script does the fallowing tasks on Human Activity Recognition Using Smartphones Data Set
# Downloadable from: http://archive.ics.uci.edu/ml/machine-learning-databases/00240/
# Extract the dataset folder into your working directory
#
#  1-Merges the training and the test sets to create one data set.
#  2-Extracts only the measurements on the mean and standard deviation for each measurement.
#  3-Uses descriptive activity names to name the activities in the data set
#  4-Appropriately labels the data set with descriptive variable names.
#  5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#
#-----------------------------------------------------------------------------------------------------------------##
rm(list=ls())
# setting the working directory
path<-getwd()
setwd(paste0(path,"/UCI HAR Dataset"))

#--------------------------------Import both train and test tables----------------------------------#
#train
subject_train = read.table('./train/subject_train.txt',header=FALSE)
x_train       = read.table('./train/x_train.txt',header=FALSE)
y_train       = read.table('./train/y_train.txt',header=FALSE)

#test
subject_test = read.table('./test/subject_test.txt',header=FALSE)
x_test       = read.table('./test/x_test.txt',header=FALSE)
y_test       = read.table('./test/y_test.txt',header=FALSE)

# Features,class labels and their names
features     = read.table('./features.txt',header=FALSE)
activity_labels = read.table('./activity_labels.txt',header=FALSE)
#--------------------------------------------------------------------------------------------------#

#-------Setting Column Names for imported data, creating train and test data and merging them------#

# giving names to columns of train dataset
colnames(subject_train)  = "subject_ID"
colnames(x_train)        = features[,2]
colnames(y_train)        = "activity_ID"
# creating train table
train_table = cbind(y_train,subject_train,x_train);

# giving names to columns of test dataset
colnames(subject_test) = "subject_ID";
colnames(x_test)       = features[,2]; 
colnames(y_test)       = "activity_ID";
#creating  test table
test_table = cbind(y_test,subject_test,x_test);

# giving names to columns of activity type table
colnames(activity_labels)  = c('activity_ID','activity_type')

Merged_data=rbind(train_table,test_table)
#--------------------------------------------------------------------------------------------------#

#--------------------------------------Removing extra columns, adding activity lables--------------------------------------#
col_names=colnames(Merged_data)
# A vector for selecting columns
selected_cols = (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names))

# Removing columns
Data = Merged_data[selected_cols==TRUE]
# Creating new table with lables
Data = merge(Data,activity_labels,by='activity_ID',all.x=TRUE)
# New Table column Names
col_names  = colnames(Data)
#--------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------- Cleaning up the variable names---------------------------------------------#

for (i in 1:length(col_names)) 
{
  col_names[i] = gsub("\\()","",col_names[i])
  col_names[i] = gsub("-std$","StdDev",col_names[i])
  col_names[i] = gsub("-mean","Mean",col_names[i])
  col_names[i] = gsub("^(t)","time",col_names[i])
  col_names[i] = gsub("^(f)","freq",col_names[i])
  col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i])
  col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
  col_names[i] = gsub("[Gg]yro","Gyro",col_names[i])
  col_names[i] = gsub("AccMag","AccMagnitude",col_names[i])
  col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
  col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i])
  col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i])
}

# Replace column names with new ones
colnames(Data) = col_names;
#-------------------------------------------------------------------------------------------------------------------------#

#removing activity type
final_data  = Data[,names(Data) != 'activity_type'];

# adding average to final data with summerizaing
tidy_data    = aggregate(final_data[,names(final_data) != c('activity_ID','subject_ID')],by=list(activity_ID=final_data$activity_ID,subject_ID = final_data$subject_ID),mean)

tidy_data    = merge(tidy_data,activity_labels,by='activity_ID',all.x=TRUE);

# Export the tidyData set 
write.table(tidy_data, './tidy_data.txt',row.names=TRUE,sep='\t');


