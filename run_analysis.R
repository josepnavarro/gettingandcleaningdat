########################################################################################################## 
# Source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
#
# This R does the following  
# 1. Merges the training and the test sets to create one data set. 
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.   
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.  
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  
#
##########################################################################################################
# 1. Merge the training and the test sets to create one data set

# Clean Environment workspace
rm(list=ls())

#set working directory to the source location
setwd("~/Formación/BigData/2015-01 Getting & Cleaning Data/UCI HAR Dataset")

# Read the data from features TXT files
features     = read.table('features.txt',header=FALSE);
activityType = read.table('activity_labels.txt',header=FALSE);

# Read the data from training TXT files
subjectTrain = read.table('train/subject_train.txt',header=FALSE);
xTrain       = read.table('train/x_train.txt',header=FALSE);
yTrain       = read.table('train/y_train.txt',header=FALSE);

# Assign column names to training loaded data
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2];
colnames(yTrain)        = "activityId";

# Create the merged training data set by yTrain, subjectTrain & xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read the data from test TXT files
subjectTest = read.table('test/subject_test.txt',header=FALSE);
xTest       = read.table('test/x_test.txt',header=FALSE);
yTest       = read.table('test/y_test.txt',header=FALSE);

# Assign column names to test loaded data
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2];
colnames(yTest)       = "activityId";

# Create the merged test data set by yTest, subjectTest & xTest
testData = cbind(yTest,subjectTest,xTest); 

# Combine training and test data
MergedData = rbind(trainingData,testData);
colNames  = colnames(MergedData);

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Obtain a vector that contains TRUE for the IDs, *mean* & *std* columns
columns = (grepl("activity*",colNames) | grepl("subject*",colNames) | grepl("*mean*",colNames) | grepl("*std*",colNames))

# Subset columns only desired columns vector
MergedData = MergedData[columns==TRUE];

# 3. Uses descriptive activity names to name the activities in the data set.
# Merge the MergedData set with the activityType table to include the activities name
MergedData = merge(MergedData,activityType,by='activityId',all.x=TRUE);
colNames  = colnames(MergedData);

# 4. Appropriately labels the data set with descriptive variable names  
for (i in 1:length(colNames))
{
  colNames[i] = gsub("[-()]","",colNames[i])
  colNames[i] = gsub("-std","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
}; 

colnames(MergedData) = colNames;

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
MergedData$activityId <- as.factor(MergedData$activityId) 
MergedData$subjectId <- as.factor(MergedData$subjectId) 

# Summarizing the MergedData table to calculate the mean of each variable for each activityId and each subjectId
tidyData = aggregate(MergedData, by=list(activity = MergedData$activityId, subject=MergedData$subjectId), mean) 

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Remove the Id columns 
tidyData  = tidyData[,names(tidyData) != c('activityId','subjectId')]; 

# Export tidyData set
write.table(tidyData, 'tidyData.txt',row.names=FALSE,sep='\t');