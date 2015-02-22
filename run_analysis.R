#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "data.zip")
#unzip("data.zip")
#setwd("./UCI HAR Dataset/")

###The working directory is set to 'UCI HAR Dataset'
library(plyr)

train_data_features<-read.table("train/X_train.txt",colClasses = "numeric")   #read in the training set
test_data_features<-read.table("test/X_test.txt",colClasses = "numeric")      #read in the test set

merged_data_features<-rbind(train_data_features,test_data_features)	# Merges the training and the test sets to create one data set
feature_names<-read.table("features.txt",stringsAsFactors=FALSE)   # read in the header names for the 561 features 
names(merged_data_features)<-feature_names[,2]			   # Assign the feature names to the merged data

## Now read in the subject identifier data for the training and the test datasets and merge them.
subject_train<-read.table("./train/subject_train.txt",colClasses = "numeric")
subject_test<-read.table("./test/subject_test.txt",colClasses = "numeric")
subject_identifier<-factor(c(subject_train[,1],subject_test[,1]))

#Read in the activites for the training set and the test set and merge them, as done above for the subject identifier data.
train_data_activity<-read.table("train/y_train.txt",colClasses = "numeric") 
test_data_activity<-read.table("test/y_test.txt",colClasses = "numeric")    
merged_activity<-c(train_data_activity[,1],test_data_activity[,1])

##########Merge the training and the test sets to create one data set ##########
raw_data_full<-cbind(merged_activity, subject_identifier, merged_data_features)

## Now identify the column names which contain the means and standard deviations of different measurements, and combine them.
cols_containing_mean_measurements<-grep("mean",names(merged_data_features), ignore.case=TRUE) 
cols_containing_std_measurements<-grep("std",names(merged_data_features), ignore.case=TRUE)   
relevant_cols<-sort(c(cols_containing_mean_measurements,cols_containing_std_measurements))  #Sort the columns by their occurence in the raw data frame. The sorting is not necessary though.

##########  Extract the subset of the data which contains only the measurements on the mean and standard deviation for each measurement. ########## 
mean_std_data<-merged_data_features[,relevant_cols]	

########## Use descriptive activity names to name the activities in the data set########## 
activity_labels<-read.table("activity_labels.txt",stringsAsFactors = FALSE)	#Read in the names of the different activities in the data set.
activity<-factor(merged_activity,labels = activity_labels[,2])		# Convert the activity vector into a factor variable and use descriptive activity names to name the activities in the data set.


########## Now appropriately label the data set with descriptive variable name  ##########
# Use the gsub function to expand the abbreviated names.

names(mean_std_data)<-gsub("^f", "frequency", names(mean_std_data)) # Variable names beginning with 'f' are frequency-domain data 
names(mean_std_data)<-gsub("^t", "time", names(mean_std_data))        # Variable names beginning with 't' are time series data 
names(mean_std_data)<-gsub("std", "standard_deviation", names(mean_std_data))
names(mean_std_data)<-gsub("Acc", "Accelerometer", names(mean_std_data))
names(mean_std_data)<-gsub("Gyro", "Gyroscope", names(mean_std_data))
names(mean_std_data)<-gsub("Mag", "Magnitude", names(mean_std_data))
names(mean_std_data)<-gsub("\\(t", "\\(time", names(mean_std_data))

########## Finally, create a tidy data set with the average of each variable for each activity and each subject ##########
#I have used the ddply function from 'plyr' package for convenience. However, the same task can alternatively be performed with the 'by' function, but that does not produce a data frame.##########
tidy_dataset_average <- ddply(mean_std_data, .(activity, subject_identifier), colMeans)

##########  Write out the tidy data set in a .txt file  ########## 

write.table(tidy_dataset_average,file = "tidy_data_set.txt",row.names = FALSE)   




## cleaned_data<-by(mean_std_data,INDICES = list(activity_type,subject_identifier),FUN = colMeans)

