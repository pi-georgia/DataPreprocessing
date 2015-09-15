# The script below does the following: 
# 1. Merges the training and the test sets to create one data set.  
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.   
# 3. Uses descriptive activity names to name the activities in the data set  
# 4. Appropriately labels the data set with descriptive variable names.   
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

#Acknowledgements
#Dataset used is by [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. 
#International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012



# Get the names of the activities that correspond to classes by reading 'activity_labels.txt': Links the class labels with their activity name.
activity_labels<- fread("./UCI HAR Dataset/activity_labels.txt")

# Reading 'train/X_train.txt': Training set.
    trainset<- read.table("./UCI HAR Dataset/train/X_train.txt")
 
# Reading 'train/y_train.txt': Training labels.
    trainnames<- fread("./UCI HAR Dataset/train/y_train.txt")
 
 #Read subject info
 subjecttrain<- fread("./UCI HAR Dataset/train/subject_train.txt")

# Reading 'test/X_test.txt': Test set.
    testset<- read.table("./UCI HAR Dataset/test/X_test.txt")

# Reading 'test/y_test.txt': Test labels.
    testnames<- fread("./UCI HAR Dataset/test/y_test.txt")
 #Read subject info
  subjecttest<- fread("./UCI HAR Dataset/test/subject_test.txt")
#Merge in phases : Activity Class with Description for train sample, then test 
library(dplyr)
 
 #create indexes, preparing for join
 test_size<- length(testset[,1]) ;  train_size<- length(trainset[,1])
 
 trainset$ID <- seq(1,train_size, by=1) ; trainnames$ID<- seq(1,train_size, by=1) ; subjecttrain$ID<- seq(1,train_size, by=1)
 testset$ID <- seq(1,test_size, by=1) ; testnames$ID<- seq(1,test_size, by=1) ; subjecttest$ID<- seq(1,test_size, by=1)
 
 #rename V1 in testnames/trainnames to class to avoid merging on generic V1. Same for subjectnames
 names(testnames)[1]<-"class" ; names(trainnames)[1]<-"class"
 names(subjecttest)[1]<-"subject" ;  names(subjecttrain)[1]<-"subject"

 #create lists, for multiple mergin
 trainlist <-list(trainset, trainnames, subjecttrain) ; testlist <-list(testset, testnames, subjecttest)

 #merge all based on their commony key = ID (automatically selected by join_all)
 library(plyr)
 train <- join_all(trainlist) ; test <- join_all(testlist)
 
 #Create a universal index across test and train : move index for table "test" by the size of table "train"
 test$ID<- test$ID+length(train[,1])
 #final merge/ as I want every measurement to remain on a separate row, 
 #I am building up a dataframe in which the first part is my trainset and the second part is my test set
 dataset<- train
 dataset[length(train[,1]) :(length(train[,1])+ length(test[,1])) , ]<- test
 
#Validate that everything went as planned by comparing before after sizes
 if( length(dataset[,1]) == (length(train[,1])+ length(test[,1]))) print("ok")

#Get the measurements names by reading the 'features.txt': List of all features.
feature_names <-fread("./UCI HAR Dataset/features.txt")
 
#rename my dataset features with their names as mentioned in the features.txt
names(dataset)[1:561] <-feature_names$V2

#apply Regular Expression for names that contain either "mean" or Mean or "std"
index<-grep("[Mm]ean|std", names(dataset))
#subset my dataset keeping only the desired, values
sub_set<-dataset[,index] ; sub_set$ID <- dataset$ID; sub_set$class <- dataset$class; sub_set$subject <- dataset$subject

            
# Get the descriptive names of the activities that correspond to classes by reading 'activity_labels.txt': Links the class labels with their activity name.
activity_labels<- fread("./UCI HAR Dataset/activity_labels.txt")
names(activity_labels)<-c("class", "activity_description")

#Well I have 2 data sets so I will merge them both with the activity_labels
dsc_dataset<-left_join(dataset,activity_labels)
dsc_subset<-left_join(sub_set,activity_labels)


#Create a new key that identifies Activity per Subject
tidy<- dsc_subset
tidy$Subject_Activity <- paste(tidy$subject,"-", tidy$activity_description)
#cleanup to keep only numerical values
subm<- tidy[, 1:87]
# rebuild by adding Subject Activity information
subm$SA<-tidy$Subject_Activity
subject_monitor<- group_by(subm, SA) %>% summarise_each(funs(mean))

# export data
write.table(dataset, "tidy_dataset.txt", row.name=FALSE)
