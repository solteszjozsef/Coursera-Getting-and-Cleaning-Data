# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

loaddata1 <- function(){

  testdata <- read.table("UCI HAR Dataset/test/X_test.txt")
  traindata <- read.table("UCI HAR Dataset/train/X_train.txt")
  rawdata<<-rbind(testdata,traindata)
  
  
  testlabels<- read.table("UCI HAR Dataset/test/y_test.txt")
  trainlabels<- read.table("UCI HAR Dataset/train/y_train.txt")
  rawlabels<<-rbind(testlabels,trainlabels)

  testsubjects<- read.table("UCI HAR Dataset/test/subject_test.txt")
  trainsubjects<- read.table("UCI HAR Dataset/train/subject_train.txt")
  rawsubjects<<-rbind(testsubjects,trainsubjects)
  
  res<-paste("Raw data loaded",nrow(rawdata),ncol(rawdata),sep=", ")
  res<-rbind(res,paste("Labels loaded",nrow(rawlabels),ncol(rawlabels),sep=", "))
  res<-rbind(res,paste("Subjects loaded",nrow(rawsubjects),ncol(rawsubjects),sep=", "))
  return(res)
  
  
}

loaddata2 <- function(){
  
  features<<- read.table("UCI HAR Dataset/features.txt")
  cleandata<<-rawdata
  colnames(cleandata)<<-features$V2
  cleandata<<-cleandata[,c(grep("-mean()",features$V2, fixed=TRUE),grep("-std()",features$V2, fixed=TRUE))]

  labelnames<<- read.table("UCI HAR Dataset/activity_labels.txt")
  cleanlabels<<-data.frame(labelnames[rawlabels$V1,2])
  names(cleanlabels)<<-"activity"
  
  cleansubjects<<-rawsubjects  
  names(cleansubjects)<<-"subject"
  
  cleandata<<-cbind(cleanlabels, cleansubjects, cleandata)
  return(paste("Mean and SD variables selected",nrow(cleandata),ncol(cleandata),sep=", "))
}

loaddata3<-function(){
  tidydata<<-aggregate(cleandata[,3:ncol(cleandata)], list(cleandata$activity, cleandata$subject), mean)
  tidydata<<-aggregate(. ~ activity + subject, data = cleandata, mean)
    return(paste("Tidy data done",nrow(tidydata),ncol(tidydata),sep=", "))
}

exportdata<-function(){
  write.table(tidydata,"tidydata.txt",row.names=FALSE)
  return("Tidy data exported")
}
loaddata666 <- function(){
  
testlabels<- read.table("UCI HAR Dataset/test/y_test.txt")
names(testlabels)<-"Labels"
labels<- read.table("UCI HAR Dataset/activity_labels.txt")
testlabels<-data.frame(labels[testlabels$Labels,2])
names(testlabels)<-"Labels"

testfeatures<- read.table("UCI HAR Dataset/features.txt")


colnames(testdata)<-testfeatures$V2
testdata<-cbind(testlabels,testdata)




trainlabels<- read.table("UCI HAR Dataset/train/y_train.txt")
names(trainlabels)<-"Labels"
labels<- read.table("UCI HAR Dataset/activity_labels.txt")
trainlabels<-data.frame(labels[trainlabels$Labels,2])
names(trainlabels)<-"Labels"

trainfeatures<- read.table("UCI HAR Dataset/features.txt")

colnames(traindata)<-trainfeatures$V2
traindata<-cbind(trainlabels,traindata)

fulldata<-rbind(testdata,traindata)
return()
}
