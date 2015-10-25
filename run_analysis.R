run_analysis<-function() {
	#read in the individual training and test files
	trainA<-read.table("./UCI HAR Dataset/train/subject_train.txt")
	trainB<-read.table("./UCI HAR Dataset/train/X_train.txt")
	trainC<-read.table("./UCI HAR Dataset/train/y_train.txt")
	testA<-read.table("./UCI HAR Dataset/test/subject_test.txt")
	testB<-read.table("./UCI HAR Dataset/test/X_test.txt")
	testC<-read.table("./UCI HAR Dataset/test/y_test.txt")
	
	#combine the training and test measurement tables(step 1)
	dataA<-rbind(trainA, testA)
	dataB<-rbind(trainB, testB)
	dataC<-rbind(trainC, testC)

	#Subset the measurement data to only include mean and standard deviation (step 2)
	features<-read.table("./UCI HAR Dataset/features.txt")
	colnames(dataB)<-as.vector(features[,2])
	dataBSubset<-dataB[,grepl("\\bmean()\\b|\\bstd()\\b", colnames(dataB))]

	#Finish combining all tables (step 1 continued)
	dat<-cbind(dataA, dataBSubset, dataC)
	
	#name the remaining columns 
	colnames(dat)[1]<-"subject"
	colnames(dat)[ncol(dat)]<-"activitycode"
	
	#create a new column and add the activity names to it (step 3)
	activities<-read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)
	dat$activityname<-"unnamed"	
	for (i in 1:nrow(activities)) {
		dat$activityname[dat$activitycode==i]<-activities[i,2]
	}
	dat$activitycode<-NULL

	#Give appropriate text variable names (From week 4 lecture 1) (step 4)
	colnames(dat)<-tolower(colnames(dat))
	colnames(dat)<-gsub("-", "", colnames(dat))
	colnames(dat)<-gsub(" ", "", colnames(dat))
	colnames(dat)<-gsub("\\()", "", colnames(dat))
	colnames(dat)<-gsub("std", "standarddeviation", colnames(dat))

	#Create a tidy data set with the average of each variable for each activity and each subject
	if(!("dplyr" %in% installed.packages()[,"Package"])){
		install.packages("dplyr")
	}
	library(dplyr)
	tidyData<- dat %>% group_by(subject, activityname) %>% summarise_each(funs(mean))
	tidyData

}