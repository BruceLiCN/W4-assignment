CodeBook
==================================================
Variable list and descriptions
------------------------------


Varible Name	| Description
----------------|--------------
finalData	| the data set we got by merging the training and the test sets
chosenCol	| The collumns with mean() or std()
finalDataSet1	| the data set in step 4
finalDataSet2	| the data set in step 5

other variables (variable name is the same as the name of the file that it was read from)
activity_labels,
features,
x_test,
y_test,
subject_test,
x_train,
y_train,
subject_train,
Analysis Process
----------------

The run_analysis.R script performs the following steps: 

* 1. Merges the training and the test sets to create one data set.

* 2. Extracts only the measurements on the mean and standard deviation for each measurement.

* 3. Uses descriptive activity names to name the activities in the data set.

* 4. Appropriately labels the data set with descriptive activity names.

* 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##The code:

##Merge the training and the test sets to create one data set.
data_train<- cbind(subject_train,y_train,x_train)
data_test<- cbind(subject_test,y_test,x_test)
finalData <- rbind(data_train,data_test)

##label the data set with descriptive variable names
finalColNames <- c('subject','activity',features[,2])
names(finalData) <- finalColNames

##Use descriptive activity names to name the activities in the data set
finalData[,2]<-activity_labels[finalData[,2],2]
rm(x_test,x_train,y_test,y_train,subject_test,subject_train,data_test,data_train,features,activity_labels)


##choose the mean and std collumns
meanCol<- grep('mean()',finalColNames)
stdCol<- grep('std()',finalColNames)
chosenCol<- c(1,2,stdCol,meanCol)

##the data set required in step 4 finished 
finalDataSet1<- finalData[,chosenCol]

##create a data set.  Its first collumn is subject, second collumn is activity, and other collumns similar to the data set in step 4, but all these collumns except the first two are empty, which we will fill later.
library('dplyr')
s121<-summarize(group_by(finalDataSet1, subject, activity), mean(subject,na.rm=TRUE))
s12<-as.data.frame(s121)[,c(1,2)]
finalDataSet2<- cbind(s12,matrix(,180,79))

##fill those empty collumns 
for(j in c(3:81)){
	for(i in c(1:180)){
	  ##get the subject and activity of this blank
		SUB<-finalDataSet2[i,1]
		ACT<-finalDataSet2[i,2]
		##calculate the mean and fill it in the blank
		MEAN<-mean(finalDataSet1[(finalDataSet1[,1]==SUB)&(finalDataSet1[,2]==ACT),j])
		finalDataSet2[i,j]<-MEAN
	} 
}
## add names
names(finalDataSet2)<-names(finalDataSet1)

View(finalDataSet1)
View(finalDataSet2)

## create the file required in step5
write.table(finalDataSet2, file = "w4_tidy_avg_dataset.txt", row.name = FALSE)
