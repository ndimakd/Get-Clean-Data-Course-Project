# Code Book (Get-Clean-Data-Course-Project)

### Data for project and initial Code Book:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Tidy data contains one table with columns:
 [1] "subject"                         
 [2] "activity"                        
 [3] "timeBodyAccmeanX"                
 [4] "timeBodyAccmeanY"                
 [5] "timeBodyAccmeanZ"                
 [6] "timeBodyAccstdX"                 
 [7] "timeBodyAccstdY"                 
 [8] "timeBodyAccstdZ"                 
 [9] "timeGravityAccmeanX"             
[10] "timeGravityAccmeanY"             
[11] "timeGravityAccmeanZ"             
[12] "timeGravityAccstdX"              
[13] "timeGravityAccstdY"              
[14] "timeGravityAccstdZ"              
[15] "timeBodyAccJerkmeanX"            
[16] "timeBodyAccJerkmeanY"            
[17] "timeBodyAccJerkmeanZ"            
[18] "timeBodyAccJerkstdX"             
[19] "timeBodyAccJerkstdY"             
[20] "timeBodyAccJerkstdZ"             
[21] "timeBodyGyromeanX"               
[22] "timeBodyGyromeanY"               
[23] "timeBodyGyromeanZ"               
[24] "timeBodyGyrostdX"                
[25] "timeBodyGyrostdY"                
[26] "timeBodyGyrostdZ"                
[27] "timeBodyGyroJerkmeanX"           
[28] "timeBodyGyroJerkmeanY"           
[29] "timeBodyGyroJerkmeanZ"           
[30] "timeBodyGyroJerkstdX"            
[31] "timeBodyGyroJerkstdY"            
[32] "timeBodyGyroJerkstdZ"            
[33] "timeBodyAccMagmean"              
[34] "timeBodyAccMagstd"               
[35] "timeGravityAccMagmean"           
[36] "timeGravityAccMagstd"            
[37] "timeBodyAccJerkMagmean"          
[38] "timeBodyAccJerkMagstd"           
[39] "timeBodyGyroMagmean"             
[40] "timeBodyGyroMagstd"              
[41] "timeBodyGyroJerkMagmean"         
[42] "timeBodyGyroJerkMagstd"          
[43] "frequencyBodyAccmeanX"           
[44] "frequencyBodyAccmeanY"           
[45] "frequencyBodyAccmeanZ"           
[46] "frequencyBodyAccstdX"            
[47] "frequencyBodyAccstdY"            
[48] "frequencyBodyAccstdZ"            
[49] "frequencyBodyAccJerkmeanX"       
[50] "frequencyBodyAccJerkmeanY"       
[51] "frequencyBodyAccJerkmeanZ"       
[52] "frequencyBodyAccJerkstdX"        
[53] "frequencyBodyAccJerkstdY"        
[54] "frequencyBodyAccJerkstdZ"        
[55] "frequencyBodyGyromeanX"          
[56] "frequencyBodyGyromeanY"          
[57] "frequencyBodyGyromeanZ"          
[58] "frequencyBodyGyrostdX"           
[59] "frequencyBodyGyrostdY"           
[60] "frequencyBodyGyrostdZ"           
[61] "frequencyBodyAccMagmean"         
[62] "frequencyBodyAccMagstd"          
[63] "frequencyBodyBodyAccJerkMagmean" 
[64] "frequencyBodyBodyAccJerkMagstd"  
[65] "frequencyBodyBodyGyroMagmean"    
[66] "frequencyBodyBodyGyroMagstd"     
[67] "frequencyBodyBodyGyroJerkMagmean"
[68] "frequencyBodyBodyGyroJerkMagstd" 

column [1] subject - subject, who performed activity. Its range from 1 to 30
column [2] activity - activities of subject (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)
column 3-968 - average based on on the mean and standard deviation for each measurement of each variable for each activity and each subject

### Instructions to get tidy data
#This script is course project for Getting & Cleaning data Coursera
#https://www.coursera.org/learn/data-cleaning/peer/FIZtT/getting-and-cleaning-data-course-project
run_analysis<-function(){
library(data.table)

f_features<-"./R_GetCl Data CourseProject/UCI HAR Dataset/features.txt"
        features<-read.table(f_features, header=FALSE)

f_alabels<-"./R_GetCl Data CourseProject/UCI HAR Dataset/activity_labels.txt"
        alabels<-read.table(f_alabels, header=FALSE)

f_xtrain<-"./R_GetCl Data CourseProject/UCI HAR Dataset/train/X_train.txt"
        xtrain<-read.table(f_xtrain, header=FALSE)

f_ytrain<-"./R_GetCl Data CourseProject/UCI HAR Dataset/train/Y_train.txt"
        ytrain<-read.table(f_ytrain, header=FALSE)

f_xtest<-"./R_GetCl Data CourseProject/UCI HAR Dataset/test/X_test.txt"
        xtest<-read.table(f_xtest, header=FALSE)
f_ytest<-"./R_GetCl Data CourseProject/UCI HAR Dataset/test/Y_test.txt"
        ytest<-read.table(f_ytest, header=FALSE)

f_strain<-"./R_GetCl Data CourseProject/UCI HAR Dataset/train/subject_train.txt"
        strain<-read.table(f_strain, header=FALSE)
        
f_stest<-"./R_GetCl Data CourseProject/UCI HAR Dataset/test/subject_test.txt"
        stest<-read.table(f_stest, header=FALSE)
        
        
#1.Merges the training and the test sets to create one data set.
xdata<-rbind(xtrain,xtest) #all sets
        colnames(xdata)<-features[,2] #Named merged data
ydata<-rbind(ytrain,ytest) #all activity labels
        colnames(ydata)<-"activity"
sdata<-rbind(strain,stest)#all subjects
        colnames(sdata)<-"subject"
ndata0<-cbind(sdata,ydata)
ndata_all<-cbind(ndata0,xdata)
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
        features[,3]<-grepl("-mean\\()|std()",features[,2]) #find mean & std features
        ms_features<-features[features[,3]==TRUE,] #Only mean & std names
        xdata2<-xdata[,ms_features[,2]] #Select only mean & std names
        colnames(xdata2)<-ms_features[,2]
        ndata<-cbind(ndata0,xdata2) #only data with mean &std
        
#3.Uses descriptive activity names to name the activities in the data set
        activity<-merge(ydata,alabels, by.x="activity", by.y="V1", all=TRUE, sort=FALSE)
                ndata$activity<-activity[,"V2"]
        
#4.Appropriately labels the data set with descriptive variable names.   
# t mean time
# f mean frequency 
        colnames(ndata)<-gsub("\\()|\\-","",colnames(ndata))
        colnames(ndata)<-gsub("^t","time",colnames(ndata))
        colnames(ndata)<-gsub("^f","frequency",colnames(ndata))
#5.From the data set in step 4, creates a second, independent tidy data set
#with the average of each variable for each activity and each subject.
ndata<-data.table(ndata)
        tdata<-ndata[,lapply(.SD,mean),by=(subject,activity), .SDcols=3:length(colnames(ndata))]
        write.table(tdata,"tidydata.txt", row.name=FALSE) # write table with rezult
}

