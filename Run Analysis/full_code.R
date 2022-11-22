library(dplyr)
library(data.table)

filename<- "getdata_projectfiles_UCI HAR Dataset.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, filename, method="curl")

###Reading in files
testy<- read.table("~/Downloads/UCI HAR Dataset/test/y_test.txt", header = F)
trainy<- read.table("~/Downloads/UCI HAR Dataset/train/y_train.txt", header = F)
testx <- read.table("~/Downloads/UCI HAR Dataset/test/X_test.txt", header = F)
trainx <- read.table("~/Downloads/UCI HAR Dataset/train/X_train.txt", header = F)
subjecttest <- read.table("~/Downloads/UCI HAR Dataset/test/subject_test.txt", header = F)
subjecttrain <- read.table("~/Downloads/UCI HAR Dataset/train/subject_train.txt", header = F)


activitylabels <- read.table("~/Downloads/UCI HAR Dataset/activity_labels.txt", header = F)
FeaturesNames <- read.table("~/Downloads/UCI HAR Dataset/features.txt", header = F)

#####Merge the dataframes
FeaturesData <- rbind(testx, trainx)
SubjectData <- rbind(subjecttest, subjecttrain)
ActivityData <- rbind(testy, trainy)

names(ActivityData) <- "ActivityN"
names(activitylabels) <- c("ActivityN", "Activity")

Activity <- left_join(ActivityData, activitylabels, "ActivityN")[, 2]


names(SubjectData) <- "Subject"
#Rename FeaturesData columns using columns from FeaturesNames
names(FeaturesData) <- FeaturesNames$V2

###Create one large Dataset with only these variables: SubjectData,  Activity,  FeaturesData
all <- cbind(SubjectData, Activity)
all <- cbind(all, FeaturesData)

###Create New datasets by extracting only the measurements on the mean and standard deviation for each measurement
subFeaturesNames <- FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesNames$V2)]
DataNames <- c("Subject", "Activity", as.character(subFeaturesNames))
all <- subset(all, select=DataNames)

#####Rename the columns of the large dataset using more descriptive activity names
names(all)<-gsub("^t", "time", names(all))
names(all)<-gsub("^f", "frequency", names(all))
names(all)<-gsub("Acc", "Accelerometer", names(all))
names(all)<-gsub("Gyro", "Gyroscope", names(all))
names(all)<-gsub("Mag", "Magnitude", names(all))
names(all)<-gsub("BodyBody", "Body", names(all))
names(all)<-gsub("tBody", "TimeBody", names(all))
names(all)<-gsub("gravity", "Gravity", names(all))
names(all) <- gsub("Jerk","(jerk signal) ",names(all))
names(all) <- gsub("Mag","(Euclidean norm) ",names(all))



####Create a second, independent tidy data set with the average of each variable for each activity and each subject
new<-aggregate(. ~Subject + Activity, all, mean)
new<-new[order(new$Subject,new$Activity),]

#Save this tidy dataset to local file
write.table(new, file = "tidydata.txt",row.name=FALSE)
