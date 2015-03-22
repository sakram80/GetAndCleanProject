## Description: Final project script 
## Author:      Shahid Akram

## load packages
library(dplyr)

## define working directory relative to the home directory
workingDirectory <- getwd()

## 1. Read train data into tables
subject_train <- read.table(paste(workingDirectory,"/train/subject_train.txt", sep = ""))
x_train <- read.table(paste(workingDirectory, "/train/X_train.txt", sep = ""))
y_train <- read.table(paste(workingDirectory, "/train/y_train.txt", sep = ""))

## 2. Combine trainData into columns as first column, subject train, second column
## as y_train, next 561 columns as x_train
trainData <- cbind(subject_train, y_train, x_train)


## 3. Read test data into tables
subject_test <- read.table(paste(workingDirectory,"/test/subject_test.txt", sep = ""))
x_test <- read.table(paste(workingDirectory, "/test/X_test.txt", sep = ""))
y_test <- read.table(paste(workingDirectory, "/test/y_test.txt", sep = ""))

## 4. Combine testData into columns as first column, subject test, second column
## as y_test, next 561 columns as x_test
testData <- cbind(subject_test, y_test, x_test)

## Combine train and test data tables 
finalData <- rbind(trainData, testData)

## 5. read features_info from file, make put it as column names of new data set
features <- read.table(paste(workingDirectory, "/features.txt", sep = ""));
df <- data.frame(V1 = c(1, 2), V2 = c("subject", "labels"))
dataHeader <- rbind(df, features)
colnames(finalData) <- dataHeader$V2

##6. get mean and std columns
## ignore.case=TRUE is not used as it does not represent actual mean in this case
meanAndStdColNames <- grep("(^.*mean.*$)|(^.*std.*$)|(^.*subject*$)|(^.*labels*$)", 
                           colnames(finalData), perl=TRUE, value=TRUE)  
finalData <- finalData[, meanAndStdColNames]

##7 read activities
activites_label <- read.table(paste(workingDirectory, "/activity_labels.txt", sep = ""));

##8. Link activity with labels 
finalData <- merge(activites_label, finalData)

##9. Remove extra columns produced by merging
finalData$V1 <- NULL
finalData$labels <- NULL

##10. Give descriptive names to each column, each row contains 3 column names in lower camel case
colnames(finalData) <- c("Activity", "Subject", "timeDomainBodyAccelerationMeanX", 
                         "timeDomainBodyAccelerationMeanY", "timeDomainBodyAccelerationMeanZ","timeDomainBodyAccelerationStandardDeviationX", 
                         "timeDomainBodyAccelerationStandardDeviationY", "timeDomainBodyAccelerationStandardDeviationZ", "timeDomainGravityAccelerationMeanX", 
                         "timeDomainGravityAccelerationMeanY", "timeDomainGravityAccelerationMeanZ", "timeDomaindGravityAccelerationStandardDeviationX", 
                         "timeDomaindGravityAccelerationStandardDeviationY", "timeDomaindGravityAccelerationStandardDeviationZ", "timeDomainBodyAccelerationJerkmeanX", 
                         "timeDomainBodyAccelerationJerkMeanY", "timeDomainBodyAccelerationJerkMeanZ", "timeDomainBodyAccelerationJerkStandardDeviationX" , 
                         "timeDomainBodyAccelerationJerkStandardDeviationY", "timeDomainBodyAccelerationJerkStandardDeviationZ", "timeDomainBodyGyroMeanX", 
                         "timeDomainBodyGyroMeanY", "timeDomainBodyGyroMeanZ", "timeDomainBodyGyrostandardDeviationX",
                         "timeDomainBodyGyroStandardDeviationY", "timeDomaindBodyGyrostandardDeviationZ", "timeDomainBodyGyroJerkmeanX", 
                         "timeDomainBodyGyroJerkMeanY", "timeDomainBodyGyroJerkMeanZ", "timeDomainBodyGyroJerkStandardDeviationX",
                         "timeDomainBodyGyroJerkStandardDeviationY", "timeDomainBodyGyroJerkStandardDeviationZ", "timeDomainBodyAccelerationMagnituteMean", 
                         "timeDomainBodyAccelerationMagnituteStandardDeviation", "timeDomainGravityAccelerationMagnituteMean", "timeDmainGravityAccelerationMagnituteStandardDevation",
                         "timeDomainBodyAccelerationJerkMagnituteMean", "timeDomainBodyAccelerationJerkMagnituteStandardDeviation", "timeDomainBodyGyroMagnitudeMean", 
                         "timeDomainBodyGyroMagnitudeStandardDeviation", "timeDomainBodyGyroJerkMagnitudeMean", "timeDomainBodyGyroJerkMagnitudeStandardDeviation",			
                         "frequencyDomainBodyAccelerationMeanX", "frequencyDomainBodyAccelerationMeanY", "frequencyDomainBodyAccelerationMeanZ", 
                         "frequencyDomainBodyAccelerationStandardDeviationX", "frequencyDomainBodyAccelerationStandardDeviationY", "frequencyDomainBodyAccelerationStandardDeviationZ",
                         "frequencyDomainBodyAccelerationMeanFrequencyX", "frequencyDomainBodyAccelerationMeanFrequencyY", "frequencyDomainBodyAccelerationMeanFrequencyZ", 
                         "frequencyDomainBodyAccelerationJerkMeanX", "frequencyDomainBodyAccelerationJerkMeanY","frequencyDomainBodyAccelerationJerkMeanZ",			
                         "frequencyDomainBodyAccelerationJerkStandardDeviationX", "frequencyDomainBodyAccelerationJerkStandardDeviationY", "frequencyDomainBodyAccelerationJerkStandardDeviationZ", 
                         "frequencyDomainBodyAccelerationJerkMeanFrequencyX", "frquencyDomainBodyAccelerationJerkMeanFrequencyY", "frequencyDomainBodyAccelerationJerkMeanFrequencyZ",			
                         "frequencyDomainBodyGyroMeanX", "frequencyDomainBodyGyroMeanY", "frequencyDomainBodyGyroMeanZ", 
                         "frequencyDomainBodyGyroStandardDeviationX", "frequencyDomainBodyGyroStandardDeviationY", "frequencyDomainBodyGyroStandardDeviationZ",			
                         "frequencyDomainBodyGyroMeanFrequencyX", "frequencyDomainBodyGyroMeanFrequencyY", "frequencyDomainBodyGyroMeanFrequencyZ", 
                         "frequencyDomainBodyAccelerationMagnitudeMean", "frequencyDomainBodyAccelerationMagnitudeStandardDeviation", "frequencydomainBodyAccelerationMagnitudeMeanFreq",
                         "frequencyDomainBodyBodyAccelerationJerkMagnitudeMean", "frequencyDomainBodyBodyAccelerationJerkMagnitudeStandardDeviation", "frequencyDomainBodyBodyAccelerationJerkMagnitudeMeanFreq", 
                         "frequencyDomainBodyBodyGyroMagnitudeMean", "frequencyDomainBodyBodyGyroMagnitudeStandardDeviation", "frequencydomainBodyBodyGyroMagnitudeMeanFrequency",			
                         "frequencyDomainBodyByGyroJerkMagnitudeMean", "frequencyDomainBodyBodyGyroJerkMagnitudeStandardDeviation", "frequencyDomainBodyBodyGyroJerkMagnitudeMeanFrequency")


##11. average it one subject and label
tidyData <- finalData %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

##12. export data set
write.table(tidyData, paste(workingDirectory,"/output.txt", sep=""), row.name=FALSE, sep="\t")




