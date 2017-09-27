## Import the dplyr library
library(dplyr)

## Change working directory
## setwd("C:/Users/Jon/WORK/COURSES/DATASCIENCESPECIALISATIONS/CourseWork/Data_Cleaning_W4/W4Project/UCI HAR Dataset")

## Test data

  ### Test subjects
  testsubject <- read.csv("test/subject_test.txt", sep="", header=FALSE)
  ### Test labels
  testlabels <- read.csv("test/y_test.txt", sep="", header=FALSE) 
  ### Test set
  testset <- read.csv("test/X_test.txt", sep="", header=FALSE)
  ### Merge into test dataframe
  test <- data.frame(testsubject, testlabels, testset)
  ### Identify as test data pre-merge with training
  test$datagroup = "test"

## Training data
  
  ### Training subjects
  trainingsubject <- read.csv("train/subject_train.txt", sep="", header=FALSE)
  ### Training labels
  traininglabels <- read.csv("train/y_train.txt", sep="", header=FALSE) 
  ### Training set
  trainingset <- read.csv("train/X_train.txt", sep="", header=FALSE)
  ### Merge into training dataframe
  training <- data.frame(trainingsubject, traininglabels, trainingset)
  ### Identify as training data pre-merge with test
  training$datagroup = "training"
  
## Combine Test & Training data
alldata <- rbind(test, training)

## Features data

  ###read 
  features <- read.csv("features.txt", sep="", header=FALSE)
  ###import as column headings into main dataset NB. Columns 1 & 2 are subject & activity
  ###but first we need to convert to a vector
  featurenames <- as.vector(features[, 2])
  colnames(alldata) <- c("subjectid", "activitylabel", featurenames, "datagroup")

## Means & Std only
  
  ###Filter combined data (alldata) to only contain:
  ###subjectid, activitylabel, mean() & std() & datagroup
  columnfilter1 <- grep(("subjectid|activitylabel|.mean().|.std().|datagroup"), names(alldata))
  alldata <- alldata[columnfilter,]
  alldatafilter1 <- alldata[,columnfilter1]
  ###Next step is necessary to remove meanFreq() columns
  columnfilter2 <- grep((".meanFreq()."), names(alldatafilter1), invert = TRUE)
  meanstddata <- alldatafilter1[,columnfilter2]

## Activities data

  ###read 
  activities <- read.csv("activity_labels.txt", sep="", header=FALSE)
  ###match activitylabel with v1 in activities and replace with text description (v2)
  meanstddata$activitylabel <- 
    as.character(activities[match(meanstddata$activitylabel, activities$V1), 'V2'])
  
## Standardise column names

  ###remove (, ), -
  names(meanstddata) = gsub("\\(\\)|-", "", names(meanstddata))
  ###remove first of BodyBody!
  names(meanstddata) = gsub("BodyBody", "Body", names(meanstddata))

## Summary dataset
  
  ###Group by subject & activity 
  ###and calculate the mean of each group (numeric)
  summarydata <- meanstddata %>%
    group_by(subjectid, activitylabel) %>%
    summarise_if(.predicate = function(x) is.numeric(x), .funs = mean)

  ###Write summarydata to an output text file
  write.table(summarydata, file="SubjectActivityAverages.txt")