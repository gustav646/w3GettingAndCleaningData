cleanData <- function(labels, features, subjectTest, xTest, yTest, subjectTrain, xTrain, yTrain) {
  
  #Load data table library
  library(data.table)
  
  #Get the file paths for all required data inputs.
  
  #Load data inputs into data tables.
  subject_test <- read.table(subjectTest)
  subject_training <- read.table(subjectTrain)
  
  xTest <- read.table(xTest)
  yTest <- read.table(yTest)
  
  xTrain <- read.table(xTrain)
  yTrain <- read.table(yTrain)

  labels <- read.table(labels)[,2]
  features <- read.table(features)[,2]
  
  #Select only "mean" and "std" variable names
  sel_features <- grepl("mean|std", features)
  
  #Assign column names to data according to features
  names(xTest) <- features
  names(xTrain) <- features
  
  #Select only columns with mean or std
  xTest <- xTest[,sel_features]
  xTrain <- xTrain[,sel_features]
  
  #Add activity name (labels) to y data labels
  yTest[,2] = labels[yTest[,1]]
  yTrain[,2] = labels[yTrain[,1]]
  
  #Improve column names
  names(yTest) <- c("activity_ID", "activity_name")
  names(subject_test) <- "subject"
  names(yTrain) <- c("activity_ID", "activity_name")
  names(subject_training) <- "subject"
  
  
  #merge data on Test
  dataTest <- cbind(as.data.table(subject_test),yTest,xTest)
  dataTrain <- cbind(as.data.table(subject_training), yTrain, xTrain)
  
  #merge both data sets into one final output
  data <- rbind(dataTest, dataTrain)
  
  #Assign descriptive names to features. 
  names(data) <- gsub("^t", "time", names(data))
  names(data) <- gsub("^f", "frequency", names(data))
  names(data) <- gsub("Acc", "Accelerometer", names(data))
  names(data) <- gsub("Gyro", "Gyroscope", names(data))
  names(data) <- gsub("Mag", "Magnitude", names(data))
  names(data) <- gsub("BodyBody", "Body", names(data))

  #Convert to factors
  data$subject <- as.factor(data$subject)
  data$activity_name <- as.factor(data$activity_name)
  
  #Calculate mean for each subject / activity pair
  filterData <- aggregate(.~subject+activity_name, data, mean)
  filterData <- arrange(filterData, subject, activity_name)
  
  write.table(filterData, file = "./tidy_data.txt")
  
}
