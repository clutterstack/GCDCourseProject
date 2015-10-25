## Getting and Cleaning Data Course Project
## clutterstack
## Started Fri Oct 16 2015

## I've unzipped the dataset into the directory "UCI HAR Dataset" and the
## instructions say the data should be in the working directory.

## Preliminaries
rm(list = ls())

# ----- Start part 1: loading and merging the test and train data -----
## "Merge the training and the test sets to create one data set."
## There are three data files for each set, plus the features.txt file to label
## each column and the activity_labels.txt file to label activities by descriptive names.

testsubject <- read.table("./test/subject_test.txt")
testobs <- read.table("./test/X_test.txt")
testactivity <- read.table("./test/y_test.txt")
testdata <- cbind(testactivity, testsubject, testobs)

trainsubject <- read.table("./train/subject_train.txt")
trainobs <- read.table("./train/X_train.txt")
trainactivity <- read.table("./train/y_train.txt")
traindata <- cbind(trainactivity, trainsubject, trainobs)

obs <- rbind(traindata, testdata)
names(obs)[1] <- "Activity"
names(obs)[2] <- "Subject"

# Delete data no longer needed, to free up memory just in case
rm(testsubject, testactivity, testobs, testdata)
rm(trainsubject, trainactivity, trainobs, traindata)
gc()

# ----- End part 1: loading and merging the test and train data -----

# ----- Start part 2: extracting mean and standard deviation values -----
# "Extracts only the measurements on the mean and standard deviation for each
# measurement."

# Load, using as.is = TRUE to avoid converting labels to factors (want characters
# so we can grep them)
# We'll use the feature labels to find all the mean and std dev column indices
featurelabels <- read.table("./features.txt",as.is = TRUE)
# Use grep to find "mean()" and "std()" in feature names
# (not keeping meanfreq() columns, just mean(); see CodeBook.md)
meancols  <- grep("mean\\(\\)", featurelabels$V2)
stdevcols  <- grep("std\\(\\)", featurelabels$V2)
# Remember to keep the first two cols (activity and subject) in the obs dataset
keepdata <- cbind(obs[, 1:2], obs[, c(meancols) + 2], obs[, c(stdevcols) + 2])
# ----- End part 2: extracting mean and standard deviation -----

# ----- Start part 3: labelling the activities -----
# "Uses descriptive activity names to name the activities in the data set"
# From the file activity_labels.txt, know to map activities as:
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

# Give them slightly tidier names.
keepdata$Activity <- factor(keepdata$Activity,
                    levels = c(1,2,3,4,5,6),
                    labels = c("Walking","WalkingUpstairs","WalkingDownstairs","Sitting","Standing","LyingDown"))
# ----- End part 3: labelling the activities -----

# ----- Start part 4: labelling the variables -----
# "Appropriately labels the data set with descriptive variable names." 
# Get rid of feature labels we don't need (features.txt has two columns; use the second column):
keeplabels <- featurelabels$V2[c(meancols,stdevcols)]
# use regex matching in gsub to remove brackets and hyphens; replace std and mean by StdDev and Mean; 
# replace t with time and f with freq
keeplabels <- gsub("-","", keeplabels)
keeplabels <- gsub("std\\(\\)","StdDev", keeplabels)
keeplabels <- gsub("mean\\(\\)","Mean", keeplabels)
keeplabels <- gsub("^t","time", keeplabels)
keeplabels <- gsub("^f","freq", keeplabels)

# Finally apply the names to the data    
names(keepdata) <- c(names(keepdata)[1:2], keeplabels)

# ----- End part 4: labelling the variables -----

# ----- Start part 5: get the mean for each activity/subject combination -----
# "From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject."

newtable <- group_by(keepdata, Activity, Subject)
final  <- newtable %>% summarise_each(funs(mean))

# Note as a check that there are 6 activities and 30 participants, so 180 rows
# is the expected length.

# ----- End part 5: get the mean for each activity, subject -----
## Output the data to a text file for delivery
# Assignment instructions are to use write.table() with row.name=FALSE
# to generate the text file.
write.table(final, file = "UCImeans.txt", row.names = FALSE)
# Read in with 
#checkoutput <- read.table("UCImeans.txt", header = TRUE)
