---
Title: README - GETTING AND CLEANING DATA - PROJECT 1
output: word_document
---
#### C FANARA JULY 2014

#### **Assignment: produce a script that:** 
1. Merges the train and the test sets to create one data set
2. Extracts only the measurements on the mean and standard deviation for each measurement 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject

#### The code is extensively commented. here we summarize what we do:
* set the environment and check data
* prepare for bindinng the dataset (select list of columns)
* bind by column separately for Test and Train
* bind by row the two obtained datasets into a single dataset
* add missing labels to complete dataset
* describe the activities
* produce the new dataset by grouping subject and activity and calculate means
* put descriptive column names
* write the table to a file

#### Preliminary: set the environment and check data
##### Data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##### Instructions to get and unzip the file
##### first, we get the files in the right environment:
```{r}
if(!file.exists("./DATA")){dir.create("./DATA")}  
setwd("./DATA")
# now set a temp container for the file
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
con <- unz(temp, "./DATA")
data <- matrix(scan(con),ncol=4,byrow=TRUE)
unlink(temp) #### unlink from the working directory
```

##### Data Reference (full description): 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

### Merge the training and the test sets to create one data set (steps 1,2,4).

#### A. (Used for debugging, commented) let's have a look / compare with info in the Readme.txt
```{r}
# read a few files from download location 
subj_train<-read.table("./DATA/train/subject_train.txt")
bgx_test<-read.table("./DATA/test/Inertial Signals/body_gyro_x_test.txt")
act_labels<-read.table("./DATA/activity_labels.txt")
features<-read.table("./DATA/features.txt")
```

#### B. extract only the measurements on mean and standard deviation for each measurement, 
(all records showing only means and st dev), so prepare a list of matching columns: these will be used to extract the corresponding columns from the train and test datasets. I Use reshape
```{r}
library(reshape)
list_feat=melt(features) # need to melt the features
list_f<-as.character(list_feat[,1]) # because they are factors, force them to characters
MatchNdx<-grep("(M|m)ean|std",list_f,value=FALSE) # list indexes ('value'=F) of all matching columns
```

#### C. Perform the necessary bindings among the given fata files
#### TEST:  X_test, subject_test and Y_test
```{r}
X_Test<-read.table("./DATA/test/x_test.txt")  # read the tests
# extract the matching: assign the labels from list_f, subject and activity data sets
colnames(X_Test)<-list_f  # assign these to column names of our dataset
MatchXtest<-X_Test[,MatchNdx] # select the columns in the original according to the matching indexes
# add the subjects and labels to the extracted data, so to cbind these
subj_test<-read.table("./DATA/test/subject_test.txt")  # the subjects in the tests
Y_Test<-read.table("./DATA/test/y_test.txt")  # the activity labels
# bind all three:
All_Test<-do.call(cbind,list(MatchXtest, subj_test, Y_Test))
```


#### TRAIN: X_train, subject_train and Y_train
```{r}
X_Train<-read.table("./DATA/train/x_train.txt") # the training
# extract the matching: assign the labels from list_f, subject and activity data sets
colnames(X_Train)<-list_f  # assign these to column names of our dataset
MatchXtrain<-X_Train[,MatchNdx] # select the columns in the original according to the matching indexes
### add the subjects and labels to the extracted data, so to cbind these
subj_train<-read.table("./DATA/train/subject_train.txt")  # the subject in the training
Y_Train<-read.table("./DATA/train/y_train.txt") # the activity labels
# bind all three:
All_Train<-do.call(cbind,list(MatchXtrain, subj_train, Y_Train))
```


#### D. now produce the COMPLETE dataset by rbinding, # this will be dataset AllTT (=All Train and Test)
```{r}
AllTT<-do.call(rbind,list(All_Train, All_Test))
#### assign the last 2 labels, from subject and activity_labels data sets
names(AllTT)[87]<-"subject"  # remember we cbound the subject from x test and train, the name goes in col 87
names(AllTT)[88]<-"activity" # remember we cbound the activity from y test and train, the name goes in col 88
```


#### E. So far we have accomplished points 1,2,4 of the assignment; now we need to describe activities, along the activity code (last column), e.g. what the code means, ex. "1 WALKING "...etc
```{r}
# add column description, empty for now
AllTT["act_description"]<-NA
# fill it with the matching column in act_labels: V1 will match with activity, V2 gives back the description
AllTT$act_description = as.character(act_labels[match(AllTT$activity, act_labels$V1),"V2"] )
```
#### So far, we obtained all the tasks from 1 to 4 (except 3, see below)


### Point 5: A. "Creates a second tidy data set with the average of each variable for each activity and each subject"
#### Note: Averages should be new, e.g. recalculated. I use aggregate
#### Group the dataset we have (AllTT) by subject and activity and get the mean for each aggregation subj+act
```{r}
attach(AllTT)  # just to refer to names without need of df name
AggAllTT <-aggregate(AllTT, by=list(subject,activity),
                    FUN=mean, na.rm=TRUE)
# print(AggAllTT) # print(AggAllTT) commented, when not used for debug
detach(AllTT)
```

#### B. we grouped on subject and activity, better to rename the two grouping columns accordingly
#### (currently 'Group1' and 'Group2'):
```{r}
names(AggAllTT)[1]<-"Group by subject"
names(AggAllTT)[2]<-"Group by activity"
```

#### C. column description:
#### fill it with the matching column in act_labels: V1 will match with activity, V2 gives back the description
```{r}
AggAllTT$act_description = as.character(act_labels[match(AggAllTT$activity, act_labels$V1),"V2"] )
```

### Now Step 3 as FINAL STEP: 
#### D. because this is the file we give to the customer, it is here that some tidying 
#### of the columns makes sense (questionable: names are descriptive already...).
#### Anyway, we only make sure that these won't cause syntax problems, by removing potentially invalid
#### characters/symbols. For the purpose, I will use make.names()
```{r}
# list names of the final data set in a vector
Name_list<-as.character(names(AggAllTT))
NewNames<-make.names(Name_list,unique=TRUE) # remove potentiallly dangerous symbols, but we are left with 'ugly' dots
names(AggAllTT) <- NewNames # assign to columns of final dataset
```
### Write the file as a comma separated value with csv
#### motivation: want the customer/grader to easily visualize data
```{r}
write.csv(AggAllTT, file = "./DATA/Tidy_Dataset.csv", quote = TRUE) # written as "tidy_dataset"
# in order to allow a markdown in RStudio, installed and used library(knitr)
```