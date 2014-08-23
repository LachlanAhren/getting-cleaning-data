# Retrieve file in a reproducible manner
retrieve_file <- function() {
    if (!file.exists("./data"))
    {
        dir.create("./data")
    }
    if (!file.exists("./data/UCI HAR Dataset"))
    {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl,"./data/getdata-projectfiles-UCI HAR Dataset.zip", "curl")
        unzip("./data/getdata-projectfiles-UCI HAR Dataset.zip",exdir = "./data")
    }
}
read_datasets <- function() {
    library(plyr)
    library(reshape2)
    X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
    X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
    Y_test <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
    Y_train <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
    subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
    subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
    combine_X <- rbind(X_test,X_train)
    combine_Y <- rbind(Y_test,Y_train)
    features <- read.table("./data/UCI HAR Dataset/features.txt")
    colnames(combine_X) <- features[,2]
    colnames(combine_X) <- tolower(colnames(combine_X))
    colnames(combine_X) <- gsub("\\(","",colnames(combine_X))
    colnames(combine_X) <- gsub("\\)","",colnames(combine_X))
    colnames(combine_X) <- gsub(",","",colnames(combine_X))
    #combine_X <- combine_X[,grep("(mean|std)",names(combine_X))]
    combine_X <- combine_X[,grep(ignore.case=TRUE,"(mean|std)",names(combine_X))]
    combine_subject <- rbind(subject_test,subject_train)
    full_data <- cbind(combine_X,combine_Y,combine_subject)


    
    activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
    #colnames(full_data) <- features[,2]
    #colnames(full_data)[562] <- "activityid"
    #colnames(full_data)[563] <- "subject"
    colnames(full_data)[length(full_data) -1] <- "activityid"
    colnames(full_data)[length(full_data)] <- "subject"
    colnames(activity_labels)[2] <- "activitytype"
    merged <- merge(full_data,activity_labels,by.x="activityid", by.y="V1")
    melted <- melt(merged, id.vars=c("activitytype", "subject", "activityid"))
    mean_data <- ddply(melted, c("activitytype", "subject", "variable", "activityid"), summarise,
          mean = mean(value))
    reshaped <- reshape(mean_data,direction="wide",idvar = c("subject","activityid","activitytype"), timevar = "variable")
    reshaped
}
retrieve_file()
full_data <- read_datasets()
