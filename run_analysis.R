#!/usr/bin/env Rscript

library(dplyr)
library(reshape2)

read.data <- function(subdir, basedir="UCI HAR Dataset") {
  ##read labels
  #features (columns of the dataset)
  #this if for requirement 4 (see the parameter col.names = features in line 23)
  features <- read.table(file = file.path(basedir,"features.txt"), header = FALSE, sep = " ", stringsAsFactors = FALSE)$V2
  #activity labels (descriptive names to name activites in the data set)
  #this if for requirement 3
  activity_labels <- read.table(file = file.path(basedir,"activity_labels.txt"), sep = "", header=FALSE, col.names = c("id", "label"))
  activity_labels <- activity_labels %>% arrange(id) %>% select(label)
  activity <- read.table(file = file.path(basedir,subdir,paste0("y_",subdir,".txt")), header = FALSE, col.names = c("id"))
  #activity <- merge(activity,activity_labels)
  activity <- activity %>% mutate(label= sapply(id,function(x) activity_labels[x,]))
  
  #read subjects
  subjects <- read.table(file = file.path(basedir,subdir,paste0("subject_",subdir,".txt")), col.names = c("subject"), colClasses = "factor")
  
  ##read the set in the subdir: should be one of test or train
  df <- read.table(file = file.path(basedir,subdir,paste0("X_",subdir,".txt")), header = FALSE, sep = "", col.names = features, row.names = NULL, check.names=FALSE)
  
  ##tidy up
  #1. filter for those columns, which names include std (standard deviation) or mean; this is for requirement 2
  #2. add activity labels
  #3. add subject
  #4. add label for source (subdir).
  df <- df[,grepl("(std|mean)\\(\\)", names(df))] %>%  
    mutate(activity = activity$label, subject = subjects$subject, source = as.factor(rep(subdir,length(subjects))))
  return(df)
}

tidy.data.train <- read.data("train")
tidy.data.test <- read.data("test")

#combine test and training (This is for requirement 1)
tidy.data <- rbind(tidy.data.train,tidy.data.test, make.row.names=TRUE)

#variables are all columns except the last 3
variables <- names(tidy.data)[-(length(tidy.data):(length(tidy.data)-2))]
#melt data
tidy.data.melted <- melt(tidy.data, id=c("subject","activity"), measure.vars = variables)
#calculate means
tidy.data.final <- dcast(tidy.data.melted, subject + activity ~ variable, mean)
#write to file
write.table(file = "result.csv", x = tidy.data.final, row.names = FALSE)
