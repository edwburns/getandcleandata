run_analysis <- function(){
  # Read relevant test and train data which appears to give us the subject
  # and activity of the observation.
  subjTs <- read.delim2("./UCI HAR Dataset/test/subject_test.txt", sep = "", header = FALSE, col.names = c("subject"), colClasses = c("integer") )
  subjTr <- read.delim2("./UCI HAR Dataset/train/subject_train.txt", sep = "", header = FALSE, col.names = c("subject"), colClasses = c("integer") )
  actvTs <- read.delim2("./UCI HAR Dataset/test/y_test.txt", sep = "", header = FALSE, col.names = c("activity"), colClasses = c("integer") )
  actvTr <- read.delim2("./UCI HAR Dataset/train/y_train.txt", sep = "", header = FALSE, col.names = c("activity"), colClasses = c("integer") )
  
  # Before reading the X_test.txt and X_train.txt files (which appear to give us
  # the observations), let us get the column names which  we want to use for this data.
  feat <- read.delim2("./UCI HAR Dataset/features.txt", sep = "", header = FALSE, colClasses = c("character") )
  l <- as.list(feat$V2)
  # Now, let us use this list "l" to set our column names when reading our observation data.
  rcrddTs <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE, colClasses = c("numeric"), col.names = l )
  rcrddTr <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE, colClasses = c("numeric"), col.names = l )
  
  # For both our test and train sets we will create our first two columns
  # for our observations which will have the subject and activity.
  Ts1 <- cbind(subjTs, actvTs)
  Tr1 <- cbind(subjTr, actvTr)
  
  # So we can use some nice functions we will use tbl_df on our test and train
  # datasets of observations.
  rcrddTsTbl <- tbl_df(rcrddTs)
  rcrddTrTbl <- tbl_df(rcrddTr)
  
  # Per our instructions, we only want those columns which have to do
  # with standard deviation and mean.  We will get them.
  cc <- grep("(.*)[Ss]td|[Mm]ean.*)", feat$V2)
  # Then we will select from our observations only those columns.
  rcrddTsTblSel <- select(rcrddTsTbl, cc)
  rcrddTrTblSel <- select(rcrddTrTbl, cc)
  
  # Using tbl_df to redefine our earlier subject and activity tables -
  Ts1Tbl <- tbl_df(Ts1)
  Tr1Tbl <- tbl_df(Tr1)
  
  # For the data from test, we combine our tables (combine columns).
  TsTbl <- cbind(Ts1Tbl, rcrddTsTblSel)
  # Likewise for the train data set . . . 
  TrTbl <- cbind(Tr1Tbl, rcrddTrTblSel)
  
  # Now let us bring both test andd train data together and use tbl_df again
  # just to make sure we ahve the structure we want for our result.
  Tbl <- rbind(TsTbl, TrTbl)
  TblTbl <- tbl_df(Tbl)
  
  # To change our activity code, which was just an integer, to use the 
  # activity description we will get label data set and do an inner join
  # with the table we just built.
  actvlbl <- read.delim2("./UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE, colClasses = c("integer","character") )
  actvlblTbl <- tbl_df(actvlbl)
  j <- inner_join(TblTbl, actvlblTbl, by = c("activity" = "V1"))
  j <- mutate(j, activity = V2)
  
  # We don't need the V2 column because of our "mutate" to set activity
  # so let us drop V2
  j <- select(j, -V2)
  
  # So that the group_by function can be used, we will change subject and activity
  # to factors.
  jm <- mutate(j, subject = as.factor(subject), activity = as.factor(activity))
  # Then, we group our data and summarise, applying the mean to the non-grouped columns.
  g <- group_by(jm, subject, activity)
  s <- summarise_all(g, mean)
  
  # Since we took the mean of the columns for each group, we will try to 
  # create a column name that reflects this.  In other words, we
  # will try to be more descriptive with our column names.
  coln <- colnames(s)
  colnMean <- lapply(coln, function(x){ifelse(x == "subject" | x == "activity",x,paste("MeanOf-", x))})
  colnames(s) <- colnMean

  # ready to return our result.
  return(s)
  
}


