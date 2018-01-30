##==============================================================================
## function run_analysis:
## 
## This is is the main function of the script of the same name, 
## which forms part of a set of files described in README.md.
##
## The function commences by unzipping the dataset provided in the repository 
## (note the download of the zip file may be activated by commenting in
##  the two lines of code commencing with "strURL" and "download.file";
##  tested only on Windows 10).
##
## The function then proceeds to perform five steps outlined for the project, 
## i.e.
## 1. Merge the training and the test sets to create one data set.
## 2. Extract only the measurements on the mean and standard deviation for each measurement.
## 3. Use descriptive activity names to name the activities in the data set.
## 4. Appropriately label the data set with descriptive variable names.
## 5. From the data set in step 4, create a second, independent tidy data. 
##
## For four of the five steps, the main function uses custom functions,
## which are saved further down in this script.
##
run_analysis <- function() {

    ##--------------------------------------------------------------------------
    ## Initialize

    ## file set
    #install_packages("readr")
    library(readr) ## for read_table functionality
    #strURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"
    strZIP <- file.path(getwd(), "UCI_HAR_dataset.zip")
    #download.file(url = strURL, destfile = strZIP) ## tested only on Windows 10
    setFULL <- unzip(strZIP)
    
    ## common file paths
    strROOT <- paste(getwd(), "/UCI HAR dataset/", sep = "") ## top folder inherent to zip file
    strOUT <- paste(getwd(), "/run_analysis_out/", sep = "") ## folder for writing text files
    
    ## lookup table for activities as provided in activity_labels.txt under strROOT;
    ## formatting the labels for purposes of comparison in functions create_tidy
    lookupACTIV <- vector(length = 6)
    lookupACTIV[1] <- sprintf("%-20s", "WALKING")
    lookupACTIV[2] <- sprintf("%-20s", "WALKING_UPSTAIRS")
    lookupACTIV[3] <- sprintf("%-20s", "WALKING_DOWNSTAIRS")
    lookupACTIV[4] <- sprintf("%-20s", "SITTING")
    lookupACTIV[5] <- sprintf("%-20s", "STANDING")
    lookupACTIV[6] <- sprintf("%-20s", "LAYING")
    
    ##--------------------------------------------------------------------------
    ## 1. Merge the training and the test sets to create one data set
    tblFULL <- readTEST_readTRAIN_merge(strROOT)
    #write_text(tblFULL, paste(strOUT, "step1_tblFULL.txt", sep = ""), " | ", FALSE)

    ##--------------------------------------------------------------------------
    ## 2. Extract only the measurements on the mean and standard deviation for each measurement
    tblPART <- extract_mean_stdev(tblFULL)
    #write_text(tblPART, paste(strOUT, "step2_tblPART.txt", sep = ""), "", FALSE)
    
    ##--------------------------------------------------------------------------
    ## 3. Use descriptive activity names to name the activities in the data set
    for (i in 1:6) {
        tblPART[, 1] <- replace(
            tblPART[, 1], 
            as.numeric(tblPART[, 1]) == i, 
            lookupACTIV[i])
    }
    #write_text(tblPART, paste(strOUT, "step3_tblPART.txt", sep = ""), "", FALSE)
    
    ##--------------------------------------------------------------------------
    ## 4. Appropriately label the data set with descriptive variable names
    tblPART <- name_variables(tblPART)
    #write_text(tblPART, paste(strOUT, "step4_tblPART.txt", sep = ""), "", TRUE)
    
    ##--------------------------------------------------------------------------
    ## 5. From the data set in step 4, create a second, independent tidy data 
    ##    set with the average of each variable for each activity and each subject
    tblTIDY <- create_tidy(tblPART, lookupACTIV)
    write_text(tblTIDY, paste(strOUT, "step5_TIDY.txt", sep = ""), "", TRUE)

}##END run_analysis

##==============================================================================
## function readTEST_readTRAIN_merge:
## 
## Merge the training and the test sets to create one data set.
##
readTEST_readTRAIN_merge <- function(strROOT) {

    ##--------------------------------------------------------------------------
    ## read files from sub-folder "test" into tables and merge them into tblTEST
    strPATH <- paste(strROOT, "test/", sep = "")
    X <- read_table(paste(strPATH, "X_test.txt", sep = ""), col_names = FALSE)
    Y <- read_table(paste(strPATH, "Y_test.txt", sep = ""), col_names = FALSE)
    subject <- read_table(paste(strPATH, "subject_test.txt", sep = ""), col_names = FALSE)
    tblTEST <- cbind(X, Y, subject)
    
    ##--------------------------------------------------------------------------
    ## read files from sub-folder "train" into tables and merge them into tblTRAIN
    strPATH <- paste(strROOT, "train/", sep = "")
    X <- read_table(paste(strPATH, "X_train.txt", sep = ""), col_names = FALSE)
    Y <- read_table(paste(strPATH, "Y_train.txt", sep = ""), col_names = FALSE)
    subject <- read_table(paste(strPATH, "subject_train.txt", sep = ""), col_names = FALSE)
    tblTRAIN <- cbind(X, Y, subject)
    
    ##--------------------------------------------------------------------------
    ## merge tblTEST and tblTRAIN into tblFULL
    tblFULL <- rbind(tblTEST, tblTRAIN)
    
    ##--------------------------------------------------------------------------
    ## return tblFULL
    tblFULL

}##END readTEST_readTRAIN_merge

##==============================================================================
## function extract_mean_stdev:
## 
## Extract only the measurements on the mean and standard deviation for each measurement.
##
extract_mean_stdev <- function(tblFULL) {

    ##--------------------------------------------------------------------------
    ## extract only mean and standard deviation measurements from tblFULL,
    ## as well as the two key fields of course
    rowMAX <- length(tblFULL[, 1])
    tblPART <- matrix(nrow = rowMAX, ncol = 68)
    for (r in 1:rowMAX) {
        
        ##~~~~~~~~~~~~
        ## key fields (their combination is unique)
        ##~~~~~~~~~~~~
        
        ## activity
        tblPART[r, 1] <- sprintf("%-20s", tblFULL[r, 562])
        
        ## subject
        tblPART[r, 2] <- sprintf("%-10s", tblFULL[r, 563])
        
        ##~~~~~~~~~~~~~
        ## time domain 
        ##~~~~~~~~~~~~~
        
        ## tBodyAcc - mean XYZ, stdev XYZ
        tblPART[r, 3] <- sprintf("%25s", tblFULL[r, 1])
        tblPART[r, 4] <- sprintf("%25s", tblFULL[r, 2])
        tblPART[r, 5] <- sprintf("%25s", tblFULL[r, 3])
        tblPART[r, 6] <- sprintf("%25s", tblFULL[r, 4])
        tblPART[r, 7] <- sprintf("%25s", tblFULL[r, 5])
        tblPART[r, 8] <- sprintf("%25s", tblFULL[r, 6])
        
        ## tGravityAcc - mean XYZ, stdev XYZ
        tblPART[r, 9] <- sprintf("%25s", tblFULL[r, 41])
        tblPART[r, 10] <- sprintf("%25s", tblFULL[r, 42])
        tblPART[r, 11] <- sprintf("%25s", tblFULL[r, 43])
        tblPART[r, 12] <- sprintf("%25s", tblFULL[r, 44])
        tblPART[r, 13] <- sprintf("%25s", tblFULL[r, 45])
        tblPART[r, 14] <- sprintf("%25s", tblFULL[r, 46])
        
        ## tBodyAccJerk - mean XYZ, stdev XYZ
        tblPART[r, 15] <- sprintf("%25s", tblFULL[r, 81])
        tblPART[r, 16] <- sprintf("%25s", tblFULL[r, 82])
        tblPART[r, 17] <- sprintf("%25s", tblFULL[r, 83])
        tblPART[r, 18] <- sprintf("%25s", tblFULL[r, 84])
        tblPART[r, 19] <- sprintf("%25s", tblFULL[r, 85])
        tblPART[r, 20] <- sprintf("%25s", tblFULL[r, 86])
        
        ## tBodyGyro - mean XYZ, stdev XYZ
        tblPART[r, 21] <- sprintf("%25s", tblFULL[r, 121])
        tblPART[r, 22] <- sprintf("%25s", tblFULL[r, 122])
        tblPART[r, 23] <- sprintf("%25s", tblFULL[r, 123])
        tblPART[r, 24] <- sprintf("%25s", tblFULL[r, 124])
        tblPART[r, 25] <- sprintf("%25s", tblFULL[r, 125])
        tblPART[r, 26] <- sprintf("%25s", tblFULL[r, 126])
        
        ## tBodyGyroJerk - mean XYZ, stdev XYZ
        tblPART[r, 27] <- sprintf("%25s", tblFULL[r, 161])
        tblPART[r, 28] <- sprintf("%25s", tblFULL[r, 162])
        tblPART[r, 29] <- sprintf("%25s", tblFULL[r, 163])
        tblPART[r, 30] <- sprintf("%25s", tblFULL[r, 164])
        tblPART[r, 31] <- sprintf("%25s", tblFULL[r, 165])
        tblPART[r, 32] <- sprintf("%25s", tblFULL[r, 166])
        
        ## tBodyAccMag - mean, stdev
        tblPART[r, 33] <- sprintf("%25s", tblFULL[r, 201])
        tblPART[r, 34] <- sprintf("%25s", tblFULL[r, 202])
        
        ## tGravityAccMag - mean, stdev
        tblPART[r, 35] <- sprintf("%25s", tblFULL[r, 214])
        tblPART[r, 36] <- sprintf("%25s", tblFULL[r, 215])
        
        ## tBodyAccJerkMag - mean, stdev
        tblPART[r, 37] <- sprintf("%25s", tblFULL[r, 227])
        tblPART[r, 38] <- sprintf("%25s", tblFULL[r, 228])
        
        ## tBodyGyroMag - mean, stdev
        tblPART[r, 39] <- sprintf("%25s", tblFULL[r, 240])
        tblPART[r, 40] <- sprintf("%25s", tblFULL[r, 241])
        
        ## tBodyGyroJerkMag - mean, stdev
        tblPART[r, 41] <- sprintf("%25s", tblFULL[r, 253])
        tblPART[r, 42] <- sprintf("%25s", tblFULL[r, 254])
        
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## frequency domain signals
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        ## fBodyAcc - mean XYZ, stdev XYZ
        tblPART[r, 43] <- sprintf("%25s", tblFULL[r, 266])
        tblPART[r, 44] <- sprintf("%25s", tblFULL[r, 267])
        tblPART[r, 45] <- sprintf("%25s", tblFULL[r, 268])
        tblPART[r, 46] <- sprintf("%25s", tblFULL[r, 269])
        tblPART[r, 47] <- sprintf("%25s", tblFULL[r, 270])
        tblPART[r, 48] <- sprintf("%25s", tblFULL[r, 271])
        
        ## fBodyAccJerk - mean XYZ, stdev XYZ
        tblPART[r, 49] <- sprintf("%25s", tblFULL[r, 345])
        tblPART[r, 50] <- sprintf("%25s", tblFULL[r, 346])
        tblPART[r, 51] <- sprintf("%25s", tblFULL[r, 347])
        tblPART[r, 52] <- sprintf("%25s", tblFULL[r, 348])
        tblPART[r, 53] <- sprintf("%25s", tblFULL[r, 349])
        tblPART[r, 54] <- sprintf("%25s", tblFULL[r, 350])
        
        ## fBodyGyro - mean XYZ, stdev XYZ
        tblPART[r, 55] <- sprintf("%25s", tblFULL[r, 424])
        tblPART[r, 56] <- sprintf("%25s", tblFULL[r, 425])
        tblPART[r, 57] <- sprintf("%25s", tblFULL[r, 426])
        tblPART[r, 58] <- sprintf("%25s", tblFULL[r, 427])
        tblPART[r, 59] <- sprintf("%25s", tblFULL[r, 428])
        tblPART[r, 60] <- sprintf("%25s", tblFULL[r, 429])
        
        ## fBodyAccMag - mean, stdev
        tblPART[r, 61] <- sprintf("%25s", tblFULL[r, 503])
        tblPART[r, 62] <- sprintf("%25s", tblFULL[r, 504])
        
        ## fBodyAccJerkMag - mean, stdev
        tblPART[r, 63] <- sprintf("%25s", tblFULL[r, 516])
        tblPART[r, 64] <- sprintf("%25s", tblFULL[r, 517])
        
        ## fBodyGyroMag - mean, stdev
        tblPART[r, 65] <- sprintf("%25s", tblFULL[r, 529])
        tblPART[r, 66] <- sprintf("%25s", tblFULL[r, 530])
        
        ## fBodyGyroJerkMag - mean, stdev
        tblPART[r, 67] <- sprintf("%25s", tblFULL[r, 542])
        tblPART[r, 68] <- sprintf("%25s", tblFULL[r, 543])
        
    }
    
    ##--------------------------------------------------------------------------
    ## return tblPART
    tblPART
    
}##END extract_mean_stdev

##==============================================================================
## name_variables:
## 
## Label the data set provided as input with descriptive variable names.
##
## Note this function is first called from the main function run_analysis 
## (top of script), and again from function create_tidy (below).
##
name_variables <- function(tbl) {
    
    ##--------------------------------------------------------------------------
    ## copy select variable names from features.txt in the data set ROOT folder
    colnames(tbl) <- c(
        
        ##~~~~~~~~~~~~
        ## key fields (their combination is unique)
        ##~~~~~~~~~~~~

        sprintf("%-20s", "Activity"),
        sprintf("%-10s", "Subject"),
        
        ##~~~~~~~~~~~~~
        ## time domain
        ##~~~~~~~~~~~~~
        
        ## features 1..6:
        sprintf("%-25s", "tBodyAcc-mean-X"),
        sprintf("%-25s", "tBodyAcc-mean-Y"),
        sprintf("%-25s", "tBodyAcc-mean-Z"),   
        sprintf("%-25s", "tBodyAcc-stdev-X"),
        sprintf("%-25s", "tBodyAcc-stdev-Y"),
        sprintf("%-25s", "tBodyAcc-stdev-Z"),
        
        ## features 41..46:
        sprintf("%-25s", "tGravityAcc-mean-X"),
        sprintf("%-25s", "tGravityAcc-mean-Y"),
        sprintf("%-25s", "tGravityAcc-mean-Z"),
        sprintf("%-25s", "tGravityAcc-stdev-X"),
        sprintf("%-25s", "tGravityAcc-stdev-Y"),
        sprintf("%-25s", "tGravityAcc-stdev-Z"),
        
        ## features 81..86:
        sprintf("%-25s", "tBodyAccJerk-mean-X"),
        sprintf("%-25s", "tBodyAccJerk-mean-Y"),
        sprintf("%-25s", "tBodyAccJerk-mean-Z"),
        sprintf("%-25s", "tBodyAccJerk-stdev-X"),
        sprintf("%-25s", "tBodyAccJerk-stdev-Y"),
        sprintf("%-25s", "tBodyAccJerk-stdev-Z"),
        
        ## features 121..126:
        sprintf("%-25s", "tBodyGyro-mean-X"),
        sprintf("%-25s", "tBodyGyro-mean-Y"),
        sprintf("%-25s", "tBodyGyro-mean-Z"),
        sprintf("%-25s", "tBodyGyro-stdev-X"),
        sprintf("%-25s", "tBodyGyro-stdev-Y"),
        sprintf("%-25s", "tBodyGyro-stdev-Z"),
        
        ## features 161..166
        sprintf("%-25s", "tBodyGyroJerk-mean-X"),
        sprintf("%-25s", "tBodyGyroJerk-mean-Y"),
        sprintf("%-25s", "tBodyGyroJerk-mean-Z"),
        sprintf("%-25s", "tBodyGyroJerk-stdev-X"),
        sprintf("%-25s", "tBodyGyroJerk-stdev-Y"),
        sprintf("%-25s", "tBodyGyroJerk-stdev-Z"),
        
        ## features 201..202
        sprintf("%-25s", "tBodyAccMag-mean"),
        sprintf("%-25s", "tBodyAccMag-stdev"),
        
        ## features 214..215:
        sprintf("%-25s", "tGravityAccMag-mean"),
        sprintf("%-25s", "tGravityAccMag-stdev"),
        
        ## features 227..228:
        sprintf("%-25s", "tBodyAccJerkMag-mean"),
        sprintf("%-25s", "tBodyAccJerkMag-stdev"),
        
        ## features 240..241:
        sprintf("%-25s", "tBodyGyroMag-mean"),
        sprintf("%-25s", "tBodyGyroMag-stdev"),
        
        ## features 253..254:
        sprintf("%-25s", "tBodyGyroJerkMag-mean"),
        sprintf("%-25s", "tBodyGyroJerkMag-stdev"),
        
        ##~~~~~~~~~~~~~~~~~~
        ## frequency domain
        ##~~~~~~~~~~~~~~~~~~
        
        ## features 266..267:
        sprintf("%-25s", "fBodyAcc-mean-X"),
        sprintf("%-25s", "fBodyAcc-mean-Y"),
        sprintf("%-25s", "fBodyAcc-mean-Z"),
        sprintf("%-25s", "fBodyAcc-stdev-X"),
        sprintf("%-25s", "fBodyAcc-stdev-Y"),
        sprintf("%-25s", "fBodyAcc-stdev-Z"),
        
        ## features 345..346:
        sprintf("%-25s", "fBodyAccJerk-mean-X"),
        sprintf("%-25s", "fBodyAccJerk-mean-Y"),
        sprintf("%-25s", "fBodyAccJerk-mean-Z"),
        sprintf("%-25s", "fBodyAccJerk-stdev-X"),
        sprintf("%-25s", "fBodyAccJerk-stdev-Y"),
        sprintf("%-25s", "fBodyAccJerk-stdev-Z"),
        
        ## features 424..425:
        sprintf("%-25s", "fBodyGyro-mean-X"),
        sprintf("%-25s", "fBodyGyro-mean-Y"),
        sprintf("%-25s", "fBodyGyro-mean-Z"),
        sprintf("%-25s", "fBodyGyro-stdev-X"),
        sprintf("%-25s", "fBodyGyro-stdev-Y"),
        sprintf("%-25s", "fBodyGyro-stdev-Z"),
        
        ## features 503..504:
        sprintf("%-25s", "fBodyAccMag-mean"),
        sprintf("%-25s", "fBodyAccMag-stdev"),
        
        ## features 516..517:
        sprintf("%-25s", "fBodyAccJerkMag-mean"),
        sprintf("%-25s", "fBodyAccJerkMag-stdev"),
        
        ## features 529..530:
        sprintf("%-25s", "fBodyGyroMag-mean"),
        sprintf("%-25s", "fBodyGyroMag-stdev"),
        
        ## features 542..543:
        sprintf("%-25s", "tBodyGyroJerkMag-mean"),
        sprintf("%-25s", "tBodyGyroJerkMag-stdev")
    
    )    

    ##--------------------------------------------------------------------------
    ## return the modified table
    tbl    

}##END name_variables

##==============================================================================
## create_tidy:
## 
## From data set tblPART provided as input, create a second, independent tidy data set. 
##
create_tidy <- function(tblPART, lookupACTIV) {
    
    ##--------------------------------------------------------------------------
    ## Create a tidy data set with the mean of each non-key variable (total 66) 
    ## in tblPART, for each combination of 2 key variables (activity + subject).
    ## Note there are 180 unique combinations of these 2 key variables, because
    ## there are 6 activities and 30 subjects.
    tblTIDY <- matrix(nrow = 180, ncol = 68)
    tblMATCH <- vector(length = 180)
    for (a in 1:6) {
        for (s in 1:30) {
            r <- (a - 1) * 30 + s
            
            ## 2 key variables (their combination is unique)
            tblTIDY[r, 1] <- sprintf("%-20s", lookupACTIV[a])
            tblTIDY[r, 2] <- sprintf("%-10s", s)

            ## 66 non-key variables
            tblMATCH <- tblPART[, 1] == lookupACTIV[a] & as.numeric(tblPART[, 2]) == s
            for (c in 3:68) {
                tblTIDY[r, c] <- sprintf("%25s", mean(as.numeric(tblPART[tblMATCH, c])))
            }
        }
    }
    tblTIDY <- name_variables(tblTIDY)

    ##--------------------------------------------------------------------------
    ## return the newly created tidy data set
    tblTIDY

}##END create_tidy

##==============================================================================
## write_text:
## 
## Utility function that merely calls R's write.table with specified arguments. 
##
write_text <- function(tblname, filename, fieldsep, colnames) {
    
    write.table(
        x = tblname,
        file = filename,
        quote = FALSE,
        sep = fieldsep,
        col.names = colnames,
        row.names = FALSE
    )

}##END write_text
