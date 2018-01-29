# GettingAndCleaningDataProject
COURSERA.org - Data Science Specialization - Getting And Cleaning Data Project (Peer-Graded)

INTRODUCTION - The University of California, Irvine (UCI) conducted Human Activity Recognition (HAR) experiments using a specific type of Samsung smartphone, and published the resultant dataset in their Machine Learning Repository. Their dataset, namely https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip, is well described in a set of text files (specifically README.txt, feature_info.txt and feature.txt) in the root folder (UCI HAR dataset) of the .zip file, as well as here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.

REPOSITORY CONTENTS:
* This README.md.
* R SCRIPT run_analysis.R - prepares a tidy sub-set of data from the UCI HAR dataset provided in the zip file, as required for the project (note: the script contains clear comments with regards to the project steps and related details).
* CODEBOOK - describes the tidy dataset prepared by the script.

PRE-REQUISITES for running the R script:
* Download https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip into your working directory.
* Save script "run_analysis.R" in your working directory.
* Create a sub-folder "run_analysis_out" directly under your working directory.
* You may have to install package "readr", if you haven't already done so - the script uses the read_table functionality of the readr library.

RUNNING the R script:
* Source function run_analysis() into your R environment - in R Studio, this can be done by opening "run_analysis.R", selecting "Source on Save", and saving the file.
* Type "run_analysis()" at the command line, and enter.

OUTPUT from running the R script:
* At the outset, the script will unzip "UCI_HAR_dataset.zip" (note: the inherent root folder is "UCI HAR dataset").
* At the end, the script will generate "step5_TIDY.txt" under sub-folder "run_analysis_out" (created as a PRE-REQUISITE).
