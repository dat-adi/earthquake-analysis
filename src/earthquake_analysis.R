# Setting up the workspace
#--------------------------------------------------------------
# Setting up the working directory
setwd("~/DA/earthquake-analysis/src/datasets")
getwd()

# Reading data from the csv into the Data variable
Data <-
  read.csv("earthquake_dataset.csv",
           stringsAsFactors = FALSE,
           header = T)
Data

# Cleaning up NA values from the data
sum(is.na(Data))
Data <- na.omit(Data)
sum(is.na(Data))

# Viewing and checking the data
table(Data$age)
View(Data)
summary(Data)
str(Data)
colnames(Data)

