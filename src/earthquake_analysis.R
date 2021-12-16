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

#---------------------------------------------------------------
# Outlier Detection

# Importing the outliers detection library
library(outliers)

# Firstly, we will need to store outliers in a vector
outliers <- boxplot(Data$age)$out
outliers

# Then, we need to find out the rows where the outliers exist
Data[which(Data$age %in% outliers),]

# Now you can remove the rows containing the outliers
Data1 <- Data[-which(Data$age %in% outliers), ]

# If you check now with boxplot, you will notice that the outliers are gone
boxplot(x = Data1$age)

#-------------------------------------------------------------------

# We store the outliers in a vector
outliers <- boxplot(Data1$area_percentage)$out

# Then, we find where the outliers exist
Data1[which(Data1$area_percentage %in% outliers),]

# Now, we can remove the rows containing the outliers based on area percentage
Data2 <- Data1[-which(Data1$area_percentage %in% outliers),]

# If you check now with boxplot, you will notice that the outliers are gone
boxplot(x = Data2$area_percentage)

#--------------------------------------------------------------------------

# Identifying outliers and forming a boxplot from the result
outliers <- boxplot(Data2$height_percentage)$out

# Then, we find where the outliers exist
Data2[which(Data2$height_percentage %in% outliers),]

# Now, we can remove the rows containing the outliers baseed on height percentage
Data3 <- Data2[-which(Data2$height_percentage %in% outliers),]

# Outliers have been removed at this point
boxplot(x = Data3$height_percentage)

