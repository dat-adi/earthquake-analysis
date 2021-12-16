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

#--------------------------------------------------------------------

# Land check
# Allocating values to the Data based on the properties of the field
Data3$land_surface_condition[Data3$land_surface_condition == 'n'] <- 1
Data3$land_surface_condition[Data3$land_surface_condition == 'o'] <- 2
Data3$land_surface_condition[Data3$land_surface_condition == 't'] <- 3
Data3$land_surface_condition

land <- table(Data3$land_surface_condition)
land

# Roof check
# Allocating values to the Data based on the properties of the field
Data3$roof_type[Data3$roof_type == 'n'] <- 0
Data3$roof_type[Data3$roof_type == 'q'] <- 1
Data3$roof_type[Data3$roof_type == 'x'] <- 2
Data3$roof_type

roof <- table(Data3$roof_type)
roof

# Foundation check
# Allocating values to the Data based on the properties of the field
Data3$foundation_type[Data3$foundation_type == 'h'] <- 1
Data3$foundation_type[Data3$foundation_type == 'i'] <- 2
Data3$foundation_type[Data3$foundation_type == 'r'] <- 3
Data3$foundation_type[Data3$foundation_type == 'u'] <- 4
Data3$foundation_type[Data3$foundation_type == 'w'] <- 5
Data3$foundation_type

foundation <- table(Data3$foundation_type)
foundation

# Ground Floor check
# Allocating values to the Data based on the properties of the field
Data3$ground_floor_type[Data3$ground_floor_type == 'f'] <- 1
Data3$ground_floor_type[Data3$ground_floor_type == 'm'] <- 2
Data3$ground_floor_type[Data3$ground_floor_type == 'v'] <- 3
Data3$ground_floor_type[Data3$ground_floor_type == 'x'] <- 4
Data3$ground_floor_type[Data3$ground_floor_type == 'z'] <- 5
Data3$ground_floor_type

ground <- table(Data3$ground_floor_type)
ground

# Other Floor check
# Allocating values to the Data based on the properties of the field
Data3$other_floor_type[Data3$other_floor_type == 'j'] <- 1
Data3$other_floor_type[Data3$other_floor_type == 'q'] <- 2
Data3$other_floor_type[Data3$other_floor_type == 's'] <- 3
Data3$other_floor_type[Data3$other_floor_type == 'x'] <- 4
otherfloor <- table(Data3$other_floor_type)
otherfloor

# Position check
# Allocating values to the Data based on the properties of the field
Data3$position[Data3$position == 'j'] <- 1
Data3$position[Data3$position == 'o'] <- 2
Data3$position[Data3$position == 's'] <- 3
Data3$position[Data3$position == 't'] <- 4
position <- table(Data3$position)
position

# Plan configuration check
# Allocating values to the Data based on the properties of the field
Data3$plan_configuration[Data3$plan_configuration == 'a'] <- 1
Data3$plan_configuration[Data3$plan_configuration == 'c'] <- 2
Data3$plan_configuration[Data3$plan_configuration == 'd'] <- 3
Data3$plan_configuration[Data3$plan_configuration == 'f'] <- 4
Data3$plan_configuration[Data3$plan_configuration == 'm'] <- 5
Data3$plan_configuration[Data3$plan_configuration == 'n'] <- 6
Data3$plan_configuration[Data3$plan_configuration == 'o'] <- 7
Data3$plan_configuration[Data3$plan_configuration == 'q'] <- 8
Data3$plan_configuration[Data3$plan_configuration == 's'] <- 9
Data3$plan_configuration[Data3$plan_configuration == 'u'] <- 10
plan <- table(Data3$plan_configuration)
plan

# Legal Ownership status check
# Allocating values to the Data based on the properties of the field
Data3$legal_ownership_status[Data3$legal_ownership_status == 'a'] <- 1
Data3$legal_ownership_status[Data3$legal_ownership_status == 'r'] <- 2
Data3$legal_ownership_status[Data3$legal_ownership_status == 'v'] <- 3
Data3$legal_ownership_status[Data3$legal_ownership_status == 'w'] <- 4

legal <- table(Data3$legal_ownership_status)
legal

# Finding a mean from the given data
mean <- mean(Data3$age, na.rm = TRUE)
mean
sum(is.na(Data3))

# Allocating to the age column
Data3$age[Data3$age == '0'] <- mean
Data3$age

age <- table(Data3$age)
age
