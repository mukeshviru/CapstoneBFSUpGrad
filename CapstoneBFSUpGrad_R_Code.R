#### Loading the data
Credit_Bureau_Data <- read.csv("Credit Bureau data.csv", header = TRUE, na.strings = c(""," ","NA"))
Demographic_Data <- read.csv("Demographic data.csv", header = TRUE, na.strings = c(""," ","NA"))

#### Check structure of the data frames
str(Credit_Bureau_Data)
str(Demographic_Data)

#### Checking duplicates
## Comparing whole rows for duplicates
sum(duplicated(Credit_Bureau_Data))
sum(duplicated(Demographic_Data))
# No duplicate rows were found

## Comparing the Application IDs for duplicates
sum(duplicated(Credit_Bureau_Data$Application.ID))
sum(duplicated(Demographic_Data$Application.ID))
# 3 duplicates found in the application ids in both data sets

## Checking the complete rows of the duplicated application ids
View(Credit_Bureau_Data[which(duplicated(Credit_Bureau_Data$Application.ID)),])
View(Demographic_Data[which(duplicated(Demographic_Data$Application.ID)),])

## Removing the rows of duplicate application ids
Credit_Bureau_Data <- Credit_Bureau_Data[-which(duplicated(Credit_Bureau_Data$Application.ID)),]
Demographic_Data <- Demographic_Data[-which(duplicated(Demographic_Data$Application.ID)),]

#### Checking NA values in the data frames in respective rows
sapply(Credit_Bureau_Data,function(x) sum(is.na(x)))
sapply(Demographic_Data,function(x) sum(is.na(x)))
# There are 1425 records in each data frames where Perfromance Tag (dependent variable) have NAs

## Removing the rows of duplicate application ids
Credit_Bureau_Data <- Credit_Bureau_Data[-which(is.na(Credit_Bureau_Data$Performance.Tag)),]
Demographic_Data <- Demographic_Data[-which(is.na(Demographic_Data$Performance.Tag)),]

#### Comparing Application ID in both the data frames
matching <- match(Credit_Bureau_Data$Application.ID,Demographic_Data$Application.ID,nomatch = 0)
sum(which(matching==0))
# Since there is no 0 value. All the application ids are present in both the data frames
