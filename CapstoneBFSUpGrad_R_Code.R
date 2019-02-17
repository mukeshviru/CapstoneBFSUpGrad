#### Important packages
#install.packages("Information")
#library(Information)
#library(ggplot2)
#library(reshape2)
#require(scales)
#library(dplyr)

#### Loading the data
Credit_Bureau_Data <- read.csv("Credit Bureau data.csv", header = TRUE, na.strings = c(""," ","NA"),stringsAsFactors = FALSE)
Demographic_Data <- read.csv("Demographic data.csv", header = TRUE, na.strings = c(""," ","NA"),stringsAsFactors = FALSE)

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

## Storing the records whose performance tag have NA in another data frame for future analysis and removing them from current data frame
NA_PerfTag_Credit_Bureau <- Credit_Bureau_Data[which(is.na(Credit_Bureau_Data$Performance.Tag)),]
Credit_Bureau_Data <- Credit_Bureau_Data[-which(is.na(Credit_Bureau_Data$Performance.Tag)),]

NA_PerfTag_Demographic <- Demographic_Data[which(is.na(Demographic_Data$Performance.Tag)),]
Demographic_Data <- Demographic_Data[-which(is.na(Demographic_Data$Performance.Tag)),]

#### Comparing Application ID in both the data frames
matching <- match(Credit_Bureau_Data$Application.ID,Demographic_Data$Application.ID,nomatch = 0)
sum(which(matching==0))
# Since there is no 0 value. All the application ids are present in both the data frames

#### Preparing master file by combining both
MasterData <- merge(Credit_Bureau_Data,Demographic_Data,by="Application.ID",all = FALSE)

## Removing duplicate columns of performance tag found in the MasterData dataframe
MasterData$Performance.Tag.x <- NULL

## Renaming the Performance.Tag.y to Performance.Tag
names(MasterData)[names(MasterData) == 'Performance.Tag.y'] <- 'Performance.Tag'

#### WOE and IV
MasterData_copy <- MasterData
colnames(MasterData_copy)

## Removing Application ID as it is not required for WOE/IV analysis
MasterData_copy$Application.ID <- NULL

## Converting all independent vaariables to factor
MasterData_copy[] <- lapply( MasterData_copy, factor)
MasterData_copy$Performance.Tag <- as.numeric(as.character(MasterData_copy$Performance.Tag))

## Converted the variables having more than 20 levels to numeric so as to bin it and calculate WOE
col_names <- c('Avgas.CC.Utilization.in.last.12.months','Outstanding.Balance','No.of.trades.opened.in.last.12.months','No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.','Total.No.of.Trades','Income','No.of.months.in.current.residence','No.of.months.in.current.company')
MasterData_copy[, col_names] <- lapply(col_names, function(x) as.numeric(as.character(MasterData_copy[[x]])))

## Calculation of WOE and IV
IV <- create_infotables(data=MasterData_copy, y="Performance.Tag", bins=10, parallel=TRUE)
IV_Value <- data.frame(IV$Summary)
IV_Value

## Variables' having IV value below 0.02 are not useful for prediction
## Hence the useful variables for prediction are
IV_Useful_Variables <- IV_Value[IV_Value$IV>=0.02,]
IV_Useful_Variables
## Hence we see 18 variables are very useful in prediction

print(IV$Tables$Avgas.CC.Utilization.in.last.12.months, row.names=FALSE)

#### EDA Analysis using ggplot
MasterData_copy$Performance.Tag <- as.factor(MasterData_copy$Performance.Tag)

print(IV$Tables$Avgas.CC.Utilization.in.last.12.months, row.names=FALSE)
d2 <- MasterData_copy %>% 
  group_by(Avgas.CC.Utilization.in.last.12.months,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = factor(Avgas.CC.Utilization.in.last.12.months), y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Avgas.CC.Utilization.in.last.12.months", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)



