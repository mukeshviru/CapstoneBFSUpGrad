#### Important packages
 # library(Information)
 # library(ggplot2)
 # library(reshape2)
 # require(scales)
 # library(dplyr)
 # library(gridExtra)
 # library(ROSE)

#### Loading the data
Credit_Bureau_Data <- read.csv("Credit Bureau data.csv", header = TRUE, na.strings = c(""," ","NA"),stringsAsFactors = FALSE)
Demographic_Data <- read.csv("Demographic data.csv", header = TRUE, na.strings = c(""," ","NA"),stringsAsFactors = FALSE)

#### Check structure of the data frames
# str(Credit_Bureau_Data)
# str(Demographic_Data)

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
# View(Credit_Bureau_Data[which(duplicated(Credit_Bureau_Data$Application.ID)),])
# View(Demographic_Data[which(duplicated(Demographic_Data$Application.ID)),])

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
# colnames(MasterData_copy)

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
IV1 <- create_infotables(data=MasterData_copy, y="Performance.Tag", bins=10, parallel=TRUE)
IV_Value <- data.frame(IV$Summary)
# IV_Value

## Variables' having IV value below 0.02 are not useful for prediction
## Hence the useful variables for prediction are
IV_Useful_Variables <- IV_Value[IV_Value$IV>=0.02,]
IV_Useful_Variables
## Hence we see 18 variables are very useful in prediction


#### EDA Analysis using ggplot
MasterData_copy$Performance.Tag <- as.factor(MasterData_copy$Performance.Tag)

## Analysis of Avgas.CC.Utilization.in.last.12.months
print(IV$Tables$Avgas.CC.Utilization.in.last.12.months, row.names=FALSE)

## Binning the variable Avgas.CC.Utilization.in.last.12.months for proper analysis
MasterData_copy$binning.Avgas.CC.Utilization.in.last.12.months <- as.factor(cut(MasterData_copy$Avgas.CC.Utilization.in.last.12.months, breaks = c(NA, 0, 4, 6, 8, 11, 14, 21,37, 51, 71, 113),include.lowest = TRUE))

d2 <- MasterData_copy %>% 
  group_by(binning.Avgas.CC.Utilization.in.last.12.months,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = binning.Avgas.CC.Utilization.in.last.12.months,Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Avgas.CC.Utilization.in.last.12.months", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)

## Analysis of No.of.PL.trades.opened.in.last.12.months
print(IV$Tables$No.of.PL.trades.opened.in.last.12.months, row.names=FALSE)
d2 <- MasterData_copy %>% 
  group_by(No.of.PL.trades.opened.in.last.12.months,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = factor(No.of.PL.trades.opened.in.last.12.months),Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "No.of.PL.trades.opened.in.last.12.months", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)

## Analysis of No.of.trades.opened.in.last.12.months
print(IV$Tables$No.of.trades.opened.in.last.12.months, row.names=FALSE)

## Binning the variable No.of.trades.opened.in.last.12.months for proper analysis
MasterData_copy$binning.No.of.trades.opened.in.last.12.months <- as.factor(cut(MasterData_copy$No.of.trades.opened.in.last.12.months, breaks = c(0, 1, 2, 3, 5, 7, 9, 12, 28),include.lowest = TRUE))

d2 <- MasterData_copy %>% 
  group_by(binning.No.of.trades.opened.in.last.12.months,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = binning.No.of.trades.opened.in.last.12.months,Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "No.of.trades.opened.in.last.12.months", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)

## Analysis of No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
print(IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,row.names = FALSE)

## Binning the variable No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. for proper analysis
MasterData_copy$binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.factor(cut(MasterData_copy$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = c(0, 1, 2, 3, 5, 8, 20),include.lowest = TRUE))

d2 <- MasterData_copy %>% 
  group_by(binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.No.of.trades.opened.in.last.12.months", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)

## Analysis of Outstanding.Balance
print(IV$Tables$Outstanding.Balance,row.names = FALSE)

## Binning the variable Outstanding.Balance for proper analysis
MasterData_copy$binning.Outstanding.Balance <- as.factor(cut(MasterData_copy$Outstanding.Balance, breaks = c(NA, 6843, 25509, 386813, 585402, 774228, 972455, 1357300, 2960998, 3282314, 5218801),include.lowest = TRUE))

d2 <- MasterData_copy %>% 
  group_by(binning.Outstanding.Balance,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = binning.Outstanding.Balance,Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Outstanding.Balance", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14) + theme(axis.text.x = element_text(angle = 90, hjust = 0.1))

## Analysis of No.of.times.30.DPD.or.worse.in.last.6.months
print(IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months,row.names = FALSE)

d2 <- MasterData_copy %>% 
  group_by(No.of.times.30.DPD.or.worse.in.last.6.months,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = factor(No.of.times.30.DPD.or.worse.in.last.6.months),Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "No.of.times.30.DPD.or.worse.in.last.6.months", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)

## Analysis of No.of.times.90.DPD.or.worse.in.last.12.months
d1<-ggplot(MasterData_copy, aes(x = factor(No.of.times.90.DPD.or.worse.in.last.12.months),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

d2<-ggplot(MasterData_copy, aes(x = factor(No.of.times.90.DPD.or.worse.in.last.12.months),fill=factor(MasterData_copy$Performance.Tag))) +geom_bar ()+geom_text(stat='count',aes(label=..count..),vjust=-.1) 
  
aa <- grid.arrange( d2,  d1, ncol=2)

## Analysis of No.of.times.30.DPD.or.worse.in.last.12.months
d1<-ggplot(MasterData_copy, aes(x = factor(No.of.times.30.DPD.or.worse.in.last.12.months),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

d2<-ggplot(MasterData_copy, aes(x = factor(No.of.times.30.DPD.or.worse.in.last.12.months),fill=factor(MasterData_copy$Performance.Tag))) +geom_bar ()+geom_text(stat='count',aes(label=..count..),vjust=-.1) 

aa1 <- grid.arrange( d2,  d1, ncol=2)

## Analysis of No.of.times.60.DPD.or.worse.in.last.6.months
d1<-ggplot(MasterData_copy, aes(x = factor(No.of.times.60.DPD.or.worse.in.last.6.months),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

d2<-ggplot(MasterData_copy, aes(x = factor(No.of.times.60.DPD.or.worse.in.last.6.months),fill=factor(MasterData_copy$Performance.Tag))) +geom_bar ()+geom_text(stat='count',aes(label=..count..),vjust=-.1) 

aa2 <- grid.arrange( d2,  d1, ncol=2)

## Analysis of No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
d1<-ggplot(MasterData_copy, aes(x = factor(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

d2<-ggplot(MasterData_copy, aes(x = factor(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.),fill=factor(MasterData_copy$Performance.Tag))) +geom_bar ()+geom_text(stat='count',aes(label=..count..),vjust=-.1) 

aa3 <- grid.arrange( d2,  d1, ncol=2)

## Analysis of No.of.trades.opened.in.last.6.months
d1<-ggplot(MasterData_copy, aes(x = factor(No.of.trades.opened.in.last.6.months),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

d2<-ggplot(MasterData_copy, aes(x = factor(No.of.trades.opened.in.last.6.months),fill=factor(MasterData_copy$Performance.Tag))) +geom_bar ()+geom_text(stat='count',aes(label=..count..),vjust=-.1) 

aa4 <- grid.arrange( d2,  d1, ncol=2)

## Analysis of No.of.times.60.DPD.or.worse.in.last.12.months
d1<-ggplot(MasterData_copy, aes(x = factor(No.of.times.60.DPD.or.worse.in.last.12.months),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

d2<-ggplot(MasterData_copy, aes(x = factor(No.of.times.60.DPD.or.worse.in.last.12.months),fill=factor(MasterData_copy$Performance.Tag))) +geom_bar ()+geom_text(stat='count',aes(label=..count..),vjust=-.1) 

aa5 <- grid.arrange( d2,  d1, ncol=2)

## Analysis of No.of.times.90.DPD.or.worse.in.last.6.months
d1<-ggplot(MasterData_copy, aes(x = factor(No.of.times.90.DPD.or.worse.in.last.6.months),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

d2<-ggplot(MasterData_copy, aes(x = factor(No.of.times.90.DPD.or.worse.in.last.6.months),fill=factor(MasterData_copy$Performance.Tag))) +geom_bar ()+geom_text(stat='count',aes(label=..count..),vjust=-.1) 


aa6 <- grid.arrange( d2,  d1, ncol=2)

## Analysis of No.of.PL.trades.opened.in.last.6.months
d1<-ggplot(MasterData_copy, aes(x = factor(No.of.PL.trades.opened.in.last.6.months),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

d2<-ggplot(MasterData_copy, aes(x = factor(No.of.PL.trades.opened.in.last.6.months),fill=factor(MasterData_copy$Performance.Tag))) +geom_bar ()+geom_text(stat='count',aes(label=..count..),vjust=-.1) 


aa7 <- grid.arrange( d2,  d1, ncol=2)

## Analysis of Total.No.of.Trades
trades<-ggplot(MasterData_copy, aes(x = factor(Total.No.of.Trades),fill=factor(MasterData_copy$Performance.Tag))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

trades

#### WOE Transformation
WOE_transformed <- MasterData_copy


## attribute - Avgas.CC.Utilization.in.last.12.months
# print(IV$Tables$Avgas.CC.Utilization.in.last.12.months, row.names=FALSE)

WOE_transformed$Avgas.CC.Utilization.in.last.12.months[which(is.na(WOE_transformed$Avgas.CC.Utilization.in.last.12.months))] <- 0.11
WOE_transformed$Avgas.CC.Utilization.in.last.12.months <- ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=72,0.39,
                                                                 ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=52,0.56,
                                                                        ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=38,0.58,
                                                                               ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=22,0.47,
                                                                                      ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=15,-0.08,
                                                                                             ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=12,-0.47,
                                                                                                    ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=9,-0.67,
                                                                                                           ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=7,-0.79,
                                                                                                                  ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=5,-0.8,
                                                                                                                         ifelse(WOE_transformed$Avgas.CC.Utilization.in.last.12.months>=0,-0.8,0.11))))))))))

## attribute - No.of.trades.opened.in.last.12.months
#print(IV$Tables$No.of.trades.opened.in.last.12.months)

WOE_transformed$No.of.trades.opened.in.last.12.months <- ifelse(WOE_transformed$No.of.trades.opened.in.last.12.months>=13,0.01,
                                                                ifelse(WOE_transformed$No.of.trades.opened.in.last.12.months>=10,0.49,
                                                                       ifelse(WOE_transformed$No.of.trades.opened.in.last.12.months>=8,0.57,
                                                                              ifelse(WOE_transformed$No.of.trades.opened.in.last.12.months>=6,0.44,
                                                                                     ifelse(WOE_transformed$No.of.trades.opened.in.last.12.months>=4,0.11,
                                                                                            ifelse(WOE_transformed$No.of.trades.opened.in.last.12.months==3,0.003,
                                                                                                   ifelse(WOE_transformed$No.of.trades.opened.in.last.12.months==2,-0.82,
                                                                                                          ifelse(WOE_transformed$No.of.trades.opened.in.last.12.months==1,-1.02,-0.65))))))))

## attribute - No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
# print(IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

WOE_transformed$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- ifelse(WOE_transformed$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>=9,0.01,
                                                                                          ifelse(WOE_transformed$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>=6,0.48,
                                                                                                 ifelse(WOE_transformed$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==5,0.59,
                                                                                                        ifelse(WOE_transformed$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==4,0.25,
                                                                                                               ifelse(WOE_transformed$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==3,0.16,
                                                                                                                      ifelse(WOE_transformed$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==2,0.14,
                                                                                                                             ifelse(WOE_transformed$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==1,-0.06,-1.07)))))))

## attribute - No.of.PL.trades.opened.in.last.12.months
# print(IV$Tables$No.of.PL.trades.opened.in.last.12.months)

WOE_transformed$No.of.PL.trades.opened.in.last.12.months <- ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==0,-0.89,
                                                                   ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==1,-0.13,
                                                                          ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==10,-0.07,
                                                                                 ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==11,0.08,
                                                                                        ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==12,0.92,
                                                                                               ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==2,0.25,
                                                                                                      ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==3,0.41,
                                                                                                             ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==4,0.5,
                                                                                                                    ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==5,0.43,
                                                                                                                           ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==6,0.38,
                                                                                                                                  ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==7,0.16,
                                                                                                                                         ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.12.months==8,0.01,0.1))))))))))))

## attribute - Total.No.of.Trades
# print(IV$Tables$Total.No.of.Trades)

WOE_transformed$Total.No.of.Trades <- ifelse(WOE_transformed$Total.No.of.Trades>=20,-0.07,
                                             ifelse(WOE_transformed$Total.No.of.Trades>=11,0.43,
                                                    ifelse(WOE_transformed$Total.No.of.Trades>=9,0.54,
                                                           ifelse(WOE_transformed$Total.No.of.Trades>=7,0.38,
                                                                  ifelse(WOE_transformed$Total.No.of.Trades==6,0.13,
                                                                         ifelse(WOE_transformed$Total.No.of.Trades==5,-0.05,
                                                                                ifelse(WOE_transformed$Total.No.of.Trades==4,-0.45,
                                                                                       ifelse(WOE_transformed$Total.No.of.Trades==3,-0.7,
                                                                                              ifelse(WOE_transformed$Total.No.of.Trades==2,-1.02,-0.07)))))))))

## attribute - Outstanding.Balance
# print(IV$Tables$Outstanding.Balance)

WOE_transformed$Outstanding.Balance <- ifelse(WOE_transformed$Outstanding.Balance>=3282409,0.29,
                                              ifelse(WOE_transformed$Outstanding.Balance>=2961005,-0.83,
                                                     ifelse(WOE_transformed$Outstanding.Balance>=1357399,-0.38,
                                                            ifelse(WOE_transformed$Outstanding.Balance>=972456,0.4,
                                                                   ifelse(WOE_transformed$Outstanding.Balance>=774241,0.43,
                                                                          ifelse(WOE_transformed$Outstanding.Balance>=585423,0.45,
                                                                                 ifelse(WOE_transformed$Outstanding.Balance>=386815,0.25,
                                                                                        ifelse(WOE_transformed$Outstanding.Balance>=25522,-0.13,
                                                                                               ifelse(WOE_transformed$Outstanding.Balance>=6847,-0.92,
                                                                                                      ifelse(WOE_transformed$Outstanding.Balance>=0,-0.77,-0.37))))))))))

## attribute -  No.of.times.30.DPD.or.worse.in.last.6.months
# print(IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months)

WOE_transformed$No.of.times.30.DPD.or.worse.in.last.6.months <- ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.6.months==7,0.48,
                                                                       ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.6.months==6,0.72,
                                                                              ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.6.months==5,1.05,
                                                                                     ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.6.months==4,0.95,
                                                                                            ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.6.months==3,0.77,
                                                                                                   ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.6.months==2,0.67,
                                                                                                          ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.6.months==1,0.46,-0.39)))))))

## attribute - No.of.PL.trades.opened.in.last.6.months
# print(IV$Tables$No.of.PL.trades.opened.in.last.6.months)

WOE_transformed$No.of.PL.trades.opened.in.last.6.months <- ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.6.months==6,-0.46,
                                                                  ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.6.months==5,0.05,
                                                                         ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.6.months==4,0.35,
                                                                                ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.6.months==3,0.42,
                                                                                       ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.6.months==2,0.44,
                                                                                              ifelse(WOE_transformed$No.of.PL.trades.opened.in.last.6.months==1,0.2,-0.65))))))

## attribute - No.of.times.30.DPD.or.worse.in.last.12.months
# print(IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months)

WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months <- ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==9,0,
                                                                        ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==8,0.77,
                                                                               ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==7,0.96,
                                                                                      ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==6,0.94,
                                                                                             ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==5,0.97,
                                                                                                    ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==4,0.81,
                                                                                                           ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==3,0.74,
                                                                                                                  ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==2,0.59,
                                                                                                                         ifelse(WOE_transformed$No.of.times.30.DPD.or.worse.in.last.12.months==1,0.07,-0.38)))))))))

## attribute - No.of.times.90.DPD.or.worse.in.last.12.months
# print(IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months)

WOE_transformed$No.of.times.90.DPD.or.worse.in.last.12.months <- ifelse(WOE_transformed$No.of.times.90.DPD.or.worse.in.last.12.months==5,1.3,
                                                                        ifelse(WOE_transformed$No.of.times.90.DPD.or.worse.in.last.12.months==4,0.96,
                                                                               ifelse(WOE_transformed$No.of.times.90.DPD.or.worse.in.last.12.months==3,0.89,
                                                                                      ifelse(WOE_transformed$No.of.times.90.DPD.or.worse.in.last.12.months==2,0.67,
                                                                                             ifelse(WOE_transformed$No.of.times.90.DPD.or.worse.in.last.12.months==1,0.51,-0.36)))))

## attribute - No.of.times.60.DPD.or.worse.in.last.6.months
# print(IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months)

WOE_transformed$No.of.times.60.DPD.or.worse.in.last.6.months <- ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.6.months==5,-0.34,
                                                                       ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.6.months==4,0.54,
                                                                              ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.6.months==3,0.67,
                                                                                     ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.6.months==2,0.93,
                                                                                            ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.6.months==1,0.87,-0.36)))))

## attribute - No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
# print(IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==0,-0.72,
                                                                                         ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==1,0.18,
                                                                                                ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==2,0.22,
                                                                                                       ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==3,0.55,
                                                                                                              ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==4,0.43,
                                                                                                                     ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==5,0.17,
                                                                                                                            ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==6,-0.01,
                                                                                                                                   ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==7,-0.2,
                                                                                                                                          ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==8,-0.07,
                                                                                                                                                 ifelse(WOE_transformed$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==9,-0.33,-0.85))))))))))
## attribute - No.of.trades.opened.in.last.6.months
# print(IV$Tables$No.of.trades.opened.in.last.6.months)

WOE_transformed$No.of.trades.opened.in.last.6.months[which(is.na(WOE_transformed$No.of.trades.opened.in.last.6.months))] <- 0
WOE_transformed$No.of.trades.opened.in.last.6.months <- ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==0,-0.66,
                                                               ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==1,-0.48,
                                                                      ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==2,0.23,
                                                                             ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==3,0.43,
                                                                                    ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==4,0.52,
                                                                                           ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==5,0.33,
                                                                                                  ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==6,0.1,
                                                                                                         ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==7,-0.07,
                                                                                                                ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==8,0.07,
                                                                                                                       ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==9,-0.22,
                                                                                                                              ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==10,-0.37,
                                                                                                                                     ifelse(WOE_transformed$No.of.trades.opened.in.last.6.months==11,-0.33,0))))))))))))

## attribute - No.of.times.60.DPD.or.worse.in.last.12.months
# print(IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months)

WOE_transformed$No.of.times.60.DPD.or.worse.in.last.12.months <- ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.12.months==0,-0.35,
                                                                        ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.12.months==1,0.21,
                                                                               ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.12.months==2,0.61,
                                                                                      ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.12.months==3,0.75,
                                                                                             ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.12.months==4,0.88,
                                                                                                    ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.12.months==5,0.81,
                                                                                                           ifelse(WOE_transformed$No.of.times.60.DPD.or.worse.in.last.12.months==6,1.1,0)))))))

## attriute - No.of.times.90.DPD.or.worse.in.last.6.months
# print(IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months)

WOE_transformed$No.of.times.90.DPD.or.worse.in.last.6.months <- ifelse(WOE_transformed$No.of.times.90.DPD.or.worse.in.last.6.months==0,-0.26,
                                                                       ifelse(WOE_transformed$No.of.times.90.DPD.or.worse.in.last.6.months==1,0.59,
                                                                              ifelse(WOE_transformed$No.of.times.90.DPD.or.worse.in.last.6.months==2,0.81,1.04)))

## attribute - No.of.months.in.current.residence
# print(IV$Tables$No.of.months.in.current.residence)

WOE_transformed$No.of.months.in.current.residence <- ifelse(WOE_transformed$No.of.months.in.current.residence>=98,-0.08,
                                                            ifelse(WOE_transformed$No.of.months.in.current.residence>=73,0.14,
                                                                   ifelse(WOE_transformed$No.of.months.in.current.residence>=50,0.13,
                                                                          ifelse(WOE_transformed$No.of.months.in.current.residence>=29,0.3,
                                                                                 ifelse(WOE_transformed$No.of.months.in.current.residence>=10,0.5,-0.27)))))

## attribute - Income
# print(IV$Tables$Income)

WOE_transformed$Income <- ifelse(WOE_transformed$Income>=49,-0.36,
                                 ifelse(WOE_transformed$Income>=42,-0.17,
                                        ifelse(WOE_transformed$Income>=37,-0.26,
                                               ifelse(WOE_transformed$Income>=32,-0.15,
                                                      ifelse(WOE_transformed$Income>=27,0.08,
                                                             ifelse(WOE_transformed$Income>=22,0.02,
                                                                    ifelse(WOE_transformed$Income>=17,0.08,
                                                                           ifelse(WOE_transformed$Income>=11,0.07,
                                                                                  ifelse(WOE_transformed$Income>=6,0.27,0.3)))))))))

## attribute - No.of.months.in.current.company
# print(IV$Tables$No.of.months.in.current.company)

WOE_transformed$No.of.months.in.current.company <- ifelse(WOE_transformed$No.of.months.in.current.company>=62,0.06,
                                                          ifelse(WOE_transformed$No.of.months.in.current.company>=54,-0.22,
                                                                 ifelse(WOE_transformed$No.of.months.in.current.company>=48,-0.22,
                                                                        ifelse(WOE_transformed$No.of.months.in.current.company>=41,-0.18,
                                                                               ifelse(WOE_transformed$No.of.months.in.current.company>=34,0.03,
                                                                                      ifelse(WOE_transformed$No.of.months.in.current.company>=27,-0.08,
                                                                                             ifelse(WOE_transformed$No.of.months.in.current.company>=20,0.04,
                                                                                                    ifelse(WOE_transformed$No.of.months.in.current.company>=13,0.21,
                                                                                                           ifelse(WOE_transformed$No.of.months.in.current.company>=6,0.17,0.1)))))))))

## Selecting only the required variables and omitting others
colnames(WOE_transformed)
WOE_transformed1 <- WOE_transformed[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,22,26,27,28)]
str(WOE_transformed1)


## Imbalanced Classification handling
table(WOE_transformed1$Performance.Tag)

# Generate synthetic data using rose
data.rose <- ROSE(Performance.Tag ~ .,data = WOE_transformed1,seed = 1)$data
table(data.rose$Performance.Tag)
