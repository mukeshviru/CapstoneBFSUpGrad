# library(dplyr)
# library(scorecard)
# suppressPackageStartupMessages( require(scorecard) )
 library(ggplot2)
# library(tidyverse)
# library(oetteR)
# library(dplyr)
# library(tibble)
# library(magrittr)
# library('Rcpp')
# library(sentimentr)
# library(highr)
# library(ROSE)
# library(caret)
# library(caTools)
# library(dummies)
# library(MASS)
# library(gridExtra)
library(reshape2)
library(lattice)
library(tidyr)
library(corrplot)

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


## Removing the rows of duplicate application ids
Credit_Bureau_Data <- Credit_Bureau_Data[-which(duplicated(Credit_Bureau_Data$Application.ID)),]
Demographic_Data <- Demographic_Data[-which(duplicated(Demographic_Data$Application.ID)),]

#### Checking NA values in the data frames in respective rows
sapply(Credit_Bureau_Data,function(x) sum(is.na(x)))
sapply(Demographic_Data,function(x) sum(is.na(x)))
# There are 1425 records in each data frames where Perfromance Tag (dependent variable) have NAs

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

## Storing the records whose performance tag have NA in another data frame for future analysis and removing them from current data frame
NA_MasterData <- MasterData[which(is.na(MasterData$Performance.Tag)),]
MasterData <- MasterData[-which(is.na(MasterData$Performance.Tag)),]

NA_Demographic_Data <- Demographic_Data[which(is.na(Demographic_Data$Performance.Tag)),]
Demographic_Data <- Demographic_Data[-which(is.na(Demographic_Data$Performance.Tag)),]

# generating information values using MULT function for selecting attributes with acceptable predictive power
data = MasterData %>%
  as_tibble()

data_demographic = Demographic_Data %>%
  as_tibble()

str(data)
data[] <- lapply( data, factor)
data_demographic[] <- lapply( data_demographic, factor)

iv = iv(data, y = 'Performance.Tag') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv_deomgraphic = iv(data_demographic, y = 'Performance.Tag') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv %>%
  knitr::kable()
iv2 <- iv[iv$info_value>=0.02,]
iv3 <-iv[iv$info_value<=0.02,]
iv2$variable
iv3$variable

#### EDA Analysis using ggplot on the 18 selected variables based on the information values of each
MasterData_copy <- MasterData
MasterData_copy$Performance.Tag <- as.factor(MasterData_copy$Performance.Tag)

## Analysis of Avgas.CC.Utilization.in.last.12.months

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
d2 <- MasterData_copy %>% 
  group_by(No.of.PL.trades.opened.in.last.12.months,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = factor(No.of.PL.trades.opened.in.last.12.months),Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "No.of.PL.trades.opened.in.last.12.months", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)

## Analysis of No.of.trades.opened.in.last.12.month

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

# Now removing the data as per information value but include the performance tag
colnames(data)
data1 <- data[,c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 23, 27, 28, 29)]
summary(data1$Performance.Tag)

## Imbalanced Classification handling
table(data1$Performance.Tag)
# since there are imbalanced in data for defaulters vs non-edfaulters, we will synthesize data

# Generate synthetic data using rose
data1[] <- lapply(data1, function(x) as.factor(as.character(x)))
data.rose <- ROSE(Performance.Tag ~ .,data = data1,seed = 1)$data
table(data.rose$Performance.Tag)
str(data.rose)
data1<-data.rose


## Converted the variables having more than 20 levels to numeric so as to bin it and calculate WOE
col_names <- c('Avgas.CC.Utilization.in.last.12.months','Outstanding.Balance','No.of.trades.opened.in.last.12.months','No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.','Total.No.of.Trades','Income','No.of.months.in.current.residence','No.of.months.in.current.company')
data1[, col_names] <- lapply(col_names, function(x) as.numeric(as.character(data1[[x]])))
summary(data1)
str(data1)
bins = woebin(data1, y = 'Performance.Tag')

woebin_plot(bins$Avgas.CC.Utilization.in.last.12.months)
woebin_plot(bins$Outstanding.Balance)
woebin_plot(bins$No.of.trades.opened.in.last.6.months)
woebin_plot(bins$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
woebin_plot(bins$Total.No.of.Trades)
woebin_plot(bins$No.of.months.in.current.company)
woebin_plot(bins$No.of.months.in.current.residence)

#### WOE Transformation
woe_data1 = woebin_ply( data1, bins ) %>%
  as_tibble()

woe_data_demographic = woebin_ply( data_demographic, bins ) %>%
  as_tibble()
  
summary(woe_data1)
str(woe_data1)

#### Logistic Regression##############################################
library(dplyr)
library(MASS)
library(caTools)
library(woeBinning)
library(scorecard)


#### For Demographic Data#############################################
#Model Building (only using demographic)

# Creating dummy variables
woe_data_demographic2 <-woe_data_demographic
woe_data_demographic2[] <- lapply(woe_data_demographic2, function(x) as.factor(as.character(x)))
woe_data_demographic2$Performance.Tag <- as.integer(woe_data2$Performance.Tag)
str(woe_data_demographic2)
k1 <- woe_data_demographic2
woe_data_demographic2$Performance.Tag <- as.factor(ifelse(woe_data_demographic2$Performance.Tag == 1, "yes", "no"))
summary(woe_data_demographic2)
# splitting into train and test data
set.seed(1)
split_indices <- sample.split(woe_data_demographic2$Performance.Tag, SplitRatio = 0.70)

train <- woe_data_demographic2[split_indices, ]
test <- woe_data_demographic2[!split_indices, ]

nrow(train)/nrow(woe_data_demographic2)
nrow(test)/nrow(woe_data_demographic2)

lm_model1 <- glm(Performance.Tag~.,family=binomial, demo_train)

summary(lm_model1)

#library(MASS)

lm_model2 <- stepAIC(lm_model1)

summary(lm_model2)

# Removing Varibale through VIF
library(car)

vif(lm_model2)

# Removing woe.Age.binned based on p-value 

lm_model3<- glm(formula = Performance.Tag ~ woe.No.of.months.in.current.residence.binned + 
                  woe.Income.binned + woe.No.of.months.in.current.company.binned + 
                  woe.No.of.dependents.binned + woe.Profession.binned + 
                  woe.Type.of.residence.binned + 
                  woe.Education.binned + woe.Gender.binned, family = binomial, 
                data = demo_train)


summary(lm_model3)
sort(vif(lm_model))
# library(car)

#REmoved woe.Education.binned

lm_model4 <- glm(formula = Performance.Tag ~ woe.No.of.months.in.current.residence.binned + 
                   woe.Income.binned + woe.No.of.months.in.current.company.binned + 
                   woe.No.of.dependents.binned + woe.Profession.binned + 
                   woe.Gender.binned, family = binomial, 
                 data = demo_train)


summary(lm_model4)


#REmoved + woe.Gender.binned 
lm_model5<-glm(formula = Performance.Tag ~ woe.No.of.months.in.current.residence.binned + 
                 woe.Income.binned + woe.No.of.months.in.current.company.binned + 
                 woe.No.of.dependents.binned + woe.Profession.binned , family = binomial, 
               data = demo_train)



summary(lm_model5)

names(demo_test)

# Predict on test data

pred_prob<-predict(lm_model5,demo_test[,-11],type="response")

pred_prob

fitted.results <- ifelse(pred_prob > 0.5,1,0)

misClasificError <- mean(fitted.results != demo_test$Performance.Tag)
print(paste('Accuracy',1-misClasificError))

#Model Evaluation
#predicted probabilities for test data

test_pred = predict(lm_model5, type = "response", 
                    newdata = demo_test[,-11])

summary(test_pred)

demo_test$prob <- test_pred
View(demo_test)

# Let's use the probability cutoff of 50%.

test_pred_prob <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_prob <- factor(ifelse(demo_test$Performance.Tag==1,"Yes","No"))

table(test_actual_prob,test_pred_prob)

test_pred_prob <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))


# Creating dummy variables
#### For Master Data#############################################

# Creating dummy variables
woe_data2<-woe_data1
woe_data2[] <- lapply(woe_data2, function(x) as.factor(as.character(x)))
woe_data2$Performance.Tag <- as.integer(woe_data2$Performance.Tag)
str(woe_data2)
k1 <- woe_data2
woe_data2$Performance.Tag <- as.factor(ifelse(woe_data2$Performance.Tag == 1, "yes", "no"))
summary(woe_data2)
# splitting into train and test data
set.seed(1)
split_indices <- sample.split(woe_data2$Performance.Tag, SplitRatio = 0.70)

train <- woe_data2[split_indices, ]
test <- woe_data2[!split_indices, ]

nrow(train)/nrow(woe_data2)
nrow(test)/nrow(woe_data2)

# Model 1: Logistic Regression
logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train)
summary(logistic_1)

# Using stepwise algorithm for removing insignificant variables 
logistic_2 <- stepAIC(logistic_1, direction = "both")

logistic_3 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.6.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe + No.of.PL.trades.opened.in.last.12.months_woe + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Total.No.of.Trades_woe + Income_woe + No.of.months.in.current.residence_woe + 
                    No.of.months.in.current.company_woe, family = "binomial", data = train)
# checking vif for logistic_3 
vif(logistic_3)
summary(logistic_3)

# removing "No.of.PL.trades.opened.in.last.12.months_woe"since vif is high and also the variable is not significant 
logistic_4 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.6.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Total.No.of.Trades_woe + Income_woe + No.of.months.in.current.residence_woe + 
                    No.of.months.in.current.company_woe, family = "binomial", data = train)
# checking vif for logistic_3 
vif(logistic_4)
summary(logistic_4)

# removing "No.of.times.60.DPD.or.worse.in.last.12.months_woe"since vif is high and also the variable is not significant 
logistic_5 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months_woe +
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.6.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Total.No.of.Trades_woe + Income_woe + No.of.months.in.current.residence_woe + 
                    No.of.months.in.current.company_woe, family = "binomial", data = train)
# checking vif for logistic_3 
vif(logistic_5)
summary(logistic_5)

# removing "No.of.PL.trades.opened.in.last.6.months_woe"since vif is high and also the variable is not significant 
logistic_6 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months_woe +
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.6.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Total.No.of.Trades_woe + Income_woe + No.of.months.in.current.residence_woe + 
                    No.of.months.in.current.company_woe, family = "binomial", data = train)
# checking vif for logistic_3 
vif(logistic_6)
summary(logistic_6)

# removing "Total.No.of.Trades_woe"since vif is high and also the variable is not significant 
logistic_7 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months_woe +
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.6.months_woe + No.of.trades.opened.in.last.12.months_woe + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Income_woe + No.of.months.in.current.residence_woe + 
                    No.of.months.in.current.company_woe, family = "binomial", data = train)
# checking vif for logistic_3 
vif(logistic_7)
summary(logistic_7)

# removing "No.of.trades.opened.in.last.6.months_woe"since vif is high and also the variable is not significant 
logistic_8 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months_woe +
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.12.months_woe + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Income_woe + No.of.months.in.current.residence_woe + 
                    No.of.months.in.current.company_woe, family = "binomial", data = train)
# checking vif for logistic_3 
vif(logistic_8)
summary(logistic_8)

# removing "No.of.months.in.current.residence_woe"since vif is high and also the variable is not significant 
logistic_9 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months_woe +
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.12.months_woe + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Income_woe + 
                    No.of.months.in.current.company_woe, family = "binomial", data = train)
# checking vif for logistic_3 
vif(logistic_9)
summary(logistic_9)

logistic_final <- logistic_9

# Predicting probabilities of responding for the test data
colnames(test)
predictions_logit <- predict(logistic_final, newdata = test[, -1], type = "response")
summary(predictions_logit)

# Model Evaluation: Logistic Regression
# After checking for various cutoff, we got better results at cut off value 45.5%.
predicted_response <- factor(ifelse(predictions_logit >= 0.455, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")
conf
# #####splitting into train and test data for random forest and Naive bias##############################
set.seed(1)
split_indices <- sample.split(woe_data1$Performance.Tag, SplitRatio = 0.70)

train <- woe_data1[split_indices, ]
test <- woe_data1[!split_indices, ]

nrow(train)/nrow(woe_data1)
nrow(test)/nrow(woe_data1)

############################################################
#Random forest 
rf_model<- randomForest::randomForest(formula=Performance.Tag~., mtry=70, ntrees=300,data=train)

rf_model

pred_rf<-predict(rf_model,newdata = test)
pred_rf

library(caret)
confusionMatrix(data=pred_rf,test$Performance.Tag,positive = "1")
################################################################
#NAIVE BAYES
library(e1071)
NB_model<-naiveBayes(Performance.Tag~.,data=train)
NB_model
preds_val <- predict(NB_model, newdata = test)
confusionMatrix(preds_val,test$Performance.Tag,positive="1")


########## Score Card #########################################
colnames(woe_data2)

woe_data2$predict_default <- predict(logistic_final, newdata = woe_data2[, -1], type = "response")
woe_data2$predict_nondefault <- 1-woe_data2$predict_default
woe_data2$odds <-  log(woe_data2$predict_nondefault/woe_data2$predict_default)

Factor = PDO/log(2)
Offset = 400 - (Factor*log(10))
PDO = 20

woe_data2$Score <- ceiling(Offset + (Factor*woe_data2$odds))
str(woe_data2$Score)
summary(woe_data2$Score)
## min - 288 to max - 365

quantile(woe_data2$Score,seq(0,1,0.01))

#### Plotting data between scores and Performance.Tag
d2 <- woe_data2 %>% 
  group_by(Score,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = Score,Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Score", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)

###### Comparing scores of defaulters and non defaulters
woe_data2_defaulters <- woe_data2[which(woe_data2$Performance.Tag=='yes'),]
summary(woe_data2_defaulters$Score)

woe_data2_nondefaulters <- woe_data2[which(woe_data2$Performance.Tag=='no'),]
summary(woe_data2_nondefaulters$Score)

woe_data2_less_330<- woe_data2[which(woe_data2$Score<330),]
d2 <- woe_data2_less_330 %>% 
  group_by(Score,Performance.Tag) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

ggplot(d2, aes(x = Score,Performance.Tag, y = perc*100, fill = factor(Performance.Tag))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Score", y = "Percent", fill = "Performance Tag") +
  theme_minimal(base_size = 14)


## From the plot and comparison it is evident that score cut off could be set to 398.
cutoff_score = 330
