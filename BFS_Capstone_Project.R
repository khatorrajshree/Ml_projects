## Module - BFS Capstone Project
## Name - Rajshree

## Business Understanding:
# CredX is a leading credit card provider that gets thousands of credit card applicants every year. 
# But in the past few years,it has experienced an increase in credit loss. 
# The company believes that the best strategy to mitigate credit risk is to 'acquire the right customers
# We have been provided with all the prospects related informations in 2 different excel files. 

## AIM:
# Identify the right customers using predictive models
# Determine the factors affecting credit risk using past data of bank's applicant
# Create strategies to mitigate the acquisition risk and access the financial benefit of project and
# Build an application scorecard and identify the cut-off score below which we would not grant credit cards to applicants.

## Data Files -   Demographic data.csv & Credit Bureau data.csv
## Data Frames 
  # df_demographic  : df contains data related to demographic
  # df_CreditBureau : df contains data related to CreditBureau
  # df_NO_CC_Demo   : df contains data related with rejected population of demographic data
  # df_merge        : df contains combined data of demographic and creditBureau
  # df_woe          : df contains woe values for all the variables
  # df_NO_CC_woe    : df conatains rejected population of merged file
################################################################################################################
## Instructions - Set the working directory path and place the data files in same path
## Remove existing memory space if any

remove(list=ls())
suppressWarnings(library(RODBC))
## read.csv() function is used to read given csv file into a data frames
## na.string() function is used to replace all blanks values to NA values

df_demographic <- read.csv("Demographic data.csv",na.strings = c(""," "))
df_CreditBureau <- read.csv("Credit Bureau data.csv",na.strings = c(""," "))
################################################################################################################
## Library & Packages
# install.packages("ROSE")
# install.packages("DMwR")
# install.packages("xgboost")
# install.packages("rpart")
# install.packages("rattle")
# install.packages("Information")
# install.package("fastmatch")
# install.packages("ROCR")
# install.packages("AUC")
# devtools::install_git("https://github.com/klarsen1/Information.git")
options(scipen=10)
library(Information)
library(ggplot2)
library(dummies)
library(caret)
library(caTools)
library(MASS)
library(car)
library(dplyr)
library(tidyr)
library(randomForest)
library(DMwR)
library(kernlab)
library(readr)
library(gridExtra)
library(xgboost)
library(mlr) 
library(rpart)
library(rattle)
library(corrplot)
library(data.table)
library(ROCR)
library(AUC)
##############################################################################################################################
## 1) Data Understanding
#   dim() function returns the number of observations and variables. 
dim(df_demographic)
dim(df_CreditBureau)
# Demographic dataset has 71295 observations of 12 variables
# creditBureau dataset has 71295 observations of 19 variables
#  names() function returns the variable names of the dataframe. 
names(df_demographic)
#[1] "Application.ID"           "Age"                                        
#[3] "Gender"                   "Marital.Status..at.the.time.of.application."
#[5] "No.of.dependents"         "Income"                                     
#[7] "Education"                "Profession"                                 
#[9] "Type.of.residence"        "No.of.months.in.current.residence"          
#[11] "No.of.months.in.current.company"     "Performance.Tag"     
names(df_CreditBureau)
#[1] "Application.ID"                                [2] "No.of.times.90.DPD.or.worse.in.last.6.months"                   
#[3] "No.of.times.60.DPD.or.worse.in.last.6.months"  [4] "No.of.times.30.DPD.or.worse.in.last.6.months"                   
#[5] "No.of.times.90.DPD.or.worse.in.last.12.months" [6] "No.of.times.60.DPD.or.worse.in.last.12.months"                  
#[7] "No.of.times.30.DPD.or.worse.in.last.12.months" [8] "Avgas.CC.Utilization.in.last.12.months"                         
#[9] "No.of.trades.opened.in.last.6.months"          [10] "No.of.trades.opened.in.last.12.months"                          
#[11] "No.of.PL.trades.opened.in.last.6.months"      [12] "No.of.PL.trades.opened.in.last.12.months"                       
#[13] "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." 
#[14] "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
#[15] "Presence.of.open.home.loan"   [16] "Outstanding.Balance"    [17] "Total.No.of.Trades"                                             
#[18] "Presence.of.open.auto.loan"   [19] "Performance.Tag" 
#  str() function helps to understand the structure of the dataframe as to  
#  which variables have what datatype and displays first few value of the variable.
## Application ID is the unique key for both the datasets. 
## Performance Tag is the dependant variable whose value we are predicting. 
str(df_demographic)
str(df_CreditBureau)
# head() function returns first few values of all the variables of the data frame. 
head(df_demographic)
head(df_CreditBureau)
## Assumptions
##  1) All the amount value columns like loan amount, income, outstanding balance etc. are given in the same currency.
## 2) The income is monthly income and is in units of thousands. 
## 3) The applicants for which Performance tag is missing(NA) are considered as the rejected cases and will be used to assess the model performance. 

##############################################################################################################################
## 2) Data Cleanup
# Removed Duplicate using duplicated() function . 3 unique application ids removed from both the dataframes

df_demographic <- df_demographic[!duplicated(df_demographic$Application.ID),] 
df_CreditBureau <- df_CreditBureau[!duplicated(df_CreditBureau$Application.ID),]
# Merge both the dfs using merge() function and before merge options checked the 
# difference between application Ids and Performance tags using setdiff() function
# Created a new merged dataframe "df_merge" which has all the columns of both the data files
setdiff(df_demographic$Application.ID,df_CreditBureau$Application.ID)
sum(!is.na(df_demographic$Performance.Tag) - !is.na(df_CreditBureau$Performance.Tag))
df_merge <- merge(df_CreditBureau,df_demographic,by="Application.ID")
# This shows there is no different values for application ids & performance tags in both the DFs
# Removed application Id & one of the Performance tag columns from master dataframe 
# since both the performance tags columns have no difference & rename one of them using colnames() 
# master dataframe has 71292 observations of 28 variables
df_merge <- df_merge[,-c(1,19)]
colnames(df_merge)[28] <- "Performance.Tag"
str(df_merge)

# NA Treatments on demographic and creditbureau dataframes
# function used : sum (),is.na(),sapply(),rowSums()
# we have total 71292 observations out of which 1577 & 3028 NAs values in both the dataframes respectively which is nearly 2% data of 71292 observation so we can safely remove it.
# but,before removing NAs values from demographic, created a new dataframe of rejected population which have performance tags as NA's
sum(is.na(df_demographic))
sum(is.na(df_CreditBureau))
sapply(df_demographic, function(x) sum(is.na(x)))
sapply(df_CreditBureau, function(x) sum(is.na(x)))
df_No_CC_Demo <- df_demographic[is.na(df_demographic$Performance.Tag),]
df_demographic_model <- df_demographic[rowSums(is.na(df_demographic)) <= 0.01,]
sum(is.na(df_demographic_model))
df_CreditBureau_model <- df_CreditBureau[rowSums(is.na(df_CreditBureau)) <= 0.01,]
sum(is.na(df_CreditBureau_model))
# df_No_CC_Demo dataframe has 1425 observations of 12 variables
# df_demographic_model has 69718 observations of 12 variables
# df_CreditBureau_model has 68844 observation of 19 variables
######################################################################################################
## Exploratory Data Analysis using Plots on demographic data 

# Writing a function "plot_response" to do the same task for each variable
plot_response <- function(cat_var, var_name){
  a <- aggregate(`Performance.Tag`~cat_var,df_demographic_model, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "Default_Rate","Total")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  ggplot(agg_response, aes(agg_response[, 1], count, label = Default_Rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
}

# Univariate & Bivariate Analysis  
# Functions used - quantile(),which(),cut(),boxplot(),ggplot()
#1) AGE wise trends
# Plotting Age histogram
ggplot(df_demographic_model,aes(Age))+geom_histogram()
# Let's check the outlier in the variables 
quantile(df_demographic_model$Age,seq(0,1,0.01))
# Box plot 
boxplot(df_demographic_model$Age)
# Outlier treatment : Capping the lower values of age with 71.
df_demographic_model[(which(df_demographic_model$Age<27)),]$Age <- 27
df_merge[(which(df_merge$Age<27)),]$Age <- 27
#Checking box plot again 
boxplot(df_demographic_model$Age~df_demographic_model$Performance.Tag)
summary(df_demographic_model$Age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#27.00   37.00   45.00   45.04   53.00   65.00 
df_demographic_model$binning.age <- as.factor(cut(df_demographic_model$Age, breaks = c(26, 37, 48, 58, 66)))
plot_response(df_demographic_model$binning.age, "Age")
# We observed that same age trends for defaulter and non defaulter i.e 4%  

#2) Gender wise trends
summary(df_demographic_model$Gender)
#F     M 
#16479 53239 
plot_response(df_demographic_model$Gender, "Gender")
# We observed that "Male" & "Female" both the genders default rate is ~ 4%

#3) Marital Status (at the time of application) wise trends
summary(df_demographic_model$Marital.Status..at.the.time.of.application.)
#Married  Single 
#59423   10295  
plot_response(df_demographic_model$Marital.Status..at.the.time.of.application., "Marital_Status")
# We observed that "both the marital status sees approx same default rate ~ 4%

#4) No of dependents wise trends
summary(df_demographic_model$No.of.dependents)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00    2.00    3.00    2.86    4.00    5.00 
plot_response(df_demographic_model$No.of.dependents, "No.of.dependents")
# We observed no visible pattern for no of dependents variables have same dafault rate ~4%

#5) Income wise trends
# Plotting Income histogram
ggplot(df_demographic_model,aes(Income))+geom_histogram()
# Let's check the outlier in the variables 
quantile(df_demographic_model$Income,seq(0,1,0.01))
# Box plot 
boxplot(df_demographic_model$Income)
# Outlier Treatment - Capping the upper values of age with 71.
df_demographic_model[(which(df_demographic_model$Income<4.50)),]$Income <- 4.50
df_merge[(which(df_merge$Income<4.50)),]$Income <- 4.50
#Checking box plot again 
boxplot(df_demographic_model$Income~df_demographic_model$Performance.Tag)
summary(df_demographic_model$Income)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.50   14.00   27.00   27.21   40.00   60.00 
df_demographic_model$binning.income <- as.factor(cut(df_demographic_model$Income, breaks = c(3.50, 10, 20, 30, 40, 50, 61)))
plot_response(df_demographic_model$binning.income, "income")
# we observed that Aapplicants having income from 0-10k, 10-20k and 20-30k have higher default rate as compared to other bins.
 
#6) Education wise trends
summary(df_demographic_model$Education)
#Bachelor      Masters       Others          Phd Professional 
#17293        23472          118         4461        24374 
plot_response(df_demographic_model$Education, "Education")
# We observed that for all the education types default rate is approx. 4% , No evident pattern observed

#7) Profession wise trends
summary(df_demographic_model$Profession)
#SAL      SE     SE_PROF 
#39606   13899   16213 
plot_response(df_demographic_model$Profession, "Profession")
#We observed that for all the profession types default rate is approx. 4% , No evident pattern observed.

#8) Type of residence wise trends
summary(df_demographic_model$Type.of.residence)
# Company provided    Living with Parent  Others  Owned   Rented
#  1598                      1772            197     13971   52180
plot_response(df_demographic_model$Type.of.residence, "Type of Residence")
# We observed that for all the types of residence default rate is approx. 4% , No evident pattern observed. 

#9) No of months in current residence wise trends
summary(df_demographic_model$No.of.months.in.current.residence)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.00    6.00   11.00   34.56   60.00  126.00 
plot_response(df_demographic_model$No.of.months.in.current.residence, "Current residence")
boxplot(df_demographic_model$No.of.months.in.current.residence~df_demographic_model$Performance.Tag)
df_demographic_model$binning.cur_residence <- as.factor(cut(df_demographic_model$No.of.months.in.current.residence, breaks = c(5, 12, 36, 60, 84, 108, 127)))
plot_response(df_demographic_model$binning.cur_residence, "No.of.months.in.current.residence")
# We observed that No visible pattern observed for months in current residence variable as for all the bins default rate is approx. 4%.

#10) No of months in current company wise trends 
summary(df_demographic_model$No.of.months.in.current.company)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.00   16.00   34.00   33.96   51.00  133.00 
quantile(df_demographic_model$No.of.months.in.current.company,seq(0,1,0.01))
# Outlier treatment : Capping the lower values of age with 71.
df_demographic_model[(which(df_demographic_model$No.of.months.in.current.company>74)),]$No.of.months.in.current.company <- 74
df_merge[(which(df_merge$No.of.months.in.current.company>74)),]$No.of.months.in.current.company <- 74
boxplot(df_demographic_model$No.of.months.in.current.company~df_demographic_model$Performance.Tag)
df_demographic_model$binning.cur_company <- as.factor(cut(df_demographic_model$No.of.months.in.current.company, breaks = c(2, 16, 34, 33, 51, 133)))
plot_response(df_demographic_model$binning.cur_company,"No.of.months.in.current.company")
# We observed that No visible pattern observed for months in current company variable as for all the bins default rate is approx. 4%.

##################################################################################################################################
## Exploratory Data Analysis using Plots on Credit_bureau_data
# Functions used - quantile(),which(),cut(),boxplot(),ggplot()

## Manually binned all the continuous variables and checked for the difference between the lowest and highest default rates. 
## The variables having significant difference in the default rate across different bins can act as good predictor variables . 

plot_response_CB <- function(cat_var, var_name){
  a <- aggregate(`Performance.Tag`~cat_var,df_CreditBureau_model, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "Default_Rate","Total")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  ggplot(agg_response, aes(agg_response[, 1], count, label = Default_Rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
}

#1) No.of.times.90.DPD.or.worse.in.last.6.months wise trends
summary(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.6.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.2703  0.0000  3.0000 
quantile(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
# outlier treatment - Capping the outliers with 99 Percentile
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.6.months>2)),]$No.of.times.90.DPD.or.worse.in.last.6.months <- 2
df_merge[(which(df_merge$No.of.times.90.DPD.or.worse.in.last.6.months>2)),]$No.of.times.90.DPD.or.worse.in.last.6.months <- 2
boxplot(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.6.months)
plot_response_CB(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.6.months, "90dpd or in 6 month")
# we observed that value 1 and 2 times 90.DPD.last.6 see more default as compared to 0

#2) No.of.times.60.DPD.or.worse.in.last.6.months wise trends
summary(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.6.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.4305  1.0000  5.0000 
boxplot(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.6.months)
quantile(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
# outlier treatment - capping the outlier at 98 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.6.months>3)),]$No.of.times.60.DPD.or.worse.in.last.6.months <- 3
df_merge[(which(df_merge$No.of.times.60.DPD.or.worse.in.last.6.months>3)),]$No.of.times.60.DPD.or.worse.in.last.6.months <- 3
plot_response_CB(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.6.months, "60dpd in 6 month")
# we observed that value 2 & 3 times 60.DPD.last 6 see more default as compared to 0 & 1

#3) No.of.times.30.DPD.or.worse.in.last.6.months wise trends
summary(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.6.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.5772  1.0000  7.0000 
boxplot(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.6.months)
quantile(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.6.months,seq(0,1,0.01))
# outlier treatment - capping the outlier at 98 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.6.months>5)),]$No.of.times.30.DPD.or.worse.in.last.6.months <- 5
df_merge[(which(df_merge$No.of.times.30.DPD.or.worse.in.last.6.months>5)),]$No.of.times.30.DPD.or.worse.in.last.6.months <- 5
plot_response_CB(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.6.months, "30 dpd in 6 month")
# we observed that value 4 & 5 times 30.DPD. see more default as compared to others

#4) No.of.times.90.DPD.or.worse.in.last.12.months wise trends
summary(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.12.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.4503  1.0000  5.0000 
quantile(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.12.months,seq(0,1,0.01))
# outlier treatment - capping the outlier at 98 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.12.months>3)),]$No.of.times.90.DPD.or.worse.in.last.12.months <- 3
df_merge[(which(df_merge$No.of.times.90.DPD.or.worse.in.last.12.months>3)),]$No.of.times.90.DPD.or.worse.in.last.12.months <- 3
boxplot(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.12.months)
plot_response_CB(df_CreditBureau_model$No.of.times.90.DPD.or.worse.in.last.12.months, "90dpd in 12 months")
# we observed that value 2 & 3 times 90.DPD.last.12 see more default as compared to others

#5) No.of.times.60.DPD.or.worse.in.last.12.months wise trends
summary(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.12.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.6555  1.0000  7.0000 
boxplot(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.12.months)
quantile(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.12.months,seq(0,1,0.01))
# outlier treatment - capping the outlier at 98 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.12.months>5)),]$No.of.times.60.DPD.or.worse.in.last.12.months <- 5
df_merge[(which(df_merge$No.of.times.60.DPD.or.worse.in.last.12.months>5)),]$No.of.times.60.DPD.or.worse.in.last.12.months <- 5
plot_response_CB(df_CreditBureau_model$No.of.times.60.DPD.or.worse.in.last.12.months, "60dpd in 12 month")
# we observed that value 4 & 5 times 60.DPD.last.12 see more default as compared to others

#6) No.of.times.30.DPD.or.worse.in.last.12.months wise trends
summary(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.12.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.8009  1.0000  9.0000 
boxplot(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.12.months)
quantile(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.12.months,seq(0,1,0.01))
# outlier treatment - capping the outlier at 98 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.12.months>6)),]$No.of.times.30.DPD.or.worse.in.last.12.months <- 6
df_merge[(which(df_merge$No.of.times.30.DPD.or.worse.in.last.12.months>6)),]$No.of.times.30.DPD.or.worse.in.last.12.months <- 6
plot_response_CB(df_CreditBureau_model$No.of.times.30.DPD.or.worse.in.last.12.months, "30dpd in 12 month")
# we observed that value 4,5 & 6 times 30.DPD.last.12 see more default as compared to others

#7) No.of.trades.opened.in.last.6.months wise trends
summary(df_CreditBureau_model$No.of.trades.opened.in.last.6.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#0.000   1.000   2.000   2.298   3.000  12.000       
boxplot(df_CreditBureau_model$No.of.trades.opened.in.last.6.months)
quantile(df_CreditBureau_model$No.of.trades.opened.in.last.6.months,seq(0,1,0.01),na.rm = TRUE)
# outlier treatment - capping the outlier at 99 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.trades.opened.in.last.6.months>9)),]$No.of.trades.opened.in.last.6.months <- 9
df_merge[(which(df_merge$No.of.trades.opened.in.last.6.months>9)),]$No.of.trades.opened.in.last.6.months <- 9
plot_response_CB(df_CreditBureau_model$No.of.trades.opened.in.last.6.months, "Trades opened in 6 months")
# we observed that value 2.5 to 5 times No.of.trades.last.6 see more default as compared to others

#8) No.of.trades.opened.in.last.12.months wise trends
summary(df_CreditBureau_model$No.of.trades.opened.in.last.12.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   2.000   5.000   5.827   9.000  28.000 
boxplot(df_CreditBureau_model$No.of.trades.opened.in.last.12.months)
quantile(df_CreditBureau_model$No.of.trades.opened.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)
# outlier treatment - capping the outlier at 99 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.trades.opened.in.last.12.months>21)),]$No.of.trades.opened.in.last.12.months <- 21
df_merge[(which(df_merge$No.of.trades.opened.in.last.12.months>21)),]$No.of.trades.opened.in.last.12.months <- 21
plot_response_CB(df_CreditBureau_model$No.of.trades.opened.in.last.12.months, "Trades opened in 12 months")
# we observed that value 5 to 10 times No.of.trades.last.12 see more default as compared to others

#9) No.of.PL.trades.opened.in.last.6.months wise trends
summary(df_CreditBureau_model$No.of.PL.trades.opened.in.last.6.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   1.207   2.000   6.000 
boxplot(df_CreditBureau_model$No.of.PL.trades.opened.in.last.6.months)
quantile(df_CreditBureau_model$No.of.PL.trades.opened.in.last.6.months,seq(0,1,0.01),na.rm = TRUE)
# outlier treatment - capping the outlier at 98 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.PL.trades.opened.in.last.6.months>5)),]$No.of.PL.trades.opened.in.last.6.months <- 5
df_merge[(which(df_merge$No.of.PL.trades.opened.in.last.6.months>5)),]$No.of.PL.trades.opened.in.last.6.months <- 5
plot_response_CB(df_CreditBureau_model$No.of.PL.trades.opened.in.last.6.months, "PL in 6 months")
# we observed that value 2 to 14 times No.of.PL.trades.last.6 see more default as compared to others

#10) No.of.PL.trades.opened.in.last.12.months wise trends
summary(df_CreditBureau_model$No.of.PL.trades.opened.in.last.12.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   2.000   2.397   4.000  12.000
boxplot(df_CreditBureau_model$No.of.PL.trades.opened.in.last.12.months)
quantile(df_CreditBureau_model$No.of.PL.trades.opened.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)
# outlier treatment - capping the outlier at 99 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.PL.trades.opened.in.last.12.months>9)),]$No.of.PL.trades.opened.in.last.12.months <- 9
df_merge[(which(df_merge$No.of.PL.trades.opened.in.last.12.months>9)),]$No.of.PL.trades.opened.in.last.12.months <- 9
plot_response_CB(df_CreditBureau_model$No.of.PL.trades.opened.in.last.12.months, "PL in 12 months")
# we observed that value 2.5 to 5 times No.of.PL.trades.last.12 see more default as compared to others

#11) No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. wise trends
summary(df_CreditBureau_model$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
# Min. 1st Qu.  Median    Mean   3rd Qu. Max. 
#0.000   0.000   1.000   1.764   3.000  10.000 
boxplot(df_CreditBureau_model$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
quantile(df_CreditBureau_model$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,seq(0,1,0.01),na.rm = TRUE)
# outlier treatment - capping the outlier at 99 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>8)),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 8
df_merge[(which(df_merge$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>8)),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 8
plot_response_CB(df_CreditBureau_model$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., "No of enquiries in Last 6 months")
# we observed that value 3 times No.of.Inquiries.in.last.6. see more default as compared to others

#12) No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. wise trends
summary(df_CreditBureau_model$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   3.000   3.535   5.000  20.000
boxplot(df_CreditBureau_model$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
quantile(df_CreditBureau_model$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,seq(0,1,0.01),na.rm = TRUE)
# outlier treatment - capping the outlier at 99 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>15)),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 15
df_merge[(which(df_merge$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>15)),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 15
plot_response_CB(df_CreditBureau_model$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., "No of enquiries in last 12 months")
# we observed that value 5 to 7 times No.of.Inquiries.in.last.12. see more default as compared to others

#13) Presence.of.open.home.loan wise trends
summary(df_CreditBureau_model$Presence.of.open.home.loan)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0000  0.0000  0.0000  0.2564  1.0000  1.0000     272 
boxplot(df_CreditBureau_model$Presence.of.open.home.loan)
quantile(df_CreditBureau_model$Presence.of.open.home.loan,seq(0,1,0.01))
plot_response_CB(df_CreditBureau_model$Presence.of.open.home.loan, "Presence of open home loan")
# we observed that no home loan see more default as compared to others

#14) Total.No.of.Trades wise trends
summary(df_CreditBureau_model$Total.No.of.Trades)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   3.000   6.000   8.187  10.000  44.000 
boxplot(df_CreditBureau_model$Total.No.of.Trades)
quantile(df_CreditBureau_model$Total.No.of.Trades,seq(0,1,0.01),na.rm = TRUE)
# outlier treatment - capping the outlier at 99 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$Total.No.of.Trades>31)),]$Total.No.of.Trades <- 31
df_merge[(which(df_merge$Total.No.of.Trades>31)),]$Total.No.of.Trades <- 31
plot_response_CB(df_CreditBureau_model$Total.No.of.Trades, "Total No. Of Trades")
# we observed that value 8 to 10 times Total.No.of.Trades wise trends. see more default as compared to others

#15) Presence.of.open.auto.loan wise trends
summary(df_CreditBureau_model$Presence.of.open.auto.loan)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.00000 0.08462 0.00000 1.00000 
boxplot(df_CreditBureau_model$Presence.of.open.auto.loan)
quantile(df_CreditBureau_model$Presence.of.open.auto.loan,seq(0,1,0.01))
plot_response_CB(df_CreditBureau_model$Presence.of.open.auto.loan, "presence of open auto loan")
# we observed no visible pattern for variable Presence.of.open.auto.loan

#16) Avgas.CC.Utilization.in.last.12.months wise trends
summary(df_CreditBureau_model$Avgas.CC.Utilization.in.last.12.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#0.0     8.0    15.0    29.7    46.0   113.0   
boxplot(df_CreditBureau_model$Avgas.CC.Utilization.in.last.12.months)
quantile(df_CreditBureau_model$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01),na.rm = TRUE)
df_CreditBureau_model$binning.ccutils <- as.factor(cut(df_CreditBureau_model$Avgas.CC.Utilization.in.last.12.months, breaks = c(-1, 8.0,15.0,29.7,46.0,113.0)))
plot_response_CB(df_CreditBureau_model$binning.ccutils, "Avgas.CC.Utilization.in.last.12.months")
# we observed that bin 30 to 113 see more default as compared to others

#17) Outstanding Balance wise trends
summary(df_CreditBureau_model$Outstanding.Balance)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#0  211537  774994 1249195 2920797 5218801      
boxplot(df_CreditBureau_model$Outstanding.Balance)
quantile(df_CreditBureau_model$Outstanding.Balance,seq(0,1,0.01))
# outlier treatment - capping the outlier at 99 Percentile 
df_CreditBureau_model[(which(df_CreditBureau_model$Outstanding.Balance>4249692.39)),]$Outstanding.Balance <- 4249692.39
df_merge[(which(df_merge$Outstanding.Balance>4249692.39)),]$Outstanding.Balance <- 4249692.39
df_CreditBureau_model$binning.outbal <- as.factor(cut(df_CreditBureau_model$Outstanding.Balance, breaks = c(-1, 211537,774994,1249195,2920797,5218801)))
plot_response_CB(df_CreditBureau_model$binning.outbal, "Outstanding Balance")
# we observed that almost same default rate across all the bins.

##Distribution of Avg Credit Card Utilization
ggplot(df_CreditBureau_model,
       aes(x = df_CreditBureau_model$Avgas.CC.Utilization.in.last.12.months)) +
  geom_density(na.rm = TRUE, col = "red", fill = "grey") +
  ggtitle("Distribution of Avg. Credit Card Utilization") + xlab("Cc Utilization") + ylab("Density")
# we observed that Most of the customers have max average utilization between 0-20

##Distribution of Outstanding Balance
ggplot(df_CreditBureau_model,
       aes(x = df_CreditBureau_model$Outstanding.Balance)) +
  geom_density(na.rm = TRUE, col = "red", fill = "grey") +
  ggtitle("Distribution of Outstanding Balance ") + xlab("Balance") + ylab("Density")
# we observed that Distribution of outstanding balance is right skewed and most of the customers have fall under 100k 
##############################################################################################################################
## NA treatment for Master merged dataframe using IV and WOE

sum(is.na(df_merge))
#Created data frame named "df_no_cc" which having all the rejected populations i.e. tag = "NAs" 
df_No_CC <- df_merge[is.na(df_merge$Performance.Tag),]
df_merge <- df_merge[!is.na(df_merge$Performance.Tag),]
# df_No_CC has 1425 observation of 28 variables
# df_merge has 69867 observation of 28 variables

# Missing value imputation in Rejected Population
sapply(df_No_CC, function(x) sum(is.na(x)))
sapply(df_No_CC_Demo, function(x) sum(is.na(x)))
# AvgCC - 35 ,Profession - 1 and education - 1
# Replaced NA's values in Avg CC util with Median , Education with "Masters" and Profession as "SAL"
summary(df_No_CC$Avgas.CC.Utilization.in.last.12.months)
# Min.   1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
# 1.00   35.00    51.00    51.08   67.00    101.00      35 
df_No_CC[which(is.na(df_No_CC$Avgas.CC.Utilization.in.last.12.months)),]$Avgas.CC.Utilization.in.last.12.months <-median(df_No_CC$Avgas.CC.Utilization.in.last.12.months)
summary(df_No_CC$Education)
# Bachelor      Masters       Others    Phd    Professional         NA's 
# 395          489            2         85          453            1 
df_No_CC[which(is.na(df_No_CC$Education)),]$Education <- "Masters"
df_No_CC_Demo[which(is.na(df_No_CC_Demo$Education)),]$Education <- "Masters"
df_No_CC[which(is.na(df_No_CC$Profession)),]$Profession <- "SAL"
df_No_CC_Demo[which(is.na(df_No_CC_Demo$Profession)),]$Profession <- "SAL"

# WOE and Information Value Analysis 
# function used : create_infotables(),DF.Replace.WOE() ,Plot_infotables()
# Since Information package treats 1 as 'Good' but performance tag of given datafiles have 1 as Bad, 
# we will read graph in this way like more negative woe values have less chance to default and positive values see more defaulting
infoTables <- create_infotables(data = df_merge ,
                                y = "Performance.Tag",
                                bins = 10,
                                parallel = T)
# dataframe IV created with variable name and IV values
IV <- infoTables$Summary
knitr::kable(head(infoTables$Summary,12))
infoTables$Summary$Variable
# From the IV values we can conclude that variables from demographic dataset didn't play much significant role in prediction. 
# And Top 16 significant variables are from Credit Bureau dataset 
# IV values ranging from 0.1-0.3 and 0.3-0.5 have Medium and high predictive power respectively
#[1] "Avgas.CC.Utilization.in.last.12.months"                         
#[2] "No.of.trades.opened.in.last.12.months"                          
#[3] "No.of.PL.trades.opened.in.last.12.months"                       
#[4] "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
#[5] "Outstanding.Balance"                                            
#[6] "No.of.times.30.DPD.or.worse.in.last.6.months"                   
#[7] "Total.No.of.Trades"                                             
#[8] "No.of.PL.trades.opened.in.last.6.months"                        
#[9] "No.of.times.90.DPD.or.worse.in.last.12.months"                  
#[10] "No.of.times.60.DPD.or.worse.in.last.6.months"                   
#[11] "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." 
#[12] "No.of.times.30.DPD.or.worse.in.last.12.months"                  
#[13] "No.of.trades.opened.in.last.6.months"                           
#[14] "No.of.times.60.DPD.or.worse.in.last.12.months"                  
#[15] "No.of.times.90.DPD.or.worse.in.last.6.months"                   
#[16] "No.of.months.in.current.residence"                              
#[17] "Income"                                                         
#[18] "No.of.months.in.current.company"      

# Exploratory Data Analysis of all 18 significant WOE variables using plots 

names <- infoTables$Summary$Variable
plots <- list()
for (i in 1:length(names)){
  plots[[i]] <- plot_infotables(infoTables, names[i],same_scales = TRUE)
}

plots[1:18]
# We observed that 
# 1) applicants whose Avgas.CC.Utilization.in.last.12.months is between 0 to 14 has less chances of default.
# 2) applicants whose No.of.trades.opened.in.last.12.months is between 0 to 2 has the less chances of default.
# 3) applicants whose No.of.PL.trades.opened.in.last.12.months is between 0 to 1 has the less chances of default
# 4) applicants whose No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. is between 0 to 1 has the less chances of default
# 5) applicants whose Outstanding.Balance is betwwn 0-386813 & 1357399-3282314 have the less chances of default 
# 6) applicants whose No.of.times.30.DPD.or.worse.in.last.6.months is 0 has the less chances of default 
# 7) applicants whose Total.No.of.Trades is between 0-4 has the less chances of default 
# 8) applicants whose No.of.PL.trades.opened.in.last.6.months is 0 has the less chances of default 
# 9) applicants whose No.of.times.90.DPD.or.worse.in.last.12.months.is 0 has the less chances of default 
# 10) applicants whose No.of.times.60.DPD.or.worse.in.last.6.months is 0 has the less chances of default 
# 11) applicants whose No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. is 0 has the less chances of default 
# 12) applicants whose No.of.times.30.DPD.or.worse.in.last.12.months is 0 has the less chances of default 
# 13) applicants whose No.of.trades.opened.in.last.6.months is between 0-1 has the less chances of default 
# 14) applicants whose No.of.times.60.DPD.or.worse.in.last.12.months is 0 has the less chances of default 
# 15) applicants whose No.of.times.90.DPD.or.worse.in.last.6.months is 0 has the less chances of default 
# 16) applicants whose No.of.months.in.current.residence is between 6-9 months & between 98-126 months has the less chances of default 
# 17) applicants whose income is betwwn 32-60 have the less chances of default 
# 18) applicant whose No.of.months.in.current.company is between 27-33 & 41-61 months have the less chances of default 

# Function to replace DF values to WOE values
DF.Replace.WOE <-
  function(X,
           y,
           Dependent = NULL) {
    z <- 0
    cz <- 0
    D <- X[, Dependent]
    x <- X[,-which(names(X) == Dependent)]
    cn <- names(x)
    if (class(y) == "Information") {
      for (i in 1:ncol(x)) {
        if (class(x[, i]) == "factor") {
          for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
            x[, i] <- as.character(x[, i])
            if (is.na(y[[1]][i][[1]][1][[1]][j])) {
              x[which(is.na(x[, i])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][4][[1]][which(is.na(y[[1]][i][[1]][1][[1]]))]
            }
            else {
              x[which(x[, i] == y[[1]][i][[1]][1][[1]][j]), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][4][[1]][j]
            }
          }
        }
        else {
          for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
            cz <-
              as.vector(strsplit(gsub(
                "[]]", "", gsub("[[]", "", y[[1]][i][[1]][1][[1]])
              ), ","))
            if (y[[1]][i][[1]][1][[1]][j] == "NA") {
              x[which(is.na(x[, i])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][4][[1]][which(y[[1]][i][[1]][1][[1]][j] == "NA")]
            }
            else {
              x[which(x[, i] >= as.double(cz[[j]][1]) &
                        x[, i] <= as.double(cz[[j]][2])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][4][[1]][j]
            }
          }
        }
      }
    }
    z <- cbind(x, D)
    colnames(z)[which(names(z) == "D")] <- Dependent
    z <- z[, -which(names(x) == cn)]
    return(z)
  }

df_woe <- DF.Replace.WOE(df_merge,infoTables,Dependent = "Performance.Tag")
df_No_CC_woe <- DF.Replace.WOE(df_No_CC,infoTables,Dependent = "Performance.Tag")

## Correlation plot 
df_corr <- df_merge[rowSums(is.na(df_merge)) <= 0.01,]
sapply(df_corr, function(x) sum(is.na(x)))
corr_data <- df_corr[,-c(14,17,18,19,20,21,23,24,25,26,27,28)]
numerics_data <- sapply(corr_data,is.numeric)
data_correlation <- cor(corr_data[,numerics_data])

corrplot(data_correlation, type = "full",tl.pos = "dt",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE)
# we observed that co-relation between most significant predictive numeric variables like No. of inquiries in last 6 months is highly correlated with No. of inquiries in last 12 months ;
# and outstanding balance and Avg.CC util have very weak correlation with all

##############################################################################################################################
## Model building for Demographic data 

# Removed application id and other binning varaibles
df_demographic_model <- df_demographic_model[,-c(1,13,14,15,16)]
# scaling numeric variables & in order to include categorical variables in regression model 
# the variable needs to converted into numeric variables by the means of "Dummy Variables".
df_demographic_model$Age <- scale(df_demographic_model$Age)
df_demographic_model$No.of.dependents <- scale(df_demographic_model$No.of.dependents)
df_demographic_model$Income <- scale(df_demographic_model$Income)
df_demographic_model$No.of.months.in.current.residence <- scale(df_demographic_model$No.of.months.in.current.residence)
df_demographic_model$No.of.months.in.current.company <- scale(df_demographic_model$No.of.months.in.current.company)
df_demographic_model <- dummy.data.frame(df_demographic_model)
# finally we have 69718 observation with 23 variables after dummy variables creation 
df_demographic_model$Performance.Tag <- as.factor(ifelse(df_demographic_model$Performance.Tag==1,"yes","no"))

################################################################################################################
## splitting the data between train and test
# set.seed () function is used for reproducible random number results

set.seed(100)
split_indices <- sample.split(df_demographic_model$Performance.Tag, SplitRatio = 0.70)

traindata = df_demographic_model[split_indices,] 
testdata = df_demographic_model[!(split_indices),]
nrow(traindata)/nrow(df_demographic_model) #0.70
nrow(testdata)/nrow(df_demographic_model) #0.30
summary(traindata$Performance.Tag)
# We have 48802 observation of 23 variables in train dataset
# And, 20916 observations of 23 variables in test dataset
# As per below summary the train data is highly unbalanced,so we used SMOTE() funtion to balance the data
# no     yes 
# 46743  2059 

traindata_SMOTE <- SMOTE(Performance.Tag ~ ., traindata , perc.over = 100 , perc.under = 200)
summary(traindata_SMOTE$Performance.Tag)
summary(testdata$Performance.Tag)
#Traindata_smote    Testdata
# no  yes           no      yes 
# 4118 4118         20033   883

# Now, to make all the variables name legal used names() function 
names(traindata_SMOTE) <- make.names(names(traindata_SMOTE)) 
names(testdata) <- make.names(names(testdata))

## Logistic Regression: Model Building on demographic data 
#-------------------------------------------------------------------------------------------------------------------------------
# Initial model containing all variables
# Model_1 - AIC 11297 nullDev 11418 resDev 11261

logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = traindata_SMOTE)
summary(logistic_1)

# Task 2.1) Perform variable selection using the usual methods
#**********************************************************************************************************************************
# Stepwise selection
## In stepAIC function, we pass our first model i.e logistic_1 and direction is set as both, because in stepwise,  
## both the forward selection of variables and backward elimination of variables happen simultaneously 
# stepAIC makes multiple calls while checking which variables to keep
# the last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 

# Commented out the StepAIC function as it takes time to complete 
# and store the output of stepwise method into an object called logistic_2

#logistic_2 <- stepAIC(logistic_1, direction = "both") #commented
#summary(logistic_2)
#Model_2 -  AIC 11283 nullDev 11418 resDev 11267
logistic_2 <- glm(formula = Performance.Tag ~ Income + EducationOthers + Type.of.residenceLiving.with.Parents + 
                    Type.of.residenceOthers + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationProfessional, 
                  family = "binomial", data = traindata_SMOTE)

# Removing multicollinearity through VIF check
summary(logistic_2)
sort(vif(logistic_2))

# Steps to be followed while building models are 
## I) Check for high value of VIFs sort() use to get the data in order
## II) check the p values of variable, if the value is high (>0.05) then remove the variable and 
## III) if the high vif variable has very low p value then pick the second highest vif and perform the same steps
## IV) checkpoint is after removing variable check the AIC value that should not dropped drastically. 
## As we can see the highest VIFs have their p values very significants so these variables are statiscally significant and we can't remove.
## we cannot exclude any variables based on vif all of them have low vif; those with higher vif are very significant and not correlated

#Model_3 -  AIC 11293 nullDev 11418 resDev 11281 Removed - Type.of.residenceLiving with Parents
logistic_3  <- glm(formula = Performance.Tag ~ No.of.dependents + Income + ProfessionSAL + 
                     No.of.months.in.current.residence + 
                     No.of.months.in.current.company, family = "binomial", data = traindata_SMOTE)
summary(logistic_3)

#Model_4 -  AIC 11292 nullDev 11418 resDev 11282 Removed - No.of.dependents
logistic_4 <- glm(formula = Performance.Tag ~  Income + ProfessionSAL + 
                    No.of.months.in.current.residence + No.of.months.in.current.company, 
                  family = "binomial", data = traindata_SMOTE)
summary(logistic_4)

# AIC 11292 nullDev 11418 resDev 11284 Removed - No.of.months.in.current.residence
logistic_5 <- glm(formula = Performance.Tag ~ Income + ProfessionSAL +  
                    No.of.months.in.current.company, family = "binomial", data = traindata_SMOTE)
summary(logistic_5)

# AIC 11290 nullDev 11418 resDev 11284 Removed  ProfessionSAL
logistic_6 <-glm(formula = Performance.Tag ~ Income  + No.of.months.in.current.company, 
                 family = "binomial", data = traindata_SMOTE)
summary(logistic_6)

#Coefficients:
#                             Estimate Std.   Error    z value  Pr(>|z|)    
# (Intercept)                     -0.02425    0.02232  -1.087     0.246    
# Income                          -0.21247    0.02278  -9.327   < 2e-16    ***
# No.of.months.in.current.company -0.09567    0.02251  -4.250 0.0000000309 ***

# Null deviance: 11418  on 8235  degrees of freedom
# Residual deviance: 11284  on 8233  degrees of freedom
# AIC: 11290

final_model<- logistic_6

############################################################################################################
## 6) Logistic Regression: Model Evaluation
# In a model evaluation process, we use our model that has been prepared with the help of training data, 
# to make predictions for the testing data. We used below methods to evaluate the model
# i) Accuracy-Sensitivity-Specificity ii) KS Statistic iii) Gain-Lift  iv) ROC curve

## A) Acuracy-Sensitivity-Specificity
# predict() function is used to predict probabilities of attrition for test data
# Input to predict function - our final_model(model_number 6) and type as response to get the output in terms of probability
# We need to exclude Performance-tag column as it is the dependent variables

test_pred = predict(final_model, type = "response", newdata = testdata[-23])

# Let's see the Min and Max values using summary function() which give us the prediction range on testdata from .06% TO 80% 
summary(test_pred)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3178  0.4464  0.4953  0.4940  0.5430  0.6275 
# Here we have probabilities for each data points(application Ids) so now we can make the decision whether the Applicants will turnover or not based on some cutoff value 
# To calculate the cutoff value we use table/ConfusionMatrix which measure the performance of a classification model

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_attrition, testdata$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from prediction range (from min.06% to max 80%) for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100) {
  OUT[i,] = perform_fn(s[i])
} 

## Plotting the Sensitivity,Specificity and Accuracy of the model to identify the final cut off value. 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

## Getting the cutoff value. 
cutoff_logi_demo <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]
cutoff_logi_demo  #0.505
# So based on the above plot we choose a cutoff value of 0.505 for final model
# since we are focusing on applicants who will default and that is capture in sensitivity so 
# when the cutoff is increased,its sensitivity decrease and specificity increase so we choose 0.505 as cutoff 
test_cutoff_attrition_logi_demo <- factor(ifelse(test_pred >=0.505, "yes", "no"))
# Here the confusion matrix gives us 3 measures which shows how well our model dscriminates between two classes
#Sensitivity is a ratio which gives true positive rate which came 0.5458664 for final model
#Specificity is a ratio which gives true negative rate which came 0.5544352 for final model 
#Accuracy - is a ratio which gives sum of true positive and true negative divided by all observation i.e. 0.5530734
conf_final <- confusionMatrix(test_cutoff_attrition_logi_demo,testdata$Performance.Tag, positive = "yes")
acc <- conf_final$overall[1]  #0.5530734
sens <- conf_final$byClass[1] #0.5458664
spec <- conf_final$byClass[2] #0.554452

## B) KS -statistic - 
## Assigning numeric values 1 and 0 for Yes and No values of predicted and actual attrition 
test_cutoff_attrition_logi1_demo <- ifelse(test_cutoff_attrition_logi_demo=="yes",1,0)
test_actual_attrition_logi_demo <- ifelse(testdata$Performance.Tag=="yes",1,0)
# prediction() function is used to create prediction objects based on the actual and predicted cutoff attrition  
# Performance() function is used to to perofrm all kinds of predictor evaluations.
# here we use parameters tpr (true positive rate) & fpr (false positive rate)

pred_object_test_logi_demo<- prediction(test_cutoff_attrition_logi1_demo, test_actual_attrition_logi_demo)
performance_measures_test_logi_demo<- ROCR::performance(pred_object_test_logi_demo, "tpr", "fpr")

#plot sensitivity/specificity curve (x-axis:specificity,y-axis:sensitivity)
plot(performance_measures_test_logi_demo)

# Calculating KS Statistic value 
ks_table_test_logi_demo <- attr(performance_measures_test_logi_demo, "y.values")[[1]] - 
  (attr(performance_measures_test_logi_demo, "x.values")[[1]])
max(ks_table_test_logi_demo) 
##KS statistic comes 10.03# 

#ks chart
deciles<-ROCR::performance(pred_object_test_logi_demo,'rpp')
k_stat_matrix<-data.frame(10*(deciles@y.values[[1]]),(performance_measures_test_logi_demo@y.values[[1]]-performance_measures_test_logi_demo@x.values[[1]]))
colnames(k_stat_matrix)[1]<-"deciles"
colnames(k_stat_matrix)[2]<-"k_statistic"
k_stat_matrix$k_statistic<-round(k_stat_matrix$k_statistic,2)
plot(k_stat_matrix)
abline(h=2.7,v=5)
#ks statistic lies withinin first 5 deciles 

## C) Lift & Gain Chart 
#The gain statistic is a clear indicator of the advantage offered by the model
#The Lift tells us the factor by which model is outperforming a random model, i.e. a model-less situation.

lift <- function(labels , predicted_prob,groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

# as observed that table shows when we using our best fit model vs anyother model then 
# at the 4th decile we'll be able to identified ~50% of the applicants who can default.
Attrition_decile_logi_demo = lift(test_actual_attrition_logi_demo, test_pred, groups = 10)
Attrition_decile_logi_demo

#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2092       125     125  14.2    1.42
#2      2  2092       100     225  25.5    1.27
#3      3  2091       115     340  38.5    1.28
#4      4  2092        90     430  48.7    1.22
#5      5  2091        98     528  59.8    1.20
#....
#10     10  2091        65     883 100      1    

# graph for Gain and lift
graphics::plot(Attrition_decile_logi_demo$bucket, Attrition_decile_logi_demo$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")
gain <- ROCR::performance(pred_object_test_logi_demo, "tpr", "rpp")
plot(gain)
#within first 4 deciles as per the model we are able to predict 50% of defaulters correctly

## D) Area under the curve:
plot(AUC::roc(test_pred,testdata$Performance.Tag))
auc(roc(test_pred,testdata$Performance.Tag))
#area under ROC CURVE is 56.40% 
##############################################################################################################################
## Decision Tree: Model Building on demographic data 

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister()

########################## Hyperparameter tuning and cross validation ##########################################

# set the number of folds in cross test to 5
tree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
tree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))

# train the model 
tree.model <- caret::train(Performance.Tag ~.,
                    data = traindata_SMOTE,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = tree.control,
                    tuneGrid = tree.grid,
                    control = rpart.control(minsplit = 50,minbucket = 20))
# Checked at cross validated model results , best value of hyperparameter and make predictions on test set
tree.model
tree.model$bestTune
tree.predict <- predict.train(tree.model, testdata)
confusionMatrix(tree.predict, testdata$Performance.Tag)  
#   cp
#2 0.0025
#Accuracy : 0.6751
#Sensitivity : 0.68582
#Specificity : 0.43262
# plot CP vs Accuracy
accuracy_graph <- data.frame(tree.model$results)
ggplot(data = accuracy_graph, aes(x = cp, y = Accuracy*100)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")

##############################################################################################################################
## Random Forest : Model Building on demographic data 

demographic_rf <- randomForest(Performance.Tag ~., data = traindata_SMOTE, proximity = F, do.trace = T, mtry = 5)
rf_pred_demo_logi <- predict(demographic_rf, testdata[, -23], type = "prob")

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_demo_logi[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, testdata$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)
OUT_rf = matrix(0,100,3)
for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf(s[i])
} 
plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_demo_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.018)]
cutoff_demo_rf # 0.396
predicted_response_demo_rf <- factor(ifelse(rf_pred_demo_logi[, 2] >= 0.396, "yes", "no"))
conf_forest <- confusionMatrix(predicted_response_demo_rf, testdata[, 23], positive = "yes")
conf_forest
# Sensitivity :0.55685
# Specificity :0.53447
# Accuracy    :0.563

## B) KS -statistic - 
## Assigning numeric values 1 and 0 for Yes and No values of predicted and actual attrition 
test_cutoff_attrition_demorf <- ifelse(predicted_response_demo_rf=="yes",1,0)
test_actual_attrition_demorf <- ifelse(testdata$Performance.Tag=="yes",1,0)
# prediction() function is used to create prediction objects based on the actual and predicted cutoff attrition  
# Performance() function is used to to perofrm all kinds of predictor evaluations.
# here we use parameters tpr (true positive rate) & fpr (false positive rate)

pred_object_test_demorf <- prediction(test_cutoff_attrition_demorf, test_actual_attrition_demorf)
performance_measures_test_demorf <- ROCR::performance(pred_object_test_demorf, "tpr", "fpr")

#plot sensitivity/specificity curve (x-axis:specificity,y-axis:sensitivity)
plot(performance_measures_test_demorf)

# Calculating KS Statistic value 
ks_table_test_demorf <- attr(performance_measures_test_demorf, "y.values")[[1]] - 
  (attr(performance_measures_test_demorf, "x.values")[[1]])
max(ks_table_test_demorf) 
##KS statistic comes 9.77# 

#ks chart
deciles<-ROCR::performance(pred_object_test_demorf,'rpp')
k_stat_matrix<-data.frame(10*(deciles@y.values[[1]]),(performance_measures_test_demorf@y.values[[1]]-performance_measures_test_demorf@x.values[[1]]))
colnames(k_stat_matrix)[1]<-"deciles"
colnames(k_stat_matrix)[2]<-"k_statistic"
k_stat_matrix$k_statistic<-round(k_stat_matrix$k_statistic,2)
plot(k_stat_matrix)
abline(h=2.7,v=5)
#ks statistic lies withinin first 5 deciles 

# C) Lift & Gain Chart 
#The gain statistic is a clear indicator of the advantage offered by the model
#The Lift tells us the factor by whichmodel is outperforming a random model, i.e. a model-less situation.

Attrition_decile_demorf = lift(test_actual_attrition_demorf, test_cutoff_attrition_demorf, groups = 10)
Attrition_decile_demorf
# as observed that table shows when using our best fit model vs random model then 
# at the 5th decile we'll be able to identified ~55% of the applicants who can default.

#bucket total totalresp Cumresp   Gain Cumlift
#<int> <int>     <dbl>   <dbl>  <dbl>   <dbl>
#  1      1  2092        28      28   3.17   0.317
#2      2  2092        84     112  12.7    0.634
#3      3  2091       110     222  25.1    0.838
#4      4  2092       134     356  40.3    1.01 
#5      5  2091       137     493  55.8    1.12 
#..
#10     10  2091       183     883 100      1 

# graph for Gain and lift
graphics::plot(Attrition_decile_demorf$bucket, Attrition_decile_demorf$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")
gain <- ROCR::performance(pred_object_test_demorf, "tpr", "rpp")
plot(gain)

# D) Area under the curve:
plot(AUC::roc(test_cutoff_attrition_demorf,testdata$Performance.Tag))
auc(roc(test_cutoff_attrition_demorf,testdata$Performance.Tag))
#area under ROC CURVE is 54.88% 

importance_demo <- demographic_rf$importance 
importance_demo_rf <- data.frame(importance_demo)
varImpPlot(demographic_rf)

##############################################################################################################################
## SVM Linear : Model Building on demographic data 

Model1_svm <- ksvm(Performance.Tag~ ., data = traindata_SMOTE, scale = FALSE, kernel = "vanilladot")

print(Model1_svm)
Eval1_linear<- predict(Model1_svm, testdata[-23])
confusionMatrix(Eval1_linear,testdata$Performance.Tag)

#Accuracy : 0.5011          
#Sensitivity : 0.49663         
#Specificity : 0.60136         

#Since the accuracy is too low with linear we move to Radial kernal

##############################################################################################################################
## SVM Radial : Model Building on demographic data 

Model1_RBF <- ksvm(Performance.Tag~ ., data = traindata_SMOTE, scale = FALSE, kernel = "rbfdot")
print(Model1_RBF)
Eval1_RBF<- predict(Model1_RBF, testdata[-23])
confusionMatrix( Eval1_RBF,testdata$Performance.Tag)
#Accuracy : 0.5224          
#Sensitivity : 0.51132         
#Specificity : 0.52434         

########################## Hyperparameter tuning and cross validation ##########################################

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister()

trainControl_demo <- trainControl(method="cv", number=5)
grid_demo <- expand.grid( C=c(1,2,3),sigma=c(0.01,0.02))
## Commenting out htis piece of code since hypertuning takes time . 
##fit.svm_rbf <- caret::train(Performance.Tag~., data=traindata_SMOTE, method="svmRadial", metric="Accuracy", 
#                     tuneGrid=grid_demo, trControl=trainControl_demo,preProcess = NULL)
#print(fit.svm_rbf)
#plot(fit.svm_rbf)

#C  sigma  Accuracy   Kappa    
#1  0.01   0.5711496  0.1422971
#1  0.02   0.5840203  0.1680369
#2  0.01   0.5802565  0.1605087
#2  0.02   0.5853561  0.1707071
#3  0.01   0.5832919  0.1665791
#3  0.02   0.5868114  0.1736154

# Re-run the model at C=3 and sigma = 0.02
Model2_RBF <- ksvm(Performance.Tag~ ., data = traindata_SMOTE, scale = FALSE, kernel = "rbfdot",C=3, 
                   kpar = list(sigma = 0.02))
print(Model2_RBF)
Eval2_RBF<- predict(Model2_RBF, testdata[-23])
confusionMatrix( Eval2_RBF,testdata$Performance.Tag)

#Accuracy : 0.5424          
#Sensitivity : 0.55132         
#Specificity : 0.53434         

##############################################################################################################################
## XGBOOST : Model Building on demographic data

labels <- as.numeric(traindata_SMOTE$Performance.Tag)-1
ts_label <- as.numeric(testdata$Performance.Tag)-1

train_XGb_demo = as.matrix(sapply(traindata_SMOTE[,-23],as.numeric))
test_XGb_demo = as.matrix(sapply(testdata[,-23],as.numeric))

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric ="auc",
  eta=0.7,
  gamma=5,
  max_depth=3,
  min_child_weight=2,
  subsample=1,
  colsample_bytree= 0.933
)

########################## Hyperparameter tuning and cross validation ##########################################

bst.cv = xgb.cv(
  params=params,
  data = train_XGb_demo[,-23],
  label = labels, #train[,23],
  nfold = 5,
  nrounds=300,
  prediction=T)
#highest test AUC was obtained was 0.778 after 118 rounds of 5 fold cross validation

watchlist <- list(train_XGb_demo, test_XGb_demo)
xgb.model <- xgboost::xgboost(params=params, data=train_XGb_demo[,-23],
                     label=labels,
                     nrounds = 118, watchlist)

xgb.predictions <- predict(xgb.model, test_XGb_demo[,-23],type = "response")

# Let's see the Min and Max values using summary function() which give us the prediction range on testdata from .06% TO 80% 
summary(xgb.predictions)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02064 0.28525 0.37131 0.38993 0.48937 0.86856 

perform_fn_xgboost <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(xgb.predictions >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_attrition, testdata$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from prediction range for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100){
  OUT[i,] = perform_fn_xgboost(s[i])
} 
## Plotting the Sensitivity,Specificity and Accuracy of the model to identify the final cut off value. 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

## Getting the cutoff value. 
cutoff_logi_xgb <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff_logi_xgb  #0.385
# So based on the above plot we choose a cutoff value of 0.385 for final model
test_cutoff_attrition_logi_xgb <- factor(ifelse(xgb.predictions >=0.385, "yes", "no"))

# Here the confusion matrix gives us 3 measures which shows how well our model dscriminates between two classes
#Sensitivity is a ratio which gives true positive rate which came 0.5515289 for final model
#Specificity is a ratio which gives true negative rate which came 0.55154 for final model0.7502
#Accuracy - is a ratio which gives sum of true positive and true negative divided by all observation i.e. 0.5522566
conf_final_logi_xgb <- confusionMatrix(test_cutoff_attrition_logi_xgb, testdata$Performance.Tag, positive = "yes")
acc <- conf_final_logi_xgb$overall[1]  #0.5552566
sens <- conf_final_logi_xgb$byClass[1] #0.5495164
spec <- conf_final_logi_xgb$byClass[2] #0.55514

##############################################################################################################################
# Model performance on rejected population

#1) Cutoff for logistic model for demographic is - 0.505

df_No_CC_Demo_model <- dummy.data.frame(df_No_CC_Demo)
names(df_No_CC_Demo_model) <- make.names(names(df_No_CC_Demo_model))
df_No_CC_Demo_model <- df_No_CC_Demo_model[,-1]
df_No_CC_Demo_model$Age <- scale(df_No_CC_Demo_model$Age)
df_No_CC_Demo_model$No.of.dependents <- scale(df_No_CC_Demo_model$No.of.dependents)
df_No_CC_Demo_model$Income <- scale(df_No_CC_Demo_model$Income)
df_No_CC_Demo_model$No.of.months.in.current.residence <- scale(df_No_CC_Demo_model$No.of.months.in.current.residence)
df_No_CC_Demo_model$No.of.months.in.current.company <- scale(df_No_CC_Demo_model$No.of.months.in.current.company)
test_pred_rejected_demo = predict(final_model, type = "response", newdata = df_No_CC_Demo_model[-23])
test_cutoff_attrition_rejec_demo <- factor(ifelse(test_pred_rejected_demo >=0.505, "yes", "no"))
summary(test_cutoff_attrition_rejec_demo)
#  no yes 
# 701 724 
#% for defaulter is 724/1425 ~ 51%

#2) Cutoff for Random model for demographic is - 0.396

rf_pred_demo_rej_rf <- predict(demographic_rf, df_No_CC_Demo_model[, -23], type = "prob")
test_cutoff_attrition_rejec_demo_rf <- factor(ifelse(rf_pred_demo_rej_rf[,2] >=0.396, "yes", "no"))
summary(test_cutoff_attrition_rejec_demo_rf)
# no  yes 
# 269 1156 
#% defaulters in rejected dataset is 1156/1425 ~ 81.12%

#3) XG BOOST : Cut off is 0.377

labels <- as.numeric(df_No_CC_Demo_model$Performance.Tag)-1
ts_label <- as.numeric(df_No_CC_Demo_model$Performance.Tag)-1
Rej_XGb_demo = as.matrix(sapply(df_No_CC_Demo_model[,-23],as.numeric))
rf_pred_demo_rej_xg <- predict(xgb.model, Rej_XGb_demo[, -23], type = "response")
test_cutoff_attrition_rejec_demo_xgb <- factor(ifelse(rf_pred_demo_rej_xg >=0.377, "yes", "no"))
summary(test_cutoff_attrition_rejec_demo_rf)
# no  yes 
# 269 1156 

#% defaulters in rejected dataset is 1156/1425 ~ 81.12%

####### Result ###############################################################################

# Demographic Dataset : Random Forest model performs better than other models 
#Final Cutoff   = 0.396
#sensitivity    = 55.68%
#specificity    = 53.44%
#Accuracy       = 56.0%
#KS statistics  ~ 9
#AUC            ~ 56.
#% of defaulters on rejected population : 81.12%


##############################################################################################################################
## Model Building for Master dataframe

## splitting the data between train and test
# set.seed () function is used for reproducible random number results

df_woe$Performance.Tag <- as.factor(ifelse(df_woe$Performance.Tag==1,"yes","no"))
split_indices_woe <- sample.split(df_woe$Performance.Tag, SplitRatio = 0.70)
train_woe = df_woe[split_indices_woe,] 
test_woe = df_woe[!(split_indices_woe),]
table(df_woe$Performance.Tag)
nrow(train_woe)/nrow(df_woe) # [1] 0.70
nrow(test_woe)/nrow(df_woe) # [1] 0.30
# no   yes 
#66920  2947  
# 48907 obs of 28 variables in train dataset
# 20960 obs of 28 variables in test dataset
#Since the data is highly unbalanced we use SMOTE() function to balance it

train_woe_SMOTE <- SMOTE(Performance.Tag ~ ., train_woe , perc.over = 100 , perc.under = 200)
table(train_woe_SMOTE$Performance.Tag)

# To make all the variables name legal used names function 
names(train_woe_SMOTE) <- make.names(names(train_woe_SMOTE)) 
names(test_woe) <- make.names(names(test_woe))

##############################################################################################################################
## Logistic Regression : Model Building on master dataframe 

# Initial model containing all variables
# Model_1 - AIC 10506 nullDev 11440 resDev 10450

logistic_w1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_woe_SMOTE)
summary(logistic_w1)

# Task 2.1) Perform variable selection using the usual methods
#**********************************************************************************************************************************
# Stepwise selection
## In stepAIC function, we pass our first model i.e logistic_w1 and direction is set as both, because in stepwise,  
## both the forward selection of variables and backward elimination of variables happen simultaneously 
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Commented out the StepAIC function as it takes time to complete 
# and store the output of stepwise method into an object called logistic_w2

#logistic_w2 <- stepAIC(logistic_w1, direction = "both") #commented

# logistic_w2 - AIC 10500 nullDev 11440 resDev 10468
logistic_w2 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     No.of.times.90.DPD.or.worse.in.last.12.months.WOE + No.of.times.60.DPD.or.worse.in.last.12.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE + No.of.trades.opened.in.last.12.months.WOE + 
                     No.of.PL.trades.opened.in.last.12.months.WOE + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + Presence.of.open.auto.loan.WOE + 
                     Age.WOE + No.of.dependents.WOE + Income.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                     data = train_woe_SMOTE)
summary(logistic_w2)
sort(vif(logistic_w2))

#logistic_w3 - Removed variable -  No.of.PL.trades.opened.in.last.12.months:WOE VIF- 7.698965 and p value- 0.018298 and 
#AIC 10498 nullDev 11440 resDev 10468

logistic_w3 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     No.of.times.90.DPD.or.worse.in.last.12.months.WOE + No.of.times.60.DPD.or.worse.in.last.12.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE + No.of.trades.opened.in.last.12.months.WOE + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + Presence.of.open.auto.loan.WOE + 
                     Age.WOE + No.of.dependents.WOE + Income.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                   data = train_woe_SMOTE)
summary(logistic_w3)
sort(vif(logistic_w3))

#logistic_w4 - Removed variable -  No.of.times.60.DPD.or.worse.in.last.12.months.WOE VIF - 5.310566 and p value- 0.091344 and 
#AIC 10499 nullDev 11440 resDev 10471

logistic_w4 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     No.of.times.90.DPD.or.worse.in.last.12.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE + No.of.trades.opened.in.last.12.months.WOE + 
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + Presence.of.open.auto.loan.WOE + 
                     Age.WOE + No.of.dependents.WOE + Income.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                   data = train_woe_SMOTE)
summary(logistic_w4)
sort(vif(logistic_w4))

#Model_5 - Removed variable -  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE VIF - 4.383480 and p value- 0.044484 and 
#AIC 10514 nullDev 11440 resDev 10488

logistic_w5 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     No.of.times.90.DPD.or.worse.in.last.12.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE + No.of.trades.opened.in.last.12.months.WOE + 
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + Presence.of.open.auto.loan.WOE + 
                     Age.WOE + No.of.dependents.WOE + Income.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                   data = train_woe_SMOTE)
summary(logistic_w5)
sort(vif(logistic_w5))

#Model_6 - Removed variable -  No.of.times.90.DPD.or.worse.in.last.12.months.WOE VIF - 3.586560 and p value- 0.084188 and 
#AIC 10514 nullDev 11440 resDev 10490

logistic_w6 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE + No.of.trades.opened.in.last.12.months.WOE + 
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + Presence.of.open.auto.loan.WOE + 
                     Age.WOE + No.of.dependents.WOE + Income.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                   data = train_woe_SMOTE)
summary(logistic_w6)
sort(vif(logistic_w6))

#Model_7 - Removed variable -  No.of.trades.opened.in.last.12.months.WOE VIF - 3.268989 and p value- 0.033439 and 
#AIC 10522 nullDev 11440 resDev 10500

logistic_w7 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE +
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + Presence.of.open.auto.loan.WOE + 
                     Age.WOE + No.of.dependents.WOE + Income.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                   data = train_woe_SMOTE)
summary(logistic_w7)
sort(vif(logistic_w7))

# we cannot exclude any other variables based on vif all of them have low vif; those with higher vif are very significant and not correlated
#Model_8 - Removed variable -  Age.WOE  p value- 0.038873  and 
#AIC 10521 nullDev 11440 resDev 10501

logistic_w8 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE +
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + Presence.of.open.auto.loan.WOE + 
                      No.of.dependents.WOE + Income.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                   data = train_woe_SMOTE)
summary(logistic_w8)

#Model_9 - Removed variable -  Presence.of.open.auto.loan.WOE  p value- 0.138675  and 
#AIC 10522 nullDev 11440 resDev 10504

logistic_w9 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE +
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + 
                     No.of.dependents.WOE + Income.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                   data = train_woe_SMOTE)
summary(logistic_w9)

#Model_10 - Removed variable -  Income.WOE  p value- 0.007682  and 
#AIC 10527 nullDev 11440 resDev 10511

logistic_w10 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                     Avgas.CC.Utilization.in.last.12.months.WOE +
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                     Outstanding.Balance.WOE + 
                     No.of.dependents.WOE + Profession.WOE + 
                     No.of.months.in.current.residence.WOE, family = "binomial", 
                   data = train_woe_SMOTE)
summary(logistic_w10)

final_model_woe <- logistic_w10

############################################################################################################
## 6) Logistic Regression: Model Evaluation
# In a model evaluation process, we use our model that has been prepared with the help of training data, 
# to make predictions for the testing data. We used below methods to evaluate the model
# i) Accuracy-Sensitivity-Specificity ii) KS Statistic iii) Gain-Lift  iv) ROC curve

## A) Acuracy-Sensitivity-Specificity
# predict() function is used to predict probabilities of attrition for test data
# Input to predict function - our final_model(model_number 6) and type as response to get the output in terms of probability
# We need to exclude Performance-tag column as it is the dependent variables

test_pred_woe = predict(final_model_woe, type = "response", newdata = test_woe[-28])

# Let's see the Min and Max values using summary function() which give us the prediction range on testdata from .06% TO 80% 
summary(test_pred_woe)
# Min.   1st Qu. Median  Mean    3rd Qu. Max. 
#0.2044  0.2957  0.4432  0.4556  0.6019  0.7566

# Here we have probabilities for each data points(employeeid) so now we can make the decision whether the applicant will churn or not based on some cutoff value 
# To calculate the cutoff value we use table/ConfusionMatrix which measure the performance of a classification model

perform_fn_logi <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred_woe >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_attrition, test_woe$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from prediction range (from min.06% to max 80%) for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100){
  OUT[i,] = perform_fn_logi(s[i])
} 
## Plotting the Sensitivity,Specificity and Accuracy of the model to identify the final cut off value. 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

## Getting the cutoff value. 
cutoff_cb_logi <- s[which(abs(OUT[,1]-OUT[,2])<0.015)]
cutoff_cb_logi #0.528
# So based on the above plot we choose a cutoff value of 0.528 for final model
# since we are focusing on applicants who will churn and that is capture in sensitivity so 
# when the cutoff is increased,its sensitivity decrease and specificity increase so we choose 0.528 as cutoff 
# Here the confusion matrix gives us 3 measures which shows how well our model dscriminates between two classes
#Sensitivity is a ratio which gives true positive rate which came 0.6414 for final model
#Specificity is a ratio which gives true negative rate which came 0.6273 for final model0.7502
#Accuracy - is a ratio which gives sum of true positive and true negative divided by all observation i.e. 0.631

test_cutoff_attrition_woe_logi <- factor(ifelse(test_pred_woe >=0.528, "yes", "no"))
conf_final_logi <- confusionMatrix(test_cutoff_attrition_woe_logi, test_woe$Performance.Tag, positive = "yes")
acc <- conf_final_logi$overall[1]  #0.631
sens <- conf_final_logi$byClass[1] #0.6414
spec <- conf_final_logi$byClass[2] #0.6273

## B) KS -statistic - 
## Assigning numeric values 1 and o for Yes and No values of predicted and actual attrition 

test_cutoff_attrition_woe <- ifelse(test_cutoff_attrition_woe_logi=="yes",1,0)
test_actual_attrition_woe <- ifelse(test_woe$Performance.Tag=="yes",1,0)
# prediction() function is used to create prediction objects based on the actual and predicted cutoff attrition  
# Performance() function is used to to perofrm all kinds of predictor evaluations.
# here we use parameters tpr (true positive rate) & fpr (false positive rate)

pred_object_test_woe<- prediction(test_cutoff_attrition_woe, test_actual_attrition_woe)
performance_measures_test_woe<- ROCR::performance(pred_object_test_woe, "tpr", "fpr")

#plot sensitivity/specificity curve (x-axis:specificity,y-axis:sensitivity)
plot(performance_measures_test_woe)

# Calculating KS Statistic value 
ks_table_test_logi_cb <- attr(performance_measures_test_woe, "y.values")[[1]] - 
  (attr(performance_measures_test_woe, "x.values")[[1]])
max(ks_table_test_logi_cb) 
##KS statistic comes 26.75# which shows that our model is good 

#ks chart
deciles<-ROCR::performance(pred_object_test_woe,'rpp')
k_stat_matrix<-data.frame(10*(deciles@y.values[[1]]),(performance_measures_test_woe@y.values[[1]]-performance_measures_test_woe@x.values[[1]]))
colnames(k_stat_matrix)[1]<-"deciles"
colnames(k_stat_matrix)[2]<-"k_statistic"
k_stat_matrix$k_statistic<-round(k_stat_matrix$k_statistic,2)
plot(k_stat_matrix)
abline(h=2.7,v=5)

## C) Lift & Gain Chart 
#The gain statistic is a clear indicator of the advantage offered by the model
#The Lift tells us the factor by whichmodel is outperforming a random model, i.e. a model-less situation.
Attrition_decile_woe = lift(test_actual_attrition_woe, test_cutoff_attrition_woe, groups = 10)
Attrition_decile_woe

# bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#  1      1  2096       180     180  20.4    2.04
#2      2  2096       156     336  38.0    1.90
#3      3  2096       133     469  53.1    1.77
#4      4  2096       112     581  65.7    1.64
#5      5  2096        82     663  75      1.5 
#....
#10     10  2096        25     884 100      1   
   
# graph for Gain and lift

plot(Attrition_decile_woe$bucket,Attrition_decile_woe$Gain,col="black",type="l",xlab="Bucket",ylab = "% of Defaulters")
graphics::plot(Attrition_decile_woe$bucket, Attrition_decile_woe$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")

## D) Area under the curve:
plot(AUC::roc(test_pred_woe,test_woe$Performance.Tag))
auc(roc(test_pred_woe,test_woe$Performance.Tag))
#area under ROC CURVE is 67.08% 

##############################################################################################################################
## Decision Tree: Model Building on Master dataframe 

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister()

########################## Hyperparameter tuning and cross validation ##########################################

tree.controlwoe = trainControl(method = "cv", number = 5)
tree.gridwoe = expand.grid(cp = seq(0, 0.02, 0.0025))
tree.modelwoe <- caret::train(Performance.Tag ~ .,
                    data = train_woe_SMOTE,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = tree.controlwoe,
                    tuneGrid = tree.gridwoe,
                    control = rpart.control(minsplit = 50,
                                            minbucket = 20))
# Checked at cross validated model results & look at best value of hyperparameter & make predictions on test set
tree.modelwoe
tree.modelwoe$bestTune
tree.predictwoe <- predict.train(tree.modelwoe, test_woe)
confusionMatrix(tree.predictwoe, test_woe$Performance.Tag) 
#Accuracy : 0.6709
#Sensitivity : 0.6767
#Specificity : 0.5384

accuracy_graph <- data.frame(tree.modelwoe$results)
ggplot(data = accuracy_graph, aes(x = cp, y = Accuracy*100)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")

##############################################################################################################################
## Random Forest : Model Building on master dataframe 

########################## Hyperparameter tuning and cross validation ##########################################

#bestmtry <- tuneRF(train_woe_SMOTE[,-28],train_woe_SMOTE[,28] , #stepFactor=1.5, ntree=500,trace = TRUE,plot=TRUE,doBest = FALSE)
#print(bestmtry)

#Output : 
#mtry OOBError
#4.OOB    4    0.223
#5.OOB    5    0.222
#7.OOB    7    0.224

# Since OOB error for mtry = 5 is minimum hence choosing mtry=5 for building the model

rf_pred_merge <- randomForest(Performance.Tag ~., data = train_woe_SMOTE, proximity = F, do.trace = T, mtry = 5 , ntree= 500)
rf_pred_master <- predict(rf_pred_merge,newdata = test_woe[-28],type = "prob")

perform_fn_rf_master <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_master[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_woe$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)
OUT_rf = matrix(0,100,3)
for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf_master(s[i])
} 

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.02)]
cutoff_rf
predicted_response_cb_rf <- factor(ifelse(rf_pred_master[, 2] >= 0.415, "yes", "no"))
conf_forest_cb <- confusionMatrix(predicted_response_cb_rf, test_woe$Performance.Tag, positive = "yes")
conf_forest_cb

#Sensitivity
conf_forest_cb$byClass[1]
#63.8
# Specificity 
conf_forest_cb$byClass[2]
#62.7
#Accuracy 
conf_forest_cb$overall[1]
#62.7

importance <- rf_pred_merge$importance 
importance <- data.frame(importance)

varImpPlot(rf_pred_merge)

## B) KS -statistic - 
# Assigning numeric values 1 and o for Yes and No values of predicted and actual attrition 
test_cutoff_attrition_woe_rf <- ifelse(predicted_response_cb_rf=="yes",1,0)
test_actual_attrition_woe_rf <- ifelse(test_woe$Performance.Tag=="yes",1,0)
# prediction() function is used to create prediction objects based on the actual and predicted cutoff attrition  
# Performance() function is used to to perofrm all kinds of predictor evaluations.
# here we use parameters tpr (true positive rate) & fpr (false positive rate)

pred_object_test_rf<- prediction(test_cutoff_attrition_woe_rf, test_actual_attrition_woe_rf)
performance_measures_test_rf<- ROCR::performance(pred_object_test_rf, "tpr", "fpr")

#plot sensitivity/specificity curve (x-axis:specificity,y-axis:sensitivity)
plot(performance_measures_test_rf)

# Calculating KS Statistic value 
ks_table_test_rf <- attr(performance_measures_test_rf, "y.values")[[1]] - 
  (attr(performance_measures_test_rf, "x.values")[[1]])
max(ks_table_test_rf) 
##KS statistic comes 25.61# which shows that our model is good and lies in the top deciles

#ks chart
deciles<-ROCR::performance(pred_object_test_rf,'rpp')
k_stat_matrix<-data.frame(10*(deciles@y.values[[1]]),(performance_measures_test_rf@y.values[[1]]-performance_measures_test_rf@x.values[[1]]))
colnames(k_stat_matrix)[1]<-"deciles"
colnames(k_stat_matrix)[2]<-"k_statistic"
k_stat_matrix$k_statistic<-round(k_stat_matrix$k_statistic,2)
plot(k_stat_matrix)
abline(h=2.7,v=5)
#ks statistic lies withinin first 5 deciles

## C) Lift & Gain Chart 
# The gain statistic is a clear indicator of the advantage offered by the model
# The Lift tells us the factor by whichmodel is outperforming a random model, i.e. a model-less situation.
# as observed that table shows when using our best fit model vs random model then 
# at the 4th decile we'll be able to identified ~60+% of the applicants who can default.
Attrition_decile_woe_rf = lift(test_actual_attrition_woe_rf, test_cutoff_attrition_woe_rf, groups = 10)
Attrition_decile_woe_rf

#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#  1      1  2096       139     139  15.7    1.57
#2      2  2096       136     275  31.1    1.56
#3      3  2096       151     426  48.2    1.61
#4      4  2096       140     566  64.0    1.60
#5      5  2096        45     611  69.1    1.38
#...
#10     10  2096        50     884 100      1     
# We can identify 69% of defaulters when targetting 50% of population. 
 
# graph for Gain and lift
graphics::plot(Attrition_decile_woe_rf$bucket, Attrition_decile_woe_rf$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")
gain <- ROCR::performance(pred_object_test_rf, "tpr", "rpp")
plot(gain)
#within first 4 deciles as per the model we are able to predict 60% of defaulters correctly

#Area under the curve:
plot(AUC::roc(test_cutoff_attrition_woe_rf,test_woe$Performance.Tag))
auc(roc(test_cutoff_attrition_woe_rf,test_woe$Performance.Tag))
#area under ROC CURVE is 62.8% 

##############################################################################################################################
## XGBoost : Model Building on master dataframe

labels <- train_woe_SMOTE$Performance.Tag
ts_label <- test_woe$Performance.Tag

labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

train_merge = as.matrix(sapply(train_woe_SMOTE,as.numeric))
test_merge = as.matrix(sapply(test_woe,as.numeric))

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric ="auc",
  eta=0.7,
  gamma=5,
  max_depth=3,
  min_child_weight=2,
  subsample=1,
  colsample_bytree= 0.933
)

########################## Hyperparameter tuning and cross validation ##########################################

bst.cv = xgb.cv(
  params=params,
  data = train_merge[,-28],
  label = labels, #train[,23],
  nfold = 5,
  nrounds=300,
  prediction=T)
#highest test AUC was obtained was 0.778 after 118 rounds of 5 fold cross validation

watchlist <- list(train_merge, test_merge)
xgb.model <- xgboost::xgboost(params=params, data=train_merge[,-28],
                     label=labels,
                     nrounds = 13, watchlist)

xgb.predictions <- predict(xgb.model, test_merge[,-28],type = "response")

# Let's see the Min and Max values using summary function() which give us the prediction range on testdata from .06% TO 80% 
summary(xgb.predictions)
# Min.   1st Qu.  Median  Mean   3rd Qu.    Max. 
#0.04524 0.19571 0.34141 0.33686 0.45462 0.93737  

perform_fn_xgboost_merger <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(xgb.predictions >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_attrition, test_woe$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100){
  OUT[i,] = perform_fn_xgboost_merger(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_cb_xgboost <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff_cb_xgboost  #0.385
# So based on the above plot we choose a cutoff value of 0.385 for final model
# since we are focusing on employees who will churn and that is capture in sensitivity so 
# when the cutoff is increased,its sensitivity decrease and specificity increase so we choose 0.1775 as cutoff 
test_cutoff_attrition_cb_xgboost <- factor(ifelse(xgb.predictions >=0.385, "yes", "no"))

# Here the confusion matrix gives us 3 measures which shows how well our model dscriminates between two classes
#Sensitivity is a ratio which gives true positive rate which came 0.585504 for final model
#Specificity is a ratio which gives true negative rate which came 0.523037 for final model0.7502
#Accuracy - is a ratio which gives sum of true positive and true negative divided by all observation i.e. 0.5256741

conf_final_cb <- confusionMatrix(test_cutoff_attrition_cb_xgboost, test_woe$Performance.Tag, positive = "yes")
acc <- conf_final_cb$overall[1]
sens <- conf_final_cb$byClass[1]
spec <- conf_final_cb$byClass[2]

acc  #0.6121183 
sens #0.6085973
spec #0.6111734

mat <- xgb.importance (feature_names = colnames(train_merge),model = xgb.model)
xgb.plot.importance (importance_matrix = mat[1:20]) 

##############################################################################################################################
## SVM Linear : Model Building on master dataframe 

Model11_svm <- ksvm(Performance.Tag ~ ., data = train_woe_SMOTE, scale = FALSE, kernel = "vanilladot")
print(Model11_svm)
Eval1_linear<- predict(Model11_svm, test_woe[-28])
confusionMatrix(Eval1_linear,test_woe$Performance.Tag)

#Accuracy : 0.5629
#Sensitivity : 0.55577         
#Specificity : 0.72518  

# since Accuracy is too low we move to Radial kernal

##############################################################################################################################
## SVM RBF : Model Building on master dataframe 

Model11_RBF <- ksvm(Performance.Tag~ ., data = train_woe_SMOTE, scale = FALSE, kernel = "rbfdot")
print(Model11_RBF)
Eval11_RBF<- predict(Model11_RBF, test_woe[-28])
confusionMatrix(Eval11_RBF,test_woe$Performance.Tag)

#Accuracy : 0.5748
#Sensitivity : 0.57149         
#Specificity : 0.65950

####################### Hyperparameter tuning and cross validation ##############################################################

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister()

trainControl_cb <- trainControl(method="cv", number=5)
grid_cb <- expand.grid( C=c(1,2,3),sigma=c(0.01,0.02))
# below hypertunning command takes some time to complete so commenting out and print the result based on hypertunning 
#fit.svm_rbf_cb <- caret::train(Performance.Tag~., data=train_woe_SMOTE, method="svmRadial", metric="Accuracy", 
#                            tuneGrid=grid_cb, trControl=trainControl_cb,preProcess = NULL)
#print(fit.svm_rbf_cb)
#plot(fit.svm_rbf_cb)

#C  sigma  Accuracy   Kappa    
#1  0.01   0.6423891  0.2847820
#1  0.02   0.6503873  0.3007761
#2  0.01   0.6442065  0.2884157
#2  0.02   0.6598384  0.3196784
#3  0.01   0.6488114  0.2976252
#3  0.02   0.6648074  0.3296163

# Re-run the model at C=3 and sigma = 0.02

Model2_RBF <- ksvm(Performance.Tag~ ., data = train_woe_SMOTE, scale = FALSE, kernel = "rbfdot",C=3, 
                   kpar = list(sigma = 0.02))
print(Model2_RBF)
Eval2_RBF<- predict(Model2_RBF, test_woe[-28])
confusionMatrix( Eval2_RBF,test_woe$Performance.Tag)

#Accuracy : 0.584 
#Sensitivity : 0.5797          
#Specificity : 0.6877  

##############################################################################################################################
## Model performance on rejected population 

#1) Cutoff for logistic model for master is - 0.528

names(df_No_CC_woe) <- make.names(names(df_No_CC_woe))
test_pred_woe1 = predict(final_model_woe, type = "response", newdata = df_No_CC_woe[-28])
summary(test_pred_woe1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2959  0.6477  0.6907  0.6802  0.7263  0.8036 
test_rejected_woe_prob <- factor(ifelse(test_pred_woe1 >= 0.528 ,"yes","no"))
summary(test_rejected_woe_prob)
#non-Default  Default 
#46           1379 
#% for defaulter is ~ 96.7%

#2) Cutoff for Random model for master is - 0.425
rf_pred_master1 <- predict(rf_pred_merge,newdata = df_No_CC_woe[-28],type = "prob")
test_cutoff_attrition_rejec_master_rf <- factor(ifelse(rf_pred_master1[, 2] >=0.425, "yes", "no"))
summary(test_cutoff_attrition_rejec_master_rf)
#non-Default  Default 
#60            1365
#% defaulters in rejected dataset ~ 95.7%

## Based on all the metrics comparing and choosing the best model 
## Model      Accuracy    Sensitivity  Specificity   KS Statistics   AUC   Performance on Rejected Population  Cut off value           Attrition decile result
## Logistic   63.1         64.14        62.73         27             67.67     96.7                                               Identify 75% of defaulters when targetting 50% population 
## Random     62.7         63.8         62.7          25.6           62.8       95.7                                                We can identify 69% of defaulters when targetting 50% of population.
## Result : Logistic Regression is the best model on Merged dataset 
## Cut off value for Probability is 0.528. 

########## Result ##############################################################################

# Merge Dataset : Logistic regression model performs better than other models 
#Final Cutoff   = 0.528
#sensitivity    = 64.14%
#specificity    = 62.73%
#Accuracy       = 63.10%
#KS statistics  ~ 27
#AUC            ~ 67.67
#% of defaulters on rejected population : 96%

#######################################################################################################################################################################
## Application Scorecard

#Application scorecard is used to derive the credit score for identifying good and bad applicants.
#Higher score indicates lesser risk of defaulting so the number of applicants below cut-off score 
# will be bad(risky) customers and we would not grant credit card to applicants. 

test_woe$P <- test_pred_woe
test_woe$odds <- (1-test_woe$P)/(test_woe$P)
test_woe$score <- 333 + (20 *(log(test_woe$odds)/log(2)))
summary(test_woe$score)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#302.8   321.7   339.5   338.7   357.2   368.7 
CUTOFF_SCORE <- 333 + (20*log((1-0.528)/0.528)/log(2)) 
CUTOFF_SCORE  #329.765
boxplot(test_woe$score)
ggplot(test_woe,aes(score))+geom_histogram()
test_woe$Prediction <- factor(ifelse(test_woe$score >=329.765, "Non-Default", "Default"))
table(test_woe$Prediction)
#Default  Non-Default 
#8073     12887
# The score distribution  for  the approved candidates shows a uniform distribution with most values lying between 320 and 372 with the 
# minimum and maximum score being  300 and 372 respectively. 

##############################################################################################################################
## Application Scorecard for Rejected population 

df_No_CC_woe$P <- test_pred_woe1
df_No_CC_woe$odds <- (1-df_No_CC_woe$P)/(df_No_CC_woe$P)
df_No_CC_woe$score <- 333 + (20 *(log(df_No_CC_woe$odds)/log(2)))
summary(df_No_CC_woe$score)
#Min.    1st Qu.  Median    Mean   3rd Qu.    Max. 
#302.8   309.7    312.2     313.8   316.2   351.4  
# mean score for rejected population is 313 and approved population is 338.7

df_No_CC_woe$Prediction <- factor(ifelse(df_No_CC_woe$score >=329.765, "Non-Default", "Defalut"))
table(df_No_CC_woe$Prediction)
boxplot(df_No_CC_woe$score)
ggplot(test_woe,aes(score))+geom_histogram()
#Default Non-Defalut 
#1379          46
# 96.7% are defaulters among the rejected population.
# We can observe that the graph is left skewed & score distribution for the rejected candidates mostly lies below the cut off score i.e.  ~ 330; 
# which shows almost all of the rejected candidates were defaulters. 

##############################################################################################################################
## Financial Assessment of Project 

# Financial Benefit of a model will be in terms of either
# a. decreasing the rejection the non-defaulters
# b. increasing the rejection of defaulters
#P&L of banks Profit = revenue-costs
#Revenue - a) Interest on asset products & Risk-based fee, affinity rebates, cross-sell, annual fee etc
#Cost - a) Net credit losses,operating expenses etc

# # as observed that table shows below when using our best fit model i.e logistic model 
# at the 5th decile we'll be able to identified ~75% of the prospects.

# bucket total totalresp Cumresp  Gain Cumlift
#   1    2096       180     180  20.4    2.04
#   2    2096       156     336  38.0    1.90
#   3    2096       133     469  53.1    1.77
#   4    2096       112     581  65.7    1.64
#   5    2096        82     663  75      1.5 
#....
#  10    2096        25     884 100      1 

## Average cost for targeting the top 75% applicants

## With Model : we can predict 7860 defaulters when targetting 50% of population i.e. 10480
## Without Model : We can  predict 419 defaulters without model when  targetting same number 
## Lets assume the loss due to an approved applicant defaulting to be 1000$  
## Potential loss which did not occur when our model is applied : (7860-419) * 1000 ~ 7.4 Million 

## Confusion Matrix of our final model : 

#              Reference
#Prediction    no      yes
### no         12595   320
##  yes         7481   564

## We have assumed a revenue loss of 500$ when application of a non defaulter is rejected and a credit loss of 1000$ when any approved applicant defaults.
## Earnings for the bank  : credit loss saved + revenue loss prevented 
## 564*1000 + 12595*500 : 6297500 
## Loss to the bank : revenue loss + credit loss 
## 7481*500 = 3740500 +  320*1000 = 320000  =  4060500
## Net Profit : (Total earnings - Loss) = (6861500 - 4060500) = 2,782,000 ~ $2.8 Million 


##############################################################################################################################
## Conclusion

#1. Demographic Dataset : Random Forest model performs better than other models 
#Final Cutoff   = 0.396
#sensitivity    = 55.68%
#specificity    = 53.44%
#Accuracy       = 56.0%
#KS statistics  ~ 9
#AUC            ~ 56.
#% of defaulters on rejected population : 81.12%

#2. Merge Dataset : Logistic regression model performs better than other models 
#Final Cutoff   = 0.528
#sensitivity    = 64.14%
#specificity    = 62.73%
#Accuracy       = 63.10%
#KS statistics  ~ 27
#AUC            ~ 67.67
#% of defaulters on rejected population : 96%

#3. significant variables using WOE and Information Value Analysis  
#[1] "Avgas.CC.Utilization.in.last.12.months"   #[2] "No.of.trades.opened.in.last.12.months"                          
#[3] "No.of.PL.trades.opened.in.last.12.months" #[4] "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
#[5] "Outstanding.Balance"  #[6] "No.of.times.30.DPD.or.worse.in.last.6.months"   #[7] "Total.No.of.Trades"                                             
#[8] "No.of.PL.trades.opened.in.last.6.months"  #[9] "No.of.times.90.DPD.or.worse.in.last.12.months"                  
#[10] "No.of.times.60.DPD.or.worse.in.last.6.months"  #[11] "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." 
#[12] "No.of.times.30.DPD.or.worse.in.last.12.months" #[13] "No.of.trades.opened.in.last.6.months"                           
#[14] "No.of.times.60.DPD.or.worse.in.last.12.months" #[15] "No.of.times.90.DPD.or.worse.in.last.6.months"                   
#[16] "No.of.months.in.current.residence" #[17] "Income"                                                         
#[18] "No.of.months.in.current.company"   

#4. Significant variables in Demographic dataset :
# No.of.months.in.current.residence
# No.of.months.in.current.company
# Income
# Age
# No.of.dependents

#5. Significant variables in Merge dataset :
#No.of.times.30.DPD.or.worse.in.last.6.months.WOE  
#Avgas.CC.Utilization.in.last.12.months.WOE 
#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE 
#Outstanding.Balance.WOE  
#No.of.dependents.WOE  
#Profession.WOE  
#No.of.months.in.current.residence.WOE

#6. Application scorecard is built on logistic regression model with Cutoff score ~ 330

#7. Financial assesment of project 
#Net_financial_gain = ~ 2.8 Million 
#Revenue_loss = ~ 37%
#Credit loss saved = ~ 2.5%

########################################## END #########################################################################
