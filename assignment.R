##------------Bank Marketing Analysis---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling and the business objective is to 
# achieve 80% of total responders at the minimum possible cost. 
#-------------------------------------------------------

# Loading bank marketing data in the working directory. 
setwd("~/Documents/bfsi elective")
bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response #0.11

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 
agg_age
# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Duration variable needed to be removed from the data set as:

#1. When the marketing team procures prospect data, 'duration' is not present in it, 
#since the call hasn't been made yet.
#2. In your analysis of marketing cost and response, you had assumed that 
#the cost of a phone call is independent of duration (???1 per call) - which is not true

#-----------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

str(bank_data)

# Model Building----------------------------------------------------------------
# Required Packages

library(caret)
library(caTools)
library(dummies)
#install.packages("MASS")
library(MASS)
library(car)
# Removing binning variables

bank_data <- bank_data[, -21]

bank_data$response <- as.integer(bank_data$response)

#k1 <- bank_data

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data) #0.70

nrow(test)/nrow(bank_data) #0.29

# removing Duration from test data and train data
train_duration <- train$duration
train <- train[,-45]
test_duration <- test$duration
test <- test[,-45]
# a) model building using logistic regression

model_1 <- glm(response ~ ., family = "binomial", data = train)

summary(model_1)

# Using stepwise algorithm for removing insignificant variables
#stepAIC(model_1, direction = "both")

model_2 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + nr.employed + `previousMore than_3_times`, 
               family = "binomial", data = train)
summary(model_2)
vif(model_2)

# removing emp.var.rate as having very high vif

model_3 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +
                 cons.price.idx + cons.conf.idx + nr.employed + `previousMore than_3_times`, 
               family = "binomial", data = train)
summary(model_3)
vif(model_3)

# removing monthoct as not significant

model_4 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov +
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +
                 cons.price.idx + cons.conf.idx + nr.employed + `previousMore than_3_times`, 
               family = "binomial", data = train)
summary(model_4)

# removing previousMore than_3_times as not significant

model_5 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov +
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +
                 cons.price.idx + cons.conf.idx + nr.employed, 
               family = "binomial", data = train)
summary(model_5)

# removing day_of_weekfri as less significant

model_6 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov +
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +
                 cons.price.idx + cons.conf.idx + nr.employed, 
               family = "binomial", data = train)
summary(model_6)

# removing educationPrimary_Education

model_7 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced +
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov +
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +
                 cons.price.idx + cons.conf.idx + nr.employed, 
               family = "binomial", data = train)
summary(model_7)

# removing maritaldivorced

model_8 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                 jobtechnician +
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov +
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +
                 cons.price.idx + cons.conf.idx + nr.employed, 
               family = "binomial", data = train)
summary(model_8)

# removing monthnov
model_9 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                 jobtechnician +
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay +
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +
                 cons.price.idx + cons.conf.idx + nr.employed, 
               family = "binomial", data = train)
summary(model_9)

# removing jobadmin. 

model_10 <- glm(formula = response ~ jobretired + jobstudent + 
                  jobtechnician +
                  educationTertiary_Education + contactcellular + monthapr + 
                  monthjul + monthjun + monthmar + monthmay +
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +
                  cons.price.idx + cons.conf.idx + nr.employed, 
                family = "binomial", data = train)
summary(model_10)

# removing jobtechnician

model_11 <- glm(formula = response ~ jobretired + jobstudent + 
                  educationTertiary_Education + contactcellular + monthapr + 
                  monthjul + monthjun + monthmar + monthmay +
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +
                  cons.price.idx + cons.conf.idx + nr.employed, 
                family = "binomial", data = train)
summary(model_11)

# removing cons.price.idx

model_12 <- glm(formula = response ~ jobretired + jobstudent + 
                  educationTertiary_Education + contactcellular + monthapr + 
                  monthjul + monthjun + monthmar + monthmay +
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +
                  cons.conf.idx + nr.employed, 
                family = "binomial", data = train)
summary(model_12)

# removing educationTertiary_Education

model_13 <- glm(formula = response ~ jobretired + jobstudent + 
                  contactcellular + monthapr + 
                  monthjul + monthjun + monthmar + monthmay +
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +
                  cons.conf.idx + nr.employed, 
                family = "binomial", data = train)
summary(model_13)

# removing jobstudent

model_14 <- glm(formula = response ~ jobretired +
                  contactcellular + monthapr + 
                  monthjul + monthjun + monthmar + monthmay +
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +
                  cons.conf.idx + nr.employed, 
                family = "binomial", data = train)
summary(model_14)
#str(test)
model_14$formula
# variables selected for final model are

#jobretired + contactcellular + monthapr + monthjul + 
 # monthjun + monthmar + monthmay + day_of_weekmon + campaign + 
  #pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
  #poutcomefailure + cons.conf.idx + nr.employed

logistic_final <- model_14


predictions_logit <- predict(logistic_final, newdata = test[, -60], type = "response")
#predictions_logit <- sort(predictions_logit, decreasing = TRUE)
summary(predictions_logit)
pred_probablity <- predictions_logit
## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit>=.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf
#  Accuracy : 0.9001             
# Sensitivity : 0.24210        
# Specificity : 0.98367        
class(predicted_response)
class(test$response)
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,2]-OUT[,1])<=0.10)]

cutoff # 0.0792
# Let's choose a cutoff value of 0.0792 for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.0792, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #0.76

sens # 0.68

spec # 0.77

#-----------Model Evaluation------------------------------------------

test$pred_probablity <- pred_probablity

test$predicted_response <- predicted_response

test$prospect_id <- 1:nrow(test)

# adding duration to the test data set
#test_duration
test$test_duration <- test_duration

# adding cost column to test dataset (Cost per call (INR) = 0.033*(duration_in_seconds) + 0.8)

test$cost <- 0.033*test$test_duration +0.8

#creating a new dataframe

model_eval <- test[, c("prospect_id", "response", "predicted_response", "pred_probablity", "test_duration", "cost")]

summary(model_eval$response)
summary(model_eval$predicted_response)

response_rate <- table(test$response)[2]/(table(test$response)[1] + table(test$response)[2])

response_rate

# sorting the probabilities in decreasing order 

model_eval <- model_eval[order(model_eval$pred_probablity, decreasing = T), ]

summary(model_eval$response[1:12356])
summary(model_eval$predicted_response[1:12356])

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
install.packages("dplyr")
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
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

# Create a Table of cumulative gain and lift 

model_eval$response <- as.factor(ifelse(model_eval$response=="yes",1,0))

LG = lift(model_eval$response, model_eval$pred_probablity, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")

# as pet the problem statement we need to figure out the X in top X%, 
#i.e. how many prospects should be called to meet the business objective.

# here X% = 80% which is covered in 5th decile as we can see from the LG table

head(LG)

#bucket total totalresp Cumresp  Gain Cumlift
#<int>   <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  1236       621     621  44.6    4.46
#2      2  1236       238     859  61.7    3.09
#3      3  1235       110     969  69.6    2.32
#4      4  1236        75    1044  75      1.88
#5      5  1235        76    1120  80.5    1.61
#6      6  1236        62    1182  84.9    1.42

# here 5th decile means by taking 50% of the total test responders,
# which cover 80% percent of the total top predicted responder who said yes.
nrow(model_eval)
.50*nrow(model_eval)
#model_eval$row <- 1:nrow(model_eval)

Top_80_per <- model_eval[1:6178,]


#Find the number of top X% prospects you should target to meet the business objective
nrow(Top_80_per) #6178

# total 6178 people should be contacted which covers 80% of test data who can respond yes

# finding avg cal duration for targeting these 6178 prospects

mean(Top_80_per$test_duration) #267.086

# so average call duration should be 267.086

