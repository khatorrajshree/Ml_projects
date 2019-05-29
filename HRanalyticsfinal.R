install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("cowplot")
install.packages("GGally")
install.packages("caTools")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)

in_time <- read.csv("in_time.csv", stringsAsFactors = F,check.names = FALSE)
out_time <- read.csv("out_time.csv", stringsAsFactors = F,check.names = FALSE)
in_time <- in_time[colSums(!is.na(in_time))!=0] 
out_time <- out_time[colSums(!is.na(out_time))!=0] ## Removing NA values 
colnames(in_time)[1] <- 'EmployeeID'
colnames(out_time)[1] <- 'EmployeeID'

hours_spent <- data.frame(sapply(2:250, function(i) difftime(time1 = out_time[,i], time2 = in_time[,i], units = "hours")))

hours_spent$Average_hours <- as.integer(rowMeans(hours_spent,na.rm = T))
##format(hours_spent)
hours_spent <- cbind(EmployeeID=in_time$EmployeeID,hours_spent)

hours_spent <- hours_spent[,c(1,251)]
table(hours_spent$Average_hours)

# Loading 3 more files
manager.data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
employee.data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general.data <- read.csv("general_data.csv", stringsAsFactors = F)

str(manager.data) # 4410 obsevation of 3 variables
str(employee.data) # 4410 observation of 4 variables
str(general.data) # 4410 observation of 24 variables including target variable
str(hours_spent) # 4410 observation of 2 variable

# checking for duplicates
length(unique(manager.data$EmployeeID)) # no duplicates
length(unique(employee.data$EmployeeID)) # no duplicates
length(unique(general.data$EmployeeID)) # no duplicates
length(unique(hours_spent$EmployeeID)) # no duplicates

# checking if employee id is same in all above 4 data set

setdiff(manager.data$EmployeeID, employee.data$EmployeeID) # same emp id across these data set
setdiff(manager.data$EmployeeID, general.data$EmployeeID) # same emp id across these data set
setdiff(general.data$EmployeeID, hours_spent$EmployeeID) # same emp id across these data set

# merging all 4 data set in 1 data frame
emp_attrition <- merge(employee.data, manager.data, by = "EmployeeID")
emp_attrition <- merge(emp_attrition, hours_spent, by = "EmployeeID")
emp_attrition <- merge(general.data, emp_attrition, by = "EmployeeID")

str(emp_attrition) # contaning 4410 observation of 30 variables

table(emp_attrition$YearsWithCurrManager)

# continues variables-age(2), distancefromhome(6), monthlyincome(14) , 
# noofcompaniesworked(15),percentsalaryhike(17), total working years(20), 
#training time last year(21), years at company(22), years since last promotion(23), 
#years with curr manager(24), avaerage hours(30)

#table(emp_attrition$StockOptionLevel)
# na value treatment
summary(emp_attrition)
# NA value treatment
# No of companies worked 19 na, total working years 9 na, environment satisfaction 25 na
# job satisfaction 20 na, worklifebalance 38 na, 
emp_attrition <- na.omit(emp_attrition)
sum(is.na(emp_attrition))
str(emp_attrition)
# removing column with near zero variance
emp_attrition <- emp_attrition[, -c(9,16,18)]
#---------------------------------------------------------------------------------
# Exploratory data analysis

# Bar charts for categorical variable with stacked attrition information


bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(emp_attrition, aes(x=BusinessTravel, fill = factor(Attrition)))+geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= Department, fill = factor(Attrition)))+ geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= Education, fill = factor(Attrition))) +geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= EducationField, fill = factor(Attrition))) + geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= Gender, fill = factor(Attrition))) +geom_bar() + bar_theme1, align = "h")

# attrition is seen at Business travel - travel rarely or travel frequently
# department - research and development, sales
# education - 1234
# education field- life sciences and medical
# gender- both
str(emp_attrition)
plot_grid(ggplot(emp_attrition, aes(x= JobLevel, fill = factor(Attrition))) + geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= JobRole, fill= factor(Attrition))) + geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= EnvironmentSatisfaction, fill = factor(Attrition))) + geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= WorkLifeBalance, fill = factor(Attrition))) + geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= JobInvolvement, fill = factor(Attrition))) + geom_bar() + bar_theme1,
          ggplot(emp_attrition, aes(x= PerformanceRating, fill = factor(Attrition))) + geom_bar() + bar_theme1, align = "h")



ggpairs(emp_attrition[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked",
        "PercentSalaryHike", "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany",
        "YearsSinceLastPromotion", "YearsWithCurrManager", "Average_hours")])
#-------------------------------------------------------------------------------------
# outlyier treatment:
quantile(emp_attrition$YearsAtCompany, seq(0,1,.01))
emp_attrition$YearsAtCompany[which(emp_attrition$YearsAtCompany>25.00)]<-25.00

# scaling continues variables
str(emp_attrition)
continues <- emp_attrition[,c(2,6,13,14,15,17,18,19, 20, 21, 27)]
emp_attrition <- emp_attrition[, -c(2,6,13,14,15,17,18,19, 20, 21, 27)]

continues <- data.frame(sapply(continues, function(x) scale(x)))
emp_attrition <- cbind(continues, emp_attrition)
str(emp_attrition)
categorical <- emp_attrition[, -c(1:12)]
emp_attrition <- emp_attrition[, c(1:11)]

# convering chracter and int to factor
# Bringing the variables in the correct format

Fun_satisfaction <- function(P_satisfaction)
{
  if (P_satisfaction==1)
    return("Low")
  else if (P_satisfaction==2)
    return("Medium")
  else if (P_satisfaction==3)
    return("High")
  else if (P_satisfaction==4)
    return("Very High")
}

categorical$EnvironmentSatisfaction <- sapply(categorical$EnvironmentSatisfaction,Fun_satisfaction)
categorical$JobInvolvement <- sapply(categorical$JobInvolvement,Fun_satisfaction)
categorical$JobSatisfaction <- sapply(categorical$JobSatisfaction,Fun_satisfaction)
#empdata_final$WorkLifeBalance <- sapply(empdata_final$WorkLifeBalance,Fun_satisfaction)

## Creating function to convert Education Level to categorical ; and defining categories. 
functedulevel <- function(level)
{
  if (level==1)
    return("Below College")
  else if (level==2)
    return("College")
  else if (level==3)
    return("Bachelor")
  else if (level==4)
    return("Master")
  else if (level==5)
    return("Doctor")
}
categorical$Education <- sapply(categorical$Education,functedulevel)

## Performance Rating 
functrating <- function(rating)
{
  if (rating==1)
    return("Low")
  else if (rating==2)
    return("Good")
  else if (rating==3)
    return("Excellent")
  else if (rating==4)
    return("Outstanding")
}
categorical$PerformanceRating <- sapply(categorical$PerformanceRating,functrating)

## Work_Life_Balance 
functwlb <- function(wlb)
{
  if (wlb==1)
    return("Bad")
  else if (wlb==2)
    return("Good")
  else if (wlb==3)
    return("Better")
  else if (wlb==4)
    return("Best")
}
categorical$WorkLifeBalance <- sapply(categorical$WorkLifeBalance,functwlb)


categorical <- data.frame(sapply(categorical, function(x) factor(x)))
str(categorical)

# creating dummy for factor variable

dummies<- data.frame(sapply(categorical, 
function(x) data.frame(model.matrix(~x,data =categorical))[,-1]))

# here 2 level variable attrition "No" gender "F" and performance rating "3" are 0

# Final dataframe after prepration

emp_attrition <- cbind(emp_attrition, dummies)
str(emp_attrition) # containing all numeric data with 4300 obs of 56 variables

#-----------------------------------------------------------------------------------
# splitting data matrix into training and testing data set:

set.seed(100)

indices = sample.split(emp_attrition$Attrition, SplitRatio = 0.7)

train = emp_attrition[indices,]

test = emp_attrition[!(indices),]

#--------------------------
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
vif(model_2)
# removing education field life science as having high vif value
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_hours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High, 
               family = "binomial", data = train)
summary(model_3)
vif(model_3)
# removing work life balance -better
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_hours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High, 
               family = "binomial", data = train)
summary(model_4)
vif(model_4)
# removing travel frequently  as high vif
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_hours + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High, 
               family = "binomial", data = train)
summary(model_5)
vif(model_5)
# removing worklife balnce- best
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_hours + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High, 
               family = "binomial", data = train)
summary(model_6)
final_model <- model_25
# removing edu field- marketing -high pvalue
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_hours + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High, 
               family = "binomial", data = train)
summary(model_7)
# removing edu field- medical- high pvalue
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_hours + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High, 
               family = "binomial", data = train)
summary(model_8)
# removing work life balance-good
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Average_hours + BusinessTravel.xTravel_Rarely + 
                 Education.xDoctor + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + 
                 JobInvolvement.xMedium + JobInvolvement.xVery.High, 
               family = "binomial", data = train)
summary(model_9)
# removing stock option level -3 high pvalue
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + 
                  JobInvolvement.xMedium + JobInvolvement.xVery.High, 
                family = "binomial", data = train)
summary(model_10)
# removing involvement -medium - high pvalue
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + 
                  JobInvolvement.xVery.High, 
                family = "binomial", data = train)
summary(model_11)
# removing job involvement very high- high pvalue
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_12)
# removing edu field others- high pvalue
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_13)
# removing job level-5- high pavalue
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_14)
# removing edu field technical degree- high pvalue
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + 
                  JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_15)
# removing marital status married
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + 
                  JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_16)
# removing job role human resourse- high pvalue
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + JobLevel.x2 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_17)
# removing environment satisfaction - very high- high pvalue
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours + BusinessTravel.xTravel_Rarely + 
                  Education.xDoctor + JobLevel.x2 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_18)
# removing business travel rarely- high pvalue

model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  Education.xDoctor + JobLevel.x2 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_19)
# removig stock option level 1 - insig
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  Education.xDoctor + JobLevel.x2 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_20)
# removing job role- sales exe- insig
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  Education.xDoctor + JobLevel.x2 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_21)
# removing job role- research director- insig
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  Education.xDoctor + JobLevel.x2 + JobRole.xManager + 
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_22)
# removing edu - doc- insig
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  JobLevel.x2 + JobRole.xManager + 
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_23)
# removing job level-2 insig
model_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  JobRole.xManager + 
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_24)
# removing jobrole- manager - insig
model_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High , 
                family = "binomial", data = train)
summary(model_25)
# removing job satisfaction - very high
model_26 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow, 
                family = "binomial", data = train)
summary(model_26)
# removing job role manufacturing director
model_27 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Average_hours +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow, 
                family = "binomial", data = train)
summary(model_27)

final_model <- model_25
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-12])
summary(test_pred)
test$prob <- test_pred

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))



library(caret)
install.packages("ModelMetrics")
library(ModelMetrics)
?confusionMatrix
install.packages("recipes")
library(recipes)
test_conf <- caret::confusionMatrix(test_actual_attrition, test_pred_attrition, positive = "Yes")
test_conf

# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_attrition, test_actual_attrition,positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
#ceating matrix for cut of value .01 to .80 

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
# chosing 34% as the cutoff acc=83.1, sens=.61 spec=.873

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
# Let's choose a cutoff value of 0.16959 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1855, "Yes", "No"))

conf_final <- caret::confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

install.packages("ROCR")
library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 51%
# KS statistic shows how well  our model dscriminates bet two classes. it should be 
# equal to or greater than 40%

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

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

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

# graph for lift

graphics::plot(attrition_decile$bucket, attrition_decile$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")

# from the graph we can see the the cummulative lift is 2.7 for top 2 decile
# which means when selecting 20% of the records based on the model, we come accross 2.7 
# times more attrition based employee as compared to 20% covered by random model.

# gain table shows that 71% of the attrition employee are covered in 3rd decile or 30% data
