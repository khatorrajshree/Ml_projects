#-------------------------SVM [0-9] digit recognization---------------------------------#
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 RBF Kernel
# 5 Hyperparameter tuning and cross validation

# 1. Business understanding

# We have been given images of a digit submitted by a user via a scanner, a tablet, or other digital devices. The goal is to
# develop a model that can correctly identify the digit (between 0-9) written in an image. 
#--------------------------------------------------------------------------------------

# 2. Data understanding
# data consists of 
# train data - 59999 observations of 785 variables
# test data -  9999 observations of 785 variables
#--------------------------------------------------------------------------------------

# 3. Data preperation

# loading reqired libraries
install.packages("doParallel")
library(doParallel)
install.packages("caret")
library(caret)
install.packages("kernlab")
library(kernlab)
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

# loading train data into digit dataframeand test data into testdigit 

digit <- read_csv("mnist_train.csv")
testdigit <- read_csv("mnist_test.csv")


dim(digit) # dimensions of data- 59999*785
dim(testdigit) # dimensions of test data- 9999*785

str(digit) # structure of data
str(testdigit) # structure of test data

summary(digit) # summary of data
summary(testdigit) # summary of test data

# checking for missing values

sapply(digit, function(x) sum(is.na(x))) # no missing values in data
sapply(testdigit, function(x) sum(is.na(x))) # no missing values in test data

# naming 1st column in test and train data

colnames(digit) <- c(1:785)
colnames(digit)[1] <- "digits"
colnames(testdigit) <- c(1:785)
colnames(testdigit)[1] <- "digits"


# as we checked the structure of data and we found that the target/response variable 
# column 1 - digits is num, we neet to convert it into factors

digit$digits <- factor(digit$digits)
testdigit$digits <- factor(testdigit$digits)

#------finding values of C and sigma by trying model building  on 5% data----------

# let us first try on some 5% of the train data
set.seed(1)
train.indices = sample(1:nrow(digit), 0.05*nrow(digit))
traindigit = digit[train.indices, ]

# 4. Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(digits~ ., data = traindigit, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, testdigit)

#confusion matrix - Linear Kernel
caret::confusionMatrix(Eval_linear,testdigit$digits)

# model gives accuracy of around 93%

#Using RBF Kernel
Model_RBF <- ksvm(digits~ ., data = traindigit, scale = FALSE, kernel = "rbfdot")
print(Model_RBF)
# from here we can say that our default sigma is 1e-07 and c=1..and now we can range 
# values by +/- accordingly

Eval_RBF<- predict(Model_RBF, testdigit)

#confusion matrix - RBF Kernel
caret::confusionMatrix(Eval_RBF,testdigit$digits)

# accuracy = 95%
# sensitivity = 95%
# specificity = 99%

# As RBF model fiving better accuracy than linear model therefore choosing RBF model.

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 3 implies Number of folds in CV.

trainControl <- caret::trainControl(method="cv", number=3, allowParallel = TRUE)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
# let us choose sigma to be 1e-09, 1e-08, 1e-07, and c= .5, 1,2
grid <- expand.grid(.sigma=c(.000000001, .00000001, .0000001), .C=c(0.5,1,2) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

library(doParallel) # for making our processing faster
cl <- makeCluster(detectCores())
registerDoParallel(cl)

fit.svm <- caret::train(digits~., data=traindigit, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)
stopCluster(cl)
# here model shows good accuracy of 93% with sigma =1e-07 and c=2
#-------------------------------------
# let us check for other sigma values and c values

set.seed(7)
grid <- expand.grid(.sigma=c(1e-07,2e-07 ,3e-07 ), .C=c(1,2,3) )

cl <- makeCluster(detectCores())
registerDoParallel(cl)
fit.svm <- caret::train(digits~., data=traindigit, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

print(fit.svm)
stopCluster(cl)
# we can see accurfacy of model is 94% at sigma = 3e-07 and c=2

# let us try another set of c snd sigma

set.seed(7)
grid <- expand.grid(.sigma=c(3e-07, 4e-07, 5e-07, 6e-07, 7e-07, 8e-07, 1e-06 ), .C=c(2,3,4) )

cl <- makeCluster(detectCores())
registerDoParallel(cl)
fit.svm <- caret::train(digits~., data=traindigit, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

print(fit.svm)
stopCluster(cl)
plot(fit.svm)
# finally choosing accuracy of 94% and sigma = 4e-07 and C = 2.
# checking acuracy for sigma 3e-07,4e-07,5e-07,6e-07 for c=2,3,4 for 15% data
-----------------------------------------------------
# considering only 15% data for model building as data is very large

set.seed(1)
train.indices = sample(1:nrow(digit), 0.15*nrow(digit))
traindigit = digit[train.indices, ]

#Using RBF Kernel
Model_RBF <- ksvm(digits~ ., data = traindigit, scale = FALSE, kernel = "rbfdot")
print(Model_RBF)# c=1, sigma=1e-07

Eval_RBF<- predict(Model_RBF, testdigit)

#confusion matrix - RBF Kernel
caret::confusionMatrix(Eval_RBF,testdigit$digits)
# accuracy = 95%
# sensitivity = 95%
# specificity = 99%

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 4 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=4, allowParallel = TRUE)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=c(3e-07,4e-07,5e-07), .C=c(1,2,3) )
# setting sigma=3e-07,4e-07,5e-07, and c=1,2,3

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

fit.svm <- train(digits~., data=traindigit, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)
stopCluster(cl)
plot(fit.svm)
# accuracy of the model is 96.4% with sigma = 5e-07 and c=3

# let us check the model accuracy on test data

evaluate_data <- predict(fit.svm, testdigit)
caret::confusionMatrix(evaluate_data, testdigit$digits)

# accuracy : 96.9%
# sensitivity : 96.8%
# specificity : 99.6%

# our model correctly identified the digits 0-9 with accuracy of 96.9% on test data.