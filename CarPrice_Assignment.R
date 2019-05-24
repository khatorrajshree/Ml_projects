cars <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)
dim(cars)
str(cars)
head(cars)
# Checking NA values
sum(is.na(cars))
# checkin duplicate values
a <- unique(cars)
a
# No duplicate values
cars[cars==""] <- NA 
# No missing values
install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
cars <- cars %>% separate(CarName, c("Company"), sep = ' ', extra = "drop", fill = "right")

table(cars$Company)
cars[which(cars$Company == "maxda"), 3] <- "mazda" 
cars[which(cars$Company == "Nissan"), 3] <- "nissan"
cars[which(cars$Company == "porcshce"), 3] <- "porsche"
cars[which(cars$Company == "toyouta"), 3] <- "toyota"
cars[which(cars$Company == "vokswagen"), 3] <- "volkswagen"
cars[which(cars$Company == "vw"), 3] <- "volkswagen"

table(cars$drivewheel)
# here drivewheel contains same values in diffent name as 4wd and fwd -
# Combining 4wd and fwd
cars[which(cars$drivewheel == "4wd"), 8] <- "fwd"
ggplot(cars, aes(x=factor(symboling))) + geom_bar()
ggplot(cars, aes(x=factor(Company))) + geom_bar()
ggplot(cars, aes(x=factor(fueltype))) + geom_bar()
ggplot(cars, aes(x=factor(aspiration))) + geom_bar()
ggplot(cars, aes(x=factor(doornumber))) + geom_bar()
ggplot(cars, aes(x=factor(carbody))) + geom_bar()
ggplot(cars, aes(x=factor(drivewheel))) + geom_bar()
ggplot(cars, aes(x=factor(enginelocation))) + geom_bar()
ggplot(cars, aes(x=wheelbase)) + geom_histogram()
ggplot(cars, aes(x=carlength)) + geom_histogram()
ggplot(cars, aes(x=factor(enginetype))) + geom_bar()
ggplot(cars, aes(x=factor(cylindernumber))) + geom_bar()
ggplot(cars, aes(x=factor(fuelsystem))) + geom_bar()
ggplot(cars, aes(x= factor(symboling), y = price, fill = factor(Company))) + geom_bar(stat = "identity", position = "dodge")
#ggplot(cars, aes(x= factor(symboling), y = price, fill = factor(fueltype))) + geom_bar(stat = "identity", position = "dodge")
ggplot(cars, aes(x= factor(symboling), y = price, fill = factor(enginetype))) + geom_bar(stat = "identity", position = "dodge")
ggplot(cars, aes(x= factor(symboling), y = price, fill = factor(fuelsystem))) + geom_bar(stat = "identity", position = "dodge")
ggplot(cars, aes(x= factor(symboling), y = price, fill = factor(carbody))) + geom_bar(stat = "identity", position = "dodge")
ggplot(cars, aes(x= factor(Company), y= price, fill = factor(fueltype))) + geom_bar(stat = "identity", position = "dodge")
ggplot(cars, aes(x= factor(Company), y= price, fill = factor(aspiration))) + geom_bar(stat = "identity", position = "dodge")
ggplot(cars, aes(x= factor(Company), y= price, fill = factor(enginetype))) + geom_bar(stat = "identity", position = "dodge")
ggplot(cars, aes(x= factor(Company), y= price, fill = factor(carbody))) + geom_bar(stat = "identity", position = "dodge")

ggplot(cars, aes(x= wheelbase, y= price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= carlength, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= carwidth, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= carheight, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= curbweight, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= enginesize, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= boreratio, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= stroke, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= compressionratio, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= horsepower, y = price)) + geom_point() + geom_smooth()
ggplot(cars, aes(x= peakrpm, y = price)) + geom_point() + geom_smooth()

install.packages("corrplot")
library(corrplot)

#---------------------------------------------------------------------------
# convering 2 level categorical variable to numeric
table(cars$fueltype)

cars$fueltype <- as.integer(cars$fueltype=="gas")

table(cars$aspiration)

cars$aspiration <- as.integer(cars$aspiration=="std")

table(cars$doornumber)
cars$doornumber <- as.integer(cars$doornumber=="four")

#table(cars$carbody)
table(cars$drivewheel)
cars$drivewheel <- as.integer(cars$drivewheel=="fwd")

table(cars$enginelocation)
cars$enginelocation <- as.integer(cars$enginelocation=="front")

x <- cars[, c(2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 17, 19, 20, 21, 22, 23, 24, 25, 26)]
m <- cor(x)
corrplot(m, method = "circle")

#table(cars$enginetype)

#table(cars$cylindernumber)

#table(cars$fuelsystem)
# converting multilevel categorical variable to numeric

dummy_1 <- data.frame(model.matrix( ~Company, data = cars))
View(dummy_1)
dummy_1 <- dummy_1[,-1]
cars_1 <- cbind(cars[,-3], dummy_1)

cars_1$symboling <- as.character(cars_1$symboling)

dummy_2 <- data.frame(model.matrix( ~symboling, data = cars_1))
View(dummy_2)
dummy_2 <- dummy_2[,-1]
cars_2 <- cbind(cars_1[,-2], dummy_2)

dummy_3 <- data.frame(model.matrix( ~carbody, data = cars_2))
View(dummy_3)
dummy_3 <- dummy_3[,-1]
cars_3 <- cbind(cars_2[,-5], dummy_3)

dummy_4 <- data.frame(model.matrix( ~enginetype, data = cars_3))
View(dummy_4)
dummy_4 <- dummy_4[,-1]
cars_4 <- cbind(cars_3[,-12], dummy_4)

dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = cars_4))
View(dummy_5)
dummy_5 <- dummy_5[,-1]
cars_5 <- cbind(cars_4[,-12], dummy_5)

dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = cars_5))
View(dummy_6)
dummy_6 <- dummy_6[,-1]
cars_6 <- cbind(cars_5[,-13], dummy_6)

str(cars_6)
#----------------------------------------------------------------------------
#m1 <- cor(cars_6)
#corrplot(m1, method = "circle")

# derived variables

quantile(cars_6$horsepower, seq(0,1,.01))
cars_6$horsepower[which(cars_6$horsepower>184.00)]<-184.00
cars_6$carvol <- cars_6$carlength * cars_6$carwidth * cars_6$carheight
cars_6$cararea <- cars_6$carlength * cars_6$carwidth
cars_6 <- cars_6[,-1]
set.seed(100)
trainindices= sample(1:nrow(cars_6), 0.7*nrow(cars_6))
train = cars_6[trainindices,]
test = cars_6[-trainindices,]


# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
install.packages("MASS")
library(MASS)
install.packages("car")
library(car)
# We have a total of 15 variables considered into the model 
#Now let;s run the code. 

step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carvol + 
                cararea + cylindernumberfive, data = train)
summary(model_2)

vif(model_2)
#removing cylindernumberfive
model_3 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carvol + 
                cararea, data = train)

summary(model_3)

vif(model_3)
# removing carvol
model_4 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi +
                cararea, data = train)

summary(model_4)

vif(model_4)
# removing fuel system spdi
model_5 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi +
                cararea, data = train)

summary(model_5)
vif(model_5)

# removing fuelsystemmpfi

model_6 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl +
                cararea, data = train)
summary(model_6)
vif(model_6)

#removing car height

model_7 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl +
                cararea, data = train)

summary(model_7)
vif(model_7)

# removing compression ratio

model_8 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl +
                cararea, data = train)

summary(model_8)
vif(model_8)
# removing fuel system 2bbl

model_9 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor +
                cararea, data = train)

summary(model_9)
vif(model_9)

# removing enginelocation

model_10 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor +
                 cararea, data = train)

summary(model_10)
vif(model_10)

# removing carhatchback

model_11 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo + carbodyhardtop + 
                 carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor +
                 cararea, data = train)

summary(model_11)
vif(model_11)

# removing carhardtop

model_12 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor +
                 cararea, data = train)
summary(model_12)
vif(model_12)

# removing carbodysedan

model_13 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 carbodywagon + enginetypeohcv + enginetyperotor +
                 cararea, data = train)
summary(model_13)
vif(model_13)

#removing carbody wagon

model_14 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetypeohcv + enginetyperotor +
                 cararea, data = train)
summary(model_14)
vif(model_14)

# removing car area

model_15 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetypeohcv + enginetyperotor, data = train)

summary(model_15)
vif(model_15)

# suppliment model
#model_15 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
 #                wheelbase + carwidth + curbweight + 
  #               enginesize + stroke + peakrpm + Companybmw + 
   #              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
    #             Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
     #            Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
      #           Companyrenault + Companysaab + Companysubaru + Companytoyota + 
       #          Companyvolkswagen + Companyvolvo +  
        #         enginetypeohcv + enginetyperotor +
         #       cararea, data = train)

# removing carwidth
 
model_16 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetypeohcv + enginetyperotor, data = train)

summary(model_16)
vif(model_16)

# removing enginetypeohcv

  
model_17 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_17)
vif(model_17)

# removing car length

model_18 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_18)

#removing wheelbase

model_19 <- lm(formula = price ~ car_ID + aspiration + drivewheel + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_19)

vif(model_19)
# removing company buick

model_20 <- lm(formula = price ~ car_ID + aspiration + drivewheel + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)

summary(model_20)
vif(model_20)
# checking alternate model
# removing car_ID
model_21 <- lm(formula = price ~ aspiration + drivewheel + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_21)
vif(model_21)

model_21 <- lm(formula = price ~ car_ID + aspiration + drivewheel + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolvo +  
                 enginetyperotor, data = train)
# removing company chivrolet

model_22 <- lm(formula = price ~ aspiration + drivewheel + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_22)
# removing peakrpm
model_23 <- lm(formula = price ~ aspiration + drivewheel + curbweight + 
                 enginesize + stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_23)
vif(model_23)
# trying alternate model
# removing curbweight
model_24 <- lm(formula = price ~ aspiration + drivewheel + 
                 enginesize + stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_24)
vif(model_24)
# removing engine size
model_24 <- lm(formula = price ~ aspiration + drivewheel + curbweight + 
                 stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
# cannot remove engine size as it reduces adj r sqr to a large extent
#checking alternate variable
# removing drive wheel

model_25<- lm(formula = price ~ aspiration + 
                enginesize + stroke + Companybmw + 
                Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo +  
                enginetyperotor, data = train)
summary(model_25)
vif(model_25)

model_25 <- lm(formula = price ~ aspiration + drivewheel + 
                 stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)

# cannot remove engine size as it reduces adj r sqr to great extent
# removing company bmw
model_26 <- lm(formula = price ~ aspiration + 
                 enginesize + stroke + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_26)
# removing company saab
model_27 <- lm(formula = price ~ aspiration + 
                 enginesize + stroke + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_27)
# removing company mercury
model_28 <- lm(formula = price ~ aspiration + 
                 enginesize + stroke + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +  
                 enginetyperotor, data = train)
summary(model_28)
# removing company volvo
model_29 <- lm(formula = price ~ aspiration + 
                 enginesize + stroke + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysubaru + Companytoyota + 
                 Companyvolkswagen + 
                 enginetyperotor, data = train)
summary(model_29)
#removing stroke
model_30 <- lm(formula = price ~ aspiration + 
                 enginesize +
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                 Companyrenault + Companysubaru + Companytoyota + 
                 Companyvolkswagen + 
                 enginetyperotor, data = train)
summary(model_30)

#-----------------new model building------------------------------------

model_2 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carvol + 
                cararea + cylindernumberfive, data = train)
summary(model_2)

vif(model_2)

# removing car height as it has high vif and high p value
model_3 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carvol + 
                cararea + cylindernumberfive, data = train)

summary(model_3)
vif(model_3)
# removing car vol as it has high vif and pvalue

model_4 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi +
                cararea + cylindernumberfive, data = train)

summary(model_4)
vif(model_4)

# removing carbody sedan
model_5 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi +
                cararea + cylindernumberfive, data = train)

summary(model_5)
vif(model_5)

# removing fuel systemmpfi
model_6 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemspdi +
                cararea + cylindernumberfive, data = train)
summary(model_6)
vif(model_6)
# removing system2bbl
model_7 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetypeohcv + enginetyperotor + 
                fuelsystemspdi +
                cararea + cylindernumberfive, data = train)
summary(model_7)
vif(model_7)
# removing ohcv
#model_8 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
 #               wheelbase + carlength + carwidth + curbweight + 
  #              enginesize + stroke + compressionratio + peakrpm + Companybmw + 
   #             Companychevrolet + Companydodge + Companyhonda + 
    #            Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
     #           Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
      #          Companyrenault + Companysaab + Companysubaru + Companytoyota + 
       #         Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
        #        carbodywagon + enginetypeohcv + enginetyperotor + 
         #       fuelsystemspdi +
          #      cararea + cylindernumberfive, data = train)
summary(model_8)
vif(model_8)
model_8 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companybuick + Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetyperotor + 
                fuelsystemspdi +
                cararea + cylindernumberfive, data = train)
# removing company buick
model_9 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                wheelbase + carlength + carwidth + curbweight + 
                enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                Companychevrolet + Companydodge + Companyhonda + 
                Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                Companynissan + Companypeugeot + Companyplymouth + Companyporsche + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetyperotor + 
                fuelsystemspdi +
                cararea + cylindernumberfive, data = train)
summary(model_9)
vif(model_9)

# removing car porche
model_10 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetyperotor + 
                 fuelsystemspdi +
                 cararea + cylindernumberfive, data = train)
summary(model_10)
vif(model_10)

# removing carbody hardtop

model_11 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + compressionratio + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo + carbodyhatchback + 
                 carbodywagon + enginetyperotor + 
                 fuelsystemspdi +
                 cararea + cylindernumberfive, data = train)

summary(model_11)
vif(model_11)

# removing compression ratio

model_12 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo + carbodyhatchback + 
                 carbodywagon + enginetyperotor + 
                 fuelsystemspdi +
                 cararea + cylindernumberfive, data = train)

summary(model_12)

# removing fuelsystemspdi
model_13 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo + carbodyhatchback + 
                 carbodywagon + enginetyperotor + cararea + cylindernumberfive, data = train)
summary(model_13)

# removing carbodyhatchback

model_14 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 carbodywagon + enginetyperotor + cararea + cylindernumberfive, data = train)
summary(model_14)

# removing carbodywagon

model_15 <- lm(formula = price ~ car_ID + aspiration + drivewheel + enginelocation + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor + cararea + cylindernumberfive, data = train)
summary(model_15)

# removing enginlocation
model_16 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor + cararea + cylindernumberfive, data = train)

summary(model_16)
vif(model_16)
# creating alternate model
# removing area
model_17 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carlength + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor + cylindernumberfive, data = train)

summary(model_17)
vif(model_17)

# removing length
model_18 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + carwidth + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor + cylindernumberfive, data = train)
summary(model_18)
vif(model_18)
# removing car width
model_19 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 wheelbase + curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor + cylindernumberfive, data = train)

summary(model_19)
vif(model_19)

# removing wheelbase

model_20 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor + cylindernumberfive, data = train)
summary(model_20)

# removing cylindernumberfive
model_21 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companychevrolet + Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor , data = train)
summary(model_21)
# removing company chevrolet

model_22 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 curbweight + 
                 enginesize + stroke + peakrpm + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor , data = train)
summary(model_22)
vif(model_22)

## removing peakrm
model_23 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 curbweight + 
                 enginesize + stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor , data = train)
summary(model_23)
vif(model_23)


# removing drive wheel
model_24 <- lm(formula = price ~ car_ID + aspiration + 
                 curbweight + 
                 enginesize + stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor , data = train)
summary(model_24)
vif(model_24)
# removing curb weight

model_25 <- lm(formula = price ~ car_ID + aspiration + 
                 enginesize + stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor , data = train)
summary(model_25)
vif(model_25)
# alternate removal
# #removing toyota
model_26 <- lm(formula = price ~ car_ID + aspiration + 
                 enginesize + stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor , data = train)
summary(model_26)
vif(model_26)
# #removing volswagan
model_26 <- lm(formula = price ~ car_ID + aspiration + 
                 enginesize + stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolvo +
                 enginetyperotor , data = train)
# removing car id
model_26 <- lm(formula = price ~ aspiration + 
                 enginesize + stroke + Companybmw + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor , data = train)

summary(model_26)
vif(model_26)


# #removing volvo
model_26 <- lm(formula = price ~ car_ID + aspiration + 
  enginesize + stroke + Companybmw + 
  Companydodge + Companyhonda + 
  Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
  Companynissan + Companypeugeot + Companyplymouth + 
  Companyrenault + Companysaab + Companysubaru + Companytoyota + 
  Companyvolkswagen +
  enginetyperotor , data = train)
summary(model_26)
vif(model_26)


# removing bmw
model_27 <- lm(formula = price ~ aspiration + 
                 enginesize + stroke + 
                 Companydodge + Companyhonda + 
                 Companyisuzu + Companymazda + Companymercury + Companymitsubishi + 
                 Companynissan + Companypeugeot + Companyplymouth + 
                 Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                 Companyvolkswagen + Companyvolvo +
                 enginetyperotor , data = train)
summary(model_27)
vif(model_27)
Predict_1 <- predict(model_25,test[,-20])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
