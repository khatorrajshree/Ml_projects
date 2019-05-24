#The aim of analysis is to identify the root cause of the problem (i.e. cancellation 
#and non-availability of cars) and recommend ways to improve the situation.

# loading uber request data in R.

uber <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)

# have glimpse of data using str()---UNDERSTANDING DATA

str(uber)
#--The data frame uber consist of 6745 obs. of 6 variables.

# DATA CLEANING

# 1. Finding any duplicate values using duplicated() function

sum(duplicated(uber$Request.id)) # No duplicate values

# 2. Checking NA values

sum(is.na(uber)) # containing 6564 NA values

# 3. Checking missing values

sapply(uber, function(x) length(which(x == ""))) # No missing values.

# 4. Correcting date and time to the correct format
# removing "-" by "/"
uber$Request.timestamp <- gsub("-", "/", uber$Request.timestamp)
uber$Drop.timestamp <- gsub("-", "/", uber$Drop.timestamp)

#---Converting date and time to standard format using lubridate()

install.packages("lubridate")
library(lubridate)
uber$Request.timestamp <- parse_date_time(uber$Request.timestamp, c("dmY HM", "dmY HMS"))
uber$Drop.timestamp <- parse_date_time(uber$Drop.timestamp, c("dmY HM", "dmY HMS"))

#---------------------------------------------------------------------------

# Creating new columns like date, request hour, weekdays etc for further analysis

uber$request_date <- format(uber$Request.timestamp, "%d")
uber$request_hour <- format(uber$Request.timestamp, "%H")
uber$day <- weekdays(uber$Request.timestamp)

#-----------------------------

install.packages("ggplot2")
library(ggplot2)
# ----------------------Problem statement 1-------------------------------

# 1. Visually identifing the most pressing problems for Uber. 

#------Univariate analysis:

ggplot(uber, aes(x = factor(Status))) + geom_bar()
# From this plot we can see that some trips have been cancelled, lots of trips 
# have shown the status as  "No cars available" nearly equal to "trip completeted". 

ggplot(uber, aes(x = factor(Pickup.point))) + geom_bar()
# From this plot we can see there are more requests made from city then airport.

ggplot(uber, aes(x= request_hour)) + geom_bar()
# From here we can see that request is comparitively  more at time "5 am to 9 am " 
# and " 5 pm to 9 pm".

ggplot(uber, aes(x= day)) + geom_bar()
# This plot shows that request for cabs are slightly more on Friday and Monday
# and comparitively less for Tuesday.

#--------------------------------------------------------------------------------
#-------Bivariate analysis-----------------

ggplot(uber, aes(x= factor(Status), fill = factor(Pickup.point))) + geom_bar() + labs(x="Status", y= "Number of Requests", fill = "Pickup point")
# From this plot we can see that if request is made from  airport then "No cars available"
# status is shown more. and if request is made from city then "Cancelled" Status is
# Shown more.


# We need to know at what time does the status " no car available" and "cancelled"
# shown more for airport and city:

# Let us see some other variations :

ggplot(uber, aes(x =request_hour, fill = factor(Pickup.point))) + geom_bar(position = "dodge") + labs(x="Request Hours", y = "No. of Requests", fill = "Pickup Point")
# Here we can see that from "5 am to 9 am" more request from city is made and
# from " 5pm to 9 pm" request from airport is made.

ggplot(uber, aes(x= request_hour, fill = factor(Status))) + geom_bar(position = "dodge") + labs(x="Request Hours", y = "No. of Requests", fill = "Status")
# Here we can see that from " 5 am to 9 am" more "cancelled" status is shown and
# from " 5 pm to 9 pm " more " No cars Available" Status is shown.

ggplot(uber,aes(x=factor(Pickup.point),fill=factor(Status)))+geom_bar(position = "dodge")+facet_wrap(~request_hour)+ labs(x= "Pickup point", y= "No. of Requests", fill = " Status")
# This graph is the combination of above two graphs showing Status and pickup for each hour.

#-----------------------------------------------------------
# Let us see the date wise segregation:

ggplot(uber, aes(x = request_hour, fill = factor(Status)))+geom_bar(position = "dodge") + facet_wrap( ~ uber$request_date, nrow =5, ncol = 1) + labs(x = "Hour", y = "No. of Requests", fill = "Status" )
# Here for all days graphs are almost same showing showing "cancelled" status for 
# morning and "No cars available" for evening. 

ggplot(uber, aes(x = request_hour,fill = factor(Pickup.point)))+geom_bar(position = "dodge") + facet_wrap( ~ uber$request_date, nrow =5, ncol = 1) + labs(x = "Hour", y = "No. of Requests", fill = "Pickup point" )
# Here for all days graphs is same showing in morning more request is made from city
# and in evening more request is made from airport.

# From the graphs above we can conclude that for each day from " 5am to 9am " if the request is made 
# from city then it is showing "cancelled" status and from "5pm to 9pm" if request is made from airport
# then it is showing " No cars available" status.
#------------------------------------------------------------------------------
# we need to figure out why this is happening:
summary(uber$request_hour) # showing character variable

uber$request_hour <- as.numeric(uber$request_hour)

uber$time_seg <- ifelse(uber$request_hour < 3, "Late_Night",ifelse(uber$request_hour < 5, "Early_Morning", ifelse(uber$request_hour < 12,"Morning",ifelse(uber$request_hour < 17,"Afternoon",ifelse(uber$request_hour < 22,"Evening","Night")))))

#-----------------------------------------------------------------------------
#------------------------------Problem statement 2----------------------------

# 2. Find out the gap between supply and demand and show the same using plots.

# let us plot a graph showing time segment and Status: 
ggplot(uber, aes(x = as.factor(time_seg), fill= factor(Status))) + geom_bar(position = "dodge")+labs(x = "Time Segment", y = "Number of Requests", fill = "Status" )

# 1. As we can see "cancelled" status in morning is more:

uber1 <- subset(uber,time_seg=="Morning")
# In morning cancelled status is high for city or airport?
ggplot(uber1, aes(x = factor(uber1$Pickup.point), fill= factor(uber1$Status))) + geom_bar(position = "fill") +labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )
# as we can see that cancelled status is high for city.

# let us see how much:
# Total Demand from city is:
dcity <- nrow(subset(uber1, uber1$Pickup.point == "City"))

# supply to city
tcity <- nrow(subset(uber1, uber1$Pickup.point == "City" & uber1$Status == "Trip Completed"))

tcity/dcity*100 # only 30% of the request in city is fulfilled

# This supply and demand gap in the morning in city may be due to - as there may not 
# be enough drivers to meet the demand therefore trips are cancelled.

# 2. As we can see "No cars Available" status in evening is more: 

uber2 <- subset(uber,time_seg=="Evening")
# In evening "No cars available" status is high for city or airport?
ggplot(uber2, aes(x = factor(uber2$Pickup.point), fill= factor(uber2$Status))) + geom_bar(position = "fill") +labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )
# as we can see that "no cars available" status is high for airport.

# let us see how much:
# Total Demand from airport is:
dairport <- nrow(subset(uber2, uber2$Pickup.point == "Airport"))

# Supply to airport:
tairport <- nrow(subset(uber2, uber2$Pickup.point == "Airport" & uber2$Status == "Trip Completed"))

tairport/dairport*100 # only 20% request in airport is fulfilled

# This supply and demand gap in evening in airport may be due to- there may not be
# enough flight taking off therefore cars must not be available from airport.

# --------------Solution to Uber Supply and demand problem-----------------
# 1. Creating Incentive programs for particular time slots:
# Morning Incentive:  Drivers should be given incentive equal to cost of airport customers (More no. of rides in city leads to more pay)
# Evening Incentives: Some Airport ride special offers to be created.

# 2. More strict cancellation policy should be created. More penalty for time 5am to 9am for cancellation from city should be included.

# 3. Bring in more cars and drivers(This may be a costly solution)

# 4. Introduce panelty for late customers..This might save time of drivers.

write.csv(uber, file = "uber.csv", row.names=FALSE)
# Important note-

# 1. For all the plots, Bar graph is chosen because for categorical variable only 
# it is always better to use bar charts.