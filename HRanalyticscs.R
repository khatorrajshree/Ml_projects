# loading in_time data
in.time <- read.csv("in_time.csv", stringsAsFactors = F)
str(in.time)


install.packages("lubridate")
library(lubridate)


#str(in.time_1)
intime <- in.time$X
in.time <- in.time[, -1]
in.time_1 <- in.time
# converting character date to standard date format:
in.time_1[] <- lapply(in.time, function(x) {parse_date_time(x, "Ymd HMS")})
str(in.time_1)

in.time <- cbind(intime, in.time_1)
names(in.time)[1] <- "EmployeeID"

# loading out time file
out.time <- read.csv("out_time.csv", stringsAsFactors = F)
outime <- out.time[,1]
out.time <- out.time[,-1]
out.time_1 <- out.time
# converting character date to standard date format:
out.time_1[] <- lapply(out.time, function(x) {parse_date_time(x, "Ymd HMS")})
str(in.time_1)
out.time <- cbind(outime, out.time_1)
names(out.time)[1] <- "EmployeeID"

#merging in time and out time
install.packages("tidyr")
library(tidyr)


inouttime <- merge(in.time, out.time, by = "EmployeeID")

str(inouttime)
#abc <- as.integer(difftime(inouttime[,264], inouttime[, 3], units = "auto"))
#abc



EmployeeID <- inouttime[,1]
inouttime_1 <- inouttime[,-1]
# calculating working hrs for each employee for each day:
for(i in 1:261){
  inouttime_1[,i] <- as.integer(difftime(inouttime_1[, i+261], inouttime_1[, i]))
}
inouttime_1 <- inouttime_1[, 1:261]
inoutfinal <- cbind(EmployeeID, inouttime_1)
# calculating mean working hr for each employee for year 2015
inoutfinal$Averageworkhr <- as.integer(rowMeans(inoutfinal,na.rm = T))

# loading employee id and avg working hr of employee in time data frame
time <- inoutfinal[,c(1, 263)]
str(time)
table(time$Averageworkhr)

# loading manager_survey_data employee_survey_data and general_data

manager.data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
employee.data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general.data <- read.csv("general_data.csv", stringsAsFactors = F)

str(manager.data)
str(employee.data)
str(general.data)

empmanager.data <- merge(employee.data, manager.data, by = "EmployeeID")
empmanagertime.data <- merge(empmanager.data, time, by = "EmployeeID")
    
emp.attrition <- merge(general.data, empmanagertime.data, by = "EmployeeID")
table(emp.attrition[,30])
