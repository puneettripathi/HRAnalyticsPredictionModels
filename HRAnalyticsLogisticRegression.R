setwd('C:\\Users\\putripat\\Downloads\\PA-I_Case_Study_HR_Analytics')

employee_survey_data <- read.csv('employee_survey_data.csv')
general_data <- read.csv("general_data.csv")
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv")

intime <- in_time[,!apply(is.na(in_time), 2, all)]
outtime <- out_time[,!apply(is.na(out_time), 2, all)]

intime <- in_time[,!apply(is.na(in_time), 2, all)]
outtime <- out_time[,!apply(is.na(out_time), 2, all)]


#cols <- colnames(intime)
#colnames(intime) <- gsub("X","Y", cols)
names(intime)[names(intime) == "X"] <- "EmployeeID"
names(outtime)[names(outtime) == "X"] <- "EmployeeID"
#merge(intime,outtime,by=c("EmployeeID"))
#intime[ , 2:250] - outtime[ , 2:250] 

library(lubridate)

intime[,2:250]<-sapply(intime[,2:250], function(x) parse_date_time(x , "YmdHMS"))
outtime[,2:250]<-sapply(outtime[,2:250], function(x) parse_date_time(x , "YmdHMS"))
emptimedf <- as.data.frame( cbind(intime$EmployeeID, sapply(outtime[,2:250] - intime[,2:250], function(x) x/60/60/8)))
str(emptimedf)
colnames(emptimedf)[names(emptimedf) == "V1"] = "EmployeeID"

empdf <- merge(employee_survey_data,general_data,by=c("EmployeeID"))
empdf <- merge(empdf,manager_survey_data,by=c("EmployeeID"))
empdf <- merge(empdf,emptimedf,by=c("EmployeeID"))
strdf = data.frame(str(empdf))
unique(empdf$Education)
unique(empdf$EducationField)
unique(empdf$DistanceFromHome)
