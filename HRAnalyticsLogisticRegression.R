setwd('C:\\Users\\putripat\\Downloads\\PA-I_Case_Study_HR_Analytics')

# Comment the next line
#setwd("D:/pgdds/Logistic Regression/LogisticRegressionCaseStudy")
##### Importing the necessary libraries #####
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(lubridate)

# Loading data file

employee_survey_data<-read.csv("employee_survey_data.csv")
general_data <- read.csv("general_data.csv")
in_time <- read.csv("in_time.csv")
manager_survey_data <- read.csv("manager_survey_data.csv")
out_time <- read.csv("out_time.csv")

intime <- in_time[,! apply( in_time , 2 , function(x) all(is.na(x)) )]
outtime <- out_time[,! apply( out_time , 2 , function(x) all(is.na(x)) )]

summary(employee_survey_data)
summary(general_data)
summary(in_time)
summary(manager_survey_data)
summary(out_time)

str(employee_survey_data)
str(general_data)
str(in_time)
str(manager_survey_data)
str(out_time)


# Collate the data together in one single file
length(unique(tolower(employee_survey_data$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(general_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(outtime$X))) # 4410, confirming X(EmployeeID) is key
length(unique(tolower(intime$X))) # 4410, confirming X(EmployeeID) is key

setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(employee_survey_data$EmployeeID,in_time$X) # Identical EmployeeID across these datasets
setdiff(employee_survey_data$EmployeeID,out_time$X) # Identical EmployeeID across these datasets

employeeHr<- merge(employee_survey_data,general_data, by="EmployeeID", all = F)
employeeHr<- merge(employeeHr,manager_survey_data, by="EmployeeID", all = F)

cols_with_na <- colnames(employeeHr)[colSums(is.na(employeeHr)) > 0]

cols_with_na
# EnvironmentSatisfaction|JobSatisfaction|WorkLifeBalance|NumCompaniesWorked|TotalWorkingYears
# Function for calculating Mode of a vector

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Fill NA values in these columns with Mode

employeeHr$EnvironmentSatisfaction[is.na(employeeHr$EnvironmentSatisfaction)] <- Mode(employeeHr$EnvironmentSatisfaction)
employeeHr$JobSatisfaction[is.na(employeeHr$JobSatisfaction)] <- Mode(employeeHr$JobSatisfaction)
employeeHr$WorkLifeBalance[is.na(employeeHr$WorkLifeBalance)] <- Mode(employeeHr$WorkLifeBalance)

# Find better strategy for these columns as they are not categorical
employeeHr$NumCompaniesWorked[is.na(employeeHr$NumCompaniesWorked)] <- median(employeeHr$NumCompaniesWorked, na.rm = TRUE)
employeeHr$TotalWorkingYears[is.na(employeeHr$TotalWorkingYears)] <- median(employeeHr$TotalWorkingYears, na.rm = TRUE)

# Working with time datasets
intime <- in_time[,!apply(is.na(in_time), 2, all)]
outtime <- out_time[,!apply(is.na(out_time), 2, all)]

intime[,2:250]<-sapply(intime[,2:250], function(x) parse_date_time(x , "YmdHMS"))
outtime[,2:250]<-sapply(outtime[,2:250], function(x) parse_date_time(x , "YmdHMS"))
emptimedf <- as.data.frame( cbind(intime$X, sapply(outtime[,2:250] - intime[,2:250], function(x) x/60/60)))
# Let's put all NA as 0 in emp time dataframe
emptimedf[, 2:250][is.na(emptimedf[, 2:250])] <- 0
colnames(emptimedf)[names(emptimedf) == "V1"] = "EmployeeID"
str(emptimedf)

View(employeeHr) #semi master file
