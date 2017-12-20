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
library(reshape2)
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


temp_df <- melt(emptimedf, id=c("EmployeeID"))
temp_df$month <- substr(temp_df$variable,7,8)

emp_avghours_pw <- aggregate(value~EmployeeID, temp_df, sum)
emp_avghours_pw$value <- emp_avghours_pw$value/52

emp_extraOffs <- aggregate(value~EmployeeID, temp_df[temp_df$value==0,], length)
colnames(emp_avghours_pw)[names(emp_avghours_pw) == "value"] = "avg_wrokhours_per_week"
colnames(emp_extraOffs)[names(emp_extraOffs) == "value"] = "Num_of_days_off"
metrics_emptime <- cbind(emp_avghours_pw,emp_extraOffs$Num_of_days_off)

employeeHr<- merge(employeeHr,metrics_emptime, by="EmployeeID", all = F)
employeeHr$overwork = ifelse(employeeHr$avg_wrokhours_per_week/40> 1, 1, 0) 
colnames(employeeHr)[names(employeeHr) == "emp_extraOffs$Num_of_days_off"] = "Num_of_days_off"
# Master dataset
View(employeeHr) 
length(names(employeeHr))

#Start of EDA
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

factor_Vars <- names(employeeHr)[sapply(employeeHr, class) == "factor"]

plot_grid(ggplot(employeeHr, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Over18,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v") 

plot_grid(ggplot(employeeHr, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v")  

plot_grid(ggplot(employeeHr, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

plot_grid(ggplot(employeeHr, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v") 
ggplot(employeeHr, aes(x=as.factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme1

ggplot(employeeHr, aes(x=Attrition,y=YearsWithCurrManager)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=YearsAtCompany)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=DistanceFromHome)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=Age)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=PercentSalaryHike)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=TotalWorkingYears)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=TrainingTimesLastYear)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=YearsSinceLastPromotion)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=avg_wrokhours_per_week)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=Num_of_days_off)) + geom_boxplot()
ggplot(employeeHr, aes(x=Attrition,y=NumCompaniesWorked)) + geom_boxplot()


ggplot(employeeHr, aes(x=avg_wrokhours_per_week)) + geom_histogram(bins = 40)
ggplot(employeeHr, aes(x=DistanceFromHome)) + geom_histogram(bins = 15)
ggplot(employeeHr, aes(x=PercentSalaryHike)) + geom_histogram(bins=15)
ggplot(employeeHr, aes(x=Age)) + geom_histogram(bins=25)
ggplot(employeeHr, aes(x=TrainingTimesLastYear)) + geom_histogram(bins=10)
ggplot(employeeHr, aes(x=YearsAtCompany)) + geom_histogram(bins=15)
