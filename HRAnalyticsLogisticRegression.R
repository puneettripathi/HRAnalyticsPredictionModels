# Comment the next line
setwd("D:/pgdds/Logistic Regression/LogisticRegressionCaseStudy")
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
library(outliers)

##### Importing CSV data to DataFrames #####
employee_survey_data<-read.csv("employee_survey_data.csv")
general_data <- read.csv("general_data.csv")
in_time <- read.csv("in_time.csv")
manager_survey_data <- read.csv("manager_survey_data.csv")
out_time <- read.csv("out_time.csv")

# Fixing NA in intime and outtime
intime <- in_time[,! apply( in_time , 2 , function(x) all(is.na(x)) )]
outtime <- out_time[,! apply( out_time , 2 , function(x) all(is.na(x)) )]

# Checking summary statistics on dataframes
summary(employee_survey_data)
summary(general_data)
summary(in_time)
summary(manager_survey_data)
summary(out_time)

# Checking structures of dataframes
str(employee_survey_data)
str(general_data)
str(in_time)
str(manager_survey_data)
str(out_time)


# Checking if there is duplicate data on the tables 
length(unique(tolower(employee_survey_data$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(general_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(outtime$X))) # 4410, confirming X(EmployeeID) is key
length(unique(tolower(intime$X))) # 4410, confirming X(EmployeeID) is key

# Checking if there is difference in EmployeeId 
setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(employee_survey_data$EmployeeID,in_time$X) # Identical EmployeeID across these datasets
setdiff(employee_survey_data$EmployeeID,out_time$X) # Identical EmployeeID across these datasets

# Merge dataframes to create single employeeHr dataframe 
employeeHr<- merge(employee_survey_data,general_data, by="EmployeeID", all = F)
employeeHr<- merge(employeeHr,manager_survey_data, by="EmployeeID", all = F)

##### Imputing Missing Values #####
# Finding columns with NA in employeeHr table
# As we will need to impute missing values
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

##### Let's Create Metrics on Time dataset #####
# There are 262 columns on each dataframe intime and outtime 
# each columns is for one non-weekend date 
# Working with time datasets
# There are 12 dates as Holidays(all values NA), let's remove that

intime <- in_time[,!apply(is.na(in_time), 2, all)]
outtime <- out_time[,!apply(is.na(out_time), 2, all)]

# Converts string to dates
intime[,2:250]<-sapply(intime[,2:250], function(x) parse_date_time(x , "YmdHMS"))
outtime[,2:250]<-sapply(outtime[,2:250], function(x) parse_date_time(x , "YmdHMS"))

# Compute hours worked each day
emptimedf <- as.data.frame( cbind(intime$X, sapply(outtime[,2:250] - intime[,2:250], function(x) x/60/60)))

# Let's put all NA as 0 in emp time dataframe
# 0 == holdiday or off taken on that day by the employee

emptimedf[, 2:250][is.na(emptimedf[, 2:250])] <- 0

#Rename column
colnames(emptimedf)[names(emptimedf) == "V1"] = "EmployeeID"
str(emptimedf)

# Create temp_df to reshape the data to create metrics on time dataframe
# We are going to create - Average Hours per week & Number of holidays taken
temp_df <- melt(emptimedf, id=c("EmployeeID"))
temp_df$month <- substr(temp_df$variable,7,8)

emp_avghours_pw <- aggregate(value~EmployeeID, temp_df, sum)
emp_avghours_pw$value <- emp_avghours_pw$value/52

emp_extraOffs <- aggregate(value~EmployeeID, temp_df[temp_df$value==0,], length)
colnames(emp_avghours_pw)[names(emp_avghours_pw) == "value"] = "avg_wrokhours_per_week"
colnames(emp_extraOffs)[names(emp_extraOffs) == "value"] = "Num_of_days_off"
metrics_emptime <- cbind(emp_avghours_pw,emp_extraOffs$Num_of_days_off)

##### Creating Master Source dataframe with all columns #####
# including date metrics
employeeHr<- merge(employeeHr,metrics_emptime, by="EmployeeID", all = F)

#Create deried metric - if person works overtime or not
employeeHr$overtime = ifelse(employeeHr$avg_wrokhours_per_week/40> 1, 1, 0) 
colnames(employeeHr)[names(employeeHr) == "emp_extraOffs$Num_of_days_off"] = "Num_of_days_off"

# Create AgeGroups
unique(employeeHr$Age)
# AgeGroups 20_30, 30_40, 40_50, ovr_50
employeeHr$AgeGroup <- ifelse(20 < employeeHr$Age & employeeHr$Age <= 30 , "20_30",
                                ifelse(30 < employeeHr$Age & employeeHr$Age <= 40 , "31_40",
                                       ifelse(40 < employeeHr$Age & employeeHr$Age <= 50 , "41_50", "over_50")
                                ))


# Create groups for NumCompaniesWorked
employeeHr$CmpWorkGroup = ifelse(employeeHr$NumCompaniesWorked <= 2 , "0_2",
                                 ifelse(2 < employeeHr$NumCompaniesWorked & employeeHr$NumCompaniesWorked <= 5 , "3_5",
                                        ifelse(5 < employeeHr$NumCompaniesWorked & employeeHr$NumCompaniesWorked <= 8 , "6_8", 
                                               "over_8")
                                 ))

# Creating segments of YearsSinceLastPromotion

employeeHr$LastPromotionSeg = ifelse(employeeHr$YearsSinceLastPromotion <= 2 , "0_2",
                                 ifelse(2 < employeeHr$YearsSinceLastPromotion & employeeHr$YearsSinceLastPromotion <= 5 , "3_5",
                                        "over_5"))

# Create groups for YearsWithCurrManager
employeeHr$CurrentMgrTimeSeg = ifelse(employeeHr$YearsWithCurrManager <= 2 , "0_2",
                                 ifelse(2 < employeeHr$YearsWithCurrManager & employeeHr$YearsWithCurrManager <= 5 , "3_5",
                                        ifelse(5 < employeeHr$YearsWithCurrManager & employeeHr$YearsWithCurrManager <= 8 , "6_8", 
                                               "over_8")
                                 ))

# Create groups for YearsAtCompany
employeeHr$CompanyYearSeg = ifelse(employeeHr$YearsAtCompany <= 2 , "0_2",
                                      ifelse(2 < employeeHr$YearsAtCompany & employeeHr$YearsAtCompany <= 5 , "3_5",
                                             ifelse(5 < employeeHr$YearsAtCompany & employeeHr$YearsAtCompany <= 8 , "6_8", 
                                                    ifelse(8 < employeeHr$YearsAtCompany & employeeHr$YearsAtCompany <= 11 , "8_11",
                                                    "over_11"))))

# Master dataset
View(employeeHr) 
length(names(employeeHr))
#32 Columns

##### Start of EDA #####
# Create theme for bar plot
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

# Find variables that are factor
factor_Vars <- names(employeeHr)[sapply(employeeHr, class) == "factor"]

# Ploting variables across attrition
plot_grid(ggplot(employeeHr, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v") 

# BusinessTravel has a clear impact on attrition
# Other variables too are showing significant spread for attrition
# Numberwise More Males leave job but percentagewise women leave mor
# Singles tend to quit jobs more than married and divorced

plot_grid(ggplot(employeeHr, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v")  

# People working in Reaserch be it Research Scientist or Lab Technician quits job more than others
# Sales executives are also attrition prone more than others

# there are still variable which are Factor(ordinal though) but have numerical value
plot_grid(ggplot(employeeHr, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 
# WorkLifeBalance & Eduation is an important Factor
# Looks like EnvironmentSatisfaction and JobSatisfaction explains variance in similar fashion

# These variables are not continuous but categorical(ordincal) in nature 
plot_grid(ggplot(employeeHr, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v") 

# All of these variables looks important as they have significant attrition spread
# StockOptionLevel and JobInvolvement looks like key

plot_grid(ggplot(employeeHr, aes(x=as.factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(CmpWorkGroup),fill=Attrition))+ geom_bar()+bar_theme1)
#Most people leaving have worked in 1 company or if it is their 1st company
#Segmented Variable gives better picture, that people with 0-2 companies or 6-8 are more prone to switch
# we will use CmpWorkGroup

plot_grid(ggplot(employeeHr, aes(x=as.factor(YearsWithCurrManager),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(CurrentMgrTimeSeg),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Attrition,y=YearsWithCurrManager)) + geom_boxplot())
# Years with manager looks to be a key in attrition
# People who have left have a median relation of 2.5 years with current manager
# But when we look at segmented variable we see that people with less than 2 years of exp with manager or with 6 to 8 years of exp
# are more prone to leave company

plot_grid(ggplot(employeeHr, aes(x=Attrition,y=DistanceFromHome)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=DistanceFromHome)) + geom_histogram(bins = 15),
          align="h")
# most people have Distance less than 10Km
# Data is right skewed and people resign have larger spread meaning 
# they travel more even though median in both cases 

plot_grid(ggplot(employeeHr, aes(x=Attrition,y=Age)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=Age)) + geom_histogram(bins=25),
          ggplot(employeeHr, aes(x=AgeGroup,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          align="h")
# Mostly work force has age between 25 to 50
# People resigning appears to be relatively younger ones
# We will use AgeGourp for modeling as it gives a creaer picture, So dropping Age

plot_grid(ggplot(employeeHr, aes(x=Attrition,y=PercentSalaryHike)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=PercentSalaryHike)) + geom_histogram(bins=15))
# Doesn't seem to have outlier 
# doesn't appear to affect attrition directly
# mostly people get 10 to 15% salary hike

plot_grid(ggplot(employeeHr, aes(x=Attrition,y=TotalWorkingYears)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=TotalWorkingYears)) + geom_histogram(bins = 40))
# People leaving company have median 7 years of experience and have relatively lower overall experience
# This might mean as people get more experienced they tend to stay at same company for longer time

plot_grid(ggplot(employeeHr, aes(x=as.factor(YearsSinceLastPromotion),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(LastPromotionSeg),fill=Attrition))+ geom_bar()+bar_theme1 )
# Looks like people are trying to change job soon after getting promotion
# that might make sense as well because that means reaching higher salary/ higher grade jump in relatively lower time span
# The segmentated vaiable captures the data variablitily better and smoother, we will use that

plot_grid(ggplot(employeeHr, aes(x=Attrition,y=avg_wrokhours_per_week)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=avg_wrokhours_per_week)) + geom_histogram(bins = 40))
# People resigning are those who are generally overworked
# Mostlypeople are working less than 35 hours & very few over 50

ggplot(employeeHr, aes(x=Attrition,y=Num_of_days_off)) + geom_boxplot()
# People resigning are those who are generally take fewer holidays in a year

plot_grid(ggplot(employeeHr, aes(x=as.factor(CompanyYearSeg),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(YearsAtCompany),fill=Attrition))+ geom_bar()+bar_theme1)
# People who have spent lesser time at company tends to resign more
# Segmented Variable gives a clear picture about it and is reasonably correlated with attrition. 
# We can say from segmented graph that lesser the number of years at company higher is risk of attrtion

plot_grid(ggplot(employeeHr, aes(x=as.factor(TrainingTimesLastYear),fill=Attrition))+ geom_bar(),
          ggplot(employeeHr, aes(x=Attrition,y=TrainingTimesLastYear)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=TrainingTimesLastYear)) + geom_histogram(bins=10))
# Looks like most people have 2 or 3 times training 
# And people leaving company are mainly those with 2 to 3 trainings
# Median is also at 75th %ile which is 3 trainings per year, data is highly skewed

# Salary Analysis
employeeHr$incomegroup = ifelse(0 < employeeHr$MonthlyIncome & employeeHr$MonthlyIncome <= 50000 , "0 to 50k",
                                ifelse(50000 < employeeHr$MonthlyIncome & employeeHr$MonthlyIncome <= 100000 , "50k to 100k",
                                       ifelse(100000 < employeeHr$MonthlyIncome & employeeHr$MonthlyIncome <= 150000 , "100k to 150k", "more than 150k")
                                ))

newsaldf = employeeHr[,c("Attrition", "incomegroup")]
plot_grid(ggplot(employeeHr, aes(x=Attrition,y=MonthlyIncome)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=MonthlyIncome)) + geom_histogram(bins=40),
          ggplot(newsaldf, aes(x=as.factor(incomegroup),fill=Attrition))+ geom_bar())
# people leaving jobs are relatively low paid
# Which is right as Salary is one of tha major reason people change jobs for
# Employees wh are paid less than 100k are more prone to leave org than othes, specially one getting 0 to 50k

##### Drop variable with just one value #####
employeeHr <- Filter(function(x)(length(unique(x))>1), employeeHr)
employeeHr <- employeeHr[, !(colnames(employeeHr) %in% c('Age','NumCompaniesWorked', "YearsSinceLastPromotion", "YearsWithCurrManager", "YearsAtCompany"))]
##### Outlier treatment for contiuous variables #####
list_of_num_cols <- c("DistanceFromHome", "MonthlyIncome", "PercentSalaryHike", "TotalWorkingYears", "avg_wrokhours_per_week", "Num_of_days_off")
# Total unique values in each column 
apply(employeeHr[,list_of_num_cols], 2, function(x)length(unique(x)))

# checking if there are outliers in numerical columns
apply(employeeHr[,list_of_num_cols], 2, function(x)length(boxplot.stats(x)$out))

xx = sapply(employeeHr[,list_of_num_cols], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

# variables that need outlier treatment
# MonthlyIncome, TotalWorkingYears, YearsWithCurrManager, avg_wrokhours_per_week

## Imputing Outliers with median value
out_pos_inc <- which(employeeHr$MonthlyIncome %in% boxplot.stats(employeeHr$MonthlyIncome)$out)
employeeHr$MonthlyIncome[out_pos_inc] <- NA
employeeHr$MonthlyIncome[is.na(employeeHr$MonthlyIncome)] <- median(employeeHr$MonthlyIncome, na.rm = TRUE)

out_pos_twy <- which(employeeHr$TotalWorkingYears %in% boxplot.stats(employeeHr$TotalWorkingYears)$out)
employeeHr$TotalWorkingYears[out_pos_twy] <- NA
employeeHr$TotalWorkingYears[is.na(employeeHr$TotalWorkingYears)] <- median(employeeHr$TotalWorkingYears, na.rm = TRUE)

out_pos_awh <- which(employeeHr$avg_wrokhours_per_week %in% boxplot.stats(employeeHr$avg_wrokhours_per_week)$out)
employeeHr$avg_wrokhours_per_week[out_pos_awh] <- NA
employeeHr$avg_wrokhours_per_week[is.na(employeeHr$avg_wrokhours_per_week)] <- median(employeeHr$avg_wrokhours_per_week, na.rm = TRUE)

##### Dummy Variable Creation #####
factor_Variables <- c("CompanyYearSeg", "CurrentMgrTimeSeg", "LastPromotionSeg", "TrainingTimesLastYear", "AgeGroup", "CmpWorkGroup", "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance", "BusinessTravel", "Department", "Education", "EducationField", "Gender", "JobLevel", "JobRole", "MaritalStatus", "StockOptionLevel", "JobInvolvement", "PerformanceRating", "overtime", "incomegroup" )
fact_table <- employeeHr[,factor_Variables]
non_fact_table <- employeeHr[,!colnames(employeeHr) %in% factor_Variables]

dummies<- data.frame(sapply(fact_table, 
                            function(x) data.frame(model.matrix(~x-1,data =fact_table))[,-1]))
