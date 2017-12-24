# Comment the next line
# setwd("D:/pgdds/Logistic Regression/LogisticRegressionCaseStudy")
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
employee_survey_data<-read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

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
length(unique(tolower(out_time$X))) # 4410, confirming X(EmployeeID) is key
length(unique(tolower(in_time$X))) # 4410, confirming X(EmployeeID) is key

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
colnames(emp_avghours_pw)[names(emp_avghours_pw) == "value"] = "avg_workhours_per_week"
colnames(emp_extraOffs)[names(emp_extraOffs) == "value"] = "Num_of_days_off"
metrics_emptime <- cbind(emp_avghours_pw,emp_extraOffs$Num_of_days_off)

##### Creating Master Source dataframe with all columns #####
# including date metrics
employeeHr<- merge(employeeHr,metrics_emptime, by="EmployeeID", all = F)

#Create deried metric - if person works overtime or not
employeeHr$overtime = ifelse(employeeHr$avg_workhours_per_week/40> 1, 1, 0) 
colnames(employeeHr)[names(employeeHr) == "emp_extraOffs$Num_of_days_off"] = "Num_of_days_off"

# Create AgeGroups
sort(unique(employeeHr$Age))
# AgeGroups Under_30, 31_40, 41_50, ovr_50
employeeHr$AgeGroup <- ifelse(employeeHr$Age <= 30, "Under_30",
                              ifelse(employeeHr$Age <= 40, "31_40",
                                     ifelse(employeeHr$Age <= 50, "41_50", "over_50")
                              ))
summary(factor(employeeHr$AgeGroup))

# Master dataset
View(employeeHr) 
length(names(employeeHr))
#33 Columns

##### Start of EDA #####
# Create theme for bar plot
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

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

ggplot(employeeHr, aes(x=as.factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme1
#Most people leaving have worked in 1 company or if it is their 1st company

plot_grid(ggplot(employeeHr, aes(x=as.factor(YearsWithCurrManager),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Attrition,y=YearsWithCurrManager)) + geom_boxplot())
# Years with manager looks to be a key in attrition
# People who have left have a median relation of 2.5 years with current manager

plot_grid(ggplot(employeeHr, aes(x=Attrition,y=DistanceFromHome)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=DistanceFromHome)) + geom_histogram(bins = 15),
          ggplot(employeeHr, aes(x=as.factor(employeeHr$DistanceFromHome),fill=Attrition))+ geom_bar()+bar_theme1,
          align="h")
# most people have Distance less than 10Km
# When we look absolute numbers we find that most people leaving job is living within 10KM

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
          ggplot(employeeHr, aes(x=TotalWorkingYears)) + geom_histogram(bins = 40),
          ggplot(employeeHr, aes(x=as.factor(TotalWorkingYears),fill=Attrition))+ geom_bar()+bar_theme1)
# People leaving company have median 7 years of experience and have relatively lower overall experience
# This might mean as people get more experienced they tend to stay at same company for longer time


plot_grid(ggplot(employeeHr, aes(x=as.factor(YearsSinceLastPromotion),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Attrition,y=YearsSinceLastPromotion)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=YearsSinceLastPromotion)) + geom_histogram(bins = 40))
# Looks like people are trying to change job soon after getting promotion
# that might make sense as well because that means reaching higher salary/ higher grade jump in relatively lower time span


plot_grid(ggplot(employeeHr, aes(x=Attrition,y=avg_workhours_per_week)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=avg_workhours_per_week)) + geom_histogram(bins = 40))
# People resigning are those who are generally overworked
# Mostlypeople are working less than 35 hours & very few over 50

ggplot(employeeHr, aes(x=Attrition,y=Num_of_days_off)) + geom_boxplot()
# People resigning are those who are generally take fewer holidays in a year

plot_grid(ggplot(employeeHr, aes(x=as.factor(YearsAtCompany),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=Attrition,y=YearsAtCompany)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=YearsAtCompany)) + geom_histogram(bins=10))
# People who have spent lesser time at company tends to resign more

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

# Drop Variables which we have created Segmented Variable for
employeeHr <- employeeHr[, !(colnames(employeeHr) %in% 
                               c('Age','incomegroup'))]

##### Outlier treatment for contiuous variables #####
list_of_num_cols <- c("DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked",
                      "TotalWorkingYears", "PercentSalaryHike", "YearsAtCompany",
                      "YearsSinceLastPromotion", "YearsWithCurrManager",
                      "avg_workhours_per_week", "Num_of_days_off")
# Total unique values in each column 
apply(employeeHr[,list_of_num_cols], 2, function(x)length(unique(x)))

# checking if there are outliers in numerical columns
apply(employeeHr[,list_of_num_cols], 2, function(x)length(boxplot.stats(x)$out))

## Checking the matrix of values at all %iles
xx = sapply(employeeHr[,list_of_num_cols], 
            function(x) quantile(x,seq(0,1,.01),na.rm = T))

# variables that need outlier treatment
# MonthlyIncome, avg_workhours_per_week, YearsSinceLastPromotion, YearsAtCompany
# TotalWorkingYears, YearsWithCurrManager

## Imputing Outliers with median value
# MonthlyIncome
out_pos_inc <- which(employeeHr$MonthlyIncome %in% boxplot.stats(employeeHr$MonthlyIncome)$out)
employeeHr$MonthlyIncome[out_pos_inc] <- NA
employeeHr$MonthlyIncome[is.na(employeeHr$MonthlyIncome)] <- median(employeeHr$MonthlyIncome, na.rm = TRUE)

# avg_workhours_per_week
out_pos_awh <- which(employeeHr$avg_workhours_per_week %in% boxplot.stats(employeeHr$avg_workhours_per_week)$out)
employeeHr$avg_workhours_per_week[out_pos_awh] <- NA
employeeHr$avg_workhours_per_week[is.na(employeeHr$avg_workhours_per_week)] <- median(employeeHr$avg_workhours_per_week, na.rm = TRUE)

# YearsSinceLastPromotion
out_pos_ylp <- which(employeeHr$YearsSinceLastPromotion %in% boxplot.stats(employeeHr$YearsSinceLastPromotion)$out)
employeeHr$YearsSinceLastPromotion[out_pos_ylp] <- NA
employeeHr$YearsSinceLastPromotion[is.na(employeeHr$YearsSinceLastPromotion)] <- median(employeeHr$YearsSinceLastPromotion, na.rm = TRUE)

# YearsAtCompany
out_pos_yac <- which(employeeHr$YearsAtCompany %in% boxplot.stats(employeeHr$YearsAtCompany)$out)
employeeHr$YearsAtCompany[out_pos_yac] <- NA
employeeHr$YearsAtCompany[is.na(employeeHr$YearsAtCompany)] <- median(employeeHr$YearsAtCompany, na.rm = TRUE)

# TotalWorkingYears
out_pos_twy <- which(employeeHr$TotalWorkingYears %in% boxplot.stats(employeeHr$TotalWorkingYears)$out)
employeeHr$TotalWorkingYears[out_pos_twy] <- NA
employeeHr$TotalWorkingYears[is.na(employeeHr$TotalWorkingYears)] <- median(employeeHr$TotalWorkingYears, na.rm = TRUE)

# YearsWithCurrManager
out_pos_cmy<- which(employeeHr$YearsWithCurrManager %in% boxplot.stats(employeeHr$YearsWithCurrManager)$out)
employeeHr$YearsWithCurrManager[out_pos_cmy] <- NA
employeeHr$YearsWithCurrManager[is.na(employeeHr$YearsWithCurrManager)] <- median(employeeHr$YearsWithCurrManager, na.rm = TRUE)

##### Dummy Variable Creation #####
factor_Variables <- c("AgeGroup",
                      "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance",
                      "BusinessTravel", "Department", "Education", "EducationField",
                      "Gender", "JobLevel", "JobRole", "MaritalStatus", "StockOptionLevel",
                      "JobInvolvement", "PerformanceRating", "overtime" )
fact_table <- employeeHr[,factor_Variables]
fact_table <- lapply(fact_table, factor)

str(fact_table)

# Create Dummy Variables
dummies<- data.frame(sapply(fact_table, 
                            function(x) data.frame(model.matrix(~x-1,data =fact_table))[,-1]))
##### Treating continuous Variables #####
non_fact_table <- employeeHr[,!colnames(employeeHr) %in% factor_Variables]
#emp_Attr <- non_fact_table[, 2]
Attrition <- ifelse(non_fact_table$Attrition == "Yes",1,0)
cont_var_df <- data.frame(sapply(non_fact_table[, 3:13], 
                                 function(x) scale(x)))

# Creating Final dataframe for building model
final_df <- cbind(Attrition,cont_var_df, dummies)

##### Seeing Correlation Matrix #####
cormatt = cor(final_df[,-1])
View(cormatt)
#PerformanceRating and PercentSalaryHike are positively and highly correlated
#YearsAtCompany and YearsWithCurrentManager are highly(+vely) correlated
#BusinessTravely - Rarely and Frequently are highly negatively correlated ~ -0.8
#overtime and avg_workhours_pw are highly correlated
#Department - Sales & ResearchDevelopment are very highly correlated ~ -0.9

##### Splitting data in train and test #####
set.seed(100)
trainindices= sample(1:nrow(final_df), 0.7*nrow(final_df))
train = final_df[trainindices,]
test = final_df[-trainindices,]

##### Start building Logistic Regression Model here #####

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2163.6 coeff : nullDev 2747.7, resDev 2043.6

# Stepwise selection
model_2 <- stepAIC(model_1, direction="both")
model_2

# Let's create a model as suggested by stepAIC method:
model_3 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                 avg_workhours_per_week + Num_of_days_off + AgeGroup.x41_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x5 + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_3)
vif(model_3)

# Let's remove EducationField.xLife.Sciences highest Vif and high p-value 0.058443

# Let's create a model as suggested by stepAIC method:
model_4 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                 avg_workhours_per_week + Num_of_days_off + AgeGroup.x41_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x5 +
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_4)
vif(model_4)

# Highest vif of train$BusinessTravel.xTravel_Frequently, train$BusinessTravel.xTravel_Rarely
cor(train$BusinessTravel.xTravel_Frequently, train$BusinessTravel.xTravel_Rarely)
# correlation = -0.77

# Let's remove train$BusinessTravel.xTravel_Rarely as this has higher p-value and is highly correlated with BusinessTravel.xTravel_Frequently
model_5 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                 avg_workhours_per_week + Num_of_days_off + AgeGroup.x41_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Department.xSales + Education.x5 +
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_5)
vif(model_5)

# Highest vif of train$Department.xResearch...Development, train$Department.xSales
cor(train$Department.xResearch...Development, train$Department.xSales)
# correlation = -0.91

# Let's remove train$Department.xSales as this one highly correlated with Department.xResearch...Development
model_6 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                 avg_workhours_per_week + Num_of_days_off + AgeGroup.x41_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_6)
vif(model_6)

# Highest vif of train$WorkLifeBalance.x2, train$WorkLifeBalance.x3
cor(train$WorkLifeBalance.x2, train$WorkLifeBalance.x3)
# correlation of -0.69

# WorkLifeBalance.x2 has high vif & high p-value, let's remove this ad build model again
model_7 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                 avg_workhours_per_week + Num_of_days_off + AgeGroup.x41_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_7)
vif(model_7)

# check correlation b/w avg_workhours_per_week & overtime
cor(train$avg_workhours_per_week, train$overtime)
#0.77
# Let's remove - overtime as it has higher p-valueof 0.08

model_8 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                 avg_workhours_per_week + Num_of_days_off + AgeGroup.x41_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3,
               family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_8)
vif(model_8)

# Let's find correlation b/w MaritalStatus.xMarried & MaritalStatus.xSingle
cor(train$MaritalStatus.xSingle, train$MaritalStatus.xMarried)
#-0.63
# Let's remove - MaritalStatus.xMarried as it has higher p-valueof 0.086015

model_9 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                 avg_workhours_per_week + Num_of_days_off + AgeGroup.x41_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3,
               family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_9)
vif(model_9)

# All variables have vif < 2
# let's now focus on p-value
# Num_of_days_off has p-value of 0.215, let's remove this
model_10 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Education.x5 + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_10)

# let's remove EducationField.xMedical, pvalue = 0.18
model_11 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Education.x5 + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_11)

# let's remove EducationField.xOther, pvalue = 0.197
model_12 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Education.x5 + EducationField.xMarketing +
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_12)

# let's remove JobRole.xSales.Executive, pvalue = 0.18
model_13 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Education.x5 + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_13)

# let's remove WorkLifeBalance.x4, pvalue = 0.19
model_14 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Education.x5 + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_14)

# let's remove StockOptionLevel.x3 , pvalue = 0.18
model_15 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Education.x5 + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_15)

# let's remove JobRole.xResearch.Director as it has p-value 0.096 > 0.05
model_16 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Education.x5 + EducationField.xMarketing + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_16)

# let's remove EducationField.xMarketing, pvalue = 0.095
model_17 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Education.x5 + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_17)

# Let's remove Department.xResearch...Development, pvalue = 0.15
model_18 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + Education.x5 + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_18)

# Let's remove StockOptionLevel.x1, pvalue= 0.88
model_19 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + Education.x5 + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_19)

# Let's remove Education.x5, with pvalue = 0.066
model_20 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently +
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_20)

# Let's remove EducationField.xTechnical.Degree, pvalue 0.059
model_21 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + JobLevel.x5 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_21)

# Next lets remvove JobLevel.x5 as it has higher p-value
model_22 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_22)

# Next lets remove JobRole.xManager
model_23 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  JobInvolvement.x3,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_23)

# Let's remove JobInvolvement.x3
model_24 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.x41_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_24)

#Let's remove AgeGroup.x41_50
model_25 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.xUnder_30 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_25)


#Let's remove JobSatisfaction.x2
model_26 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.xUnder_30 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_26)

#Let's remove JobSatisfaction.x3, as its pvalue is now 0.08
model_27 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.xUnder_30 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_27)

# Let's remove TrainingTimesLastYear as it has p-value higher than others and is 2*
model_28 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.xUnder_30 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_28)

# Let's remove WorkLifeBalance.x3, as it is the only variable that's 2 star
model_29 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsAtCompany + YearsSinceLastPromotion + 
                  avg_workhours_per_week + AgeGroup.xUnder_30 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 +
                  BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                family = "binomial", data = train)
# Let us look at the summary of the model
summary(model_29)

### Model Evaluation
final_model <- model_29

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-c(1,2)])

# Let's see the summary 
summary(test_pred)

test$prob <- test_pred
View(test)

test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.80,length=200)
OUT = matrix(0,200,3)
for(i in 1:200)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
# Let's choose a cutoff value of 0.1727638 for final model
test_cutoff_attrition <- factor(ifelse(test_pred >=0.1727638, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

##### Evaluating Model on KS-Statistics
test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# KS-Statistics is 0.4535

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

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
# Here we reach 76.3% gain in first 4 deciles
# and it outperform random model with a factor of 1.908213

###### Results Explained ###### 
# Ours is a fairly well performing model
### Variables explained --
#NumCompaniesWorked --> The person is more likely to jump orgs in their early days
#TotalWorkingYears --> this variable has a very low p-value and has stronger impact on attrition. The employees with higher work experience tend to stay longer at company
#YearsAtCompany --> People who have spent lesser time at company tends to resign more
#YearsSinceLastPromotion --> Has strong impact on attrition, people tend to leave company soon after getting promotion
#avg_workhours_per_week --> Has strong impact on attrition, people who are working more hours, i.e. overworked employee, are more likely to leave company
#AgeGroup.xUnder_30 --> it tells that people with age under 30 are more likely to change job than others
#EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3  - People who say that work environment is not so good are more likely to leave
#EnvironmentSatisfaction.x4 --> this is a strange entry, it means that people who find environment satisfactory are also looking for change
#JobSatisfaction.x4 --> People who are satisfied with their job are also leaving, which means that there are better opportunities they must be getting outside. It is time to evaluate what company offers its employees to work on
#BusinessTravel.xTravel_Frequently --> People traveling frequently is coming out a factor for attrition, as it may be affecting their work life balance
#JobRole.xManufacturing.Director --> employees who are director are leaving too, they might be looking for career progression
#MaritalStatus.xSingle --> the employees who are single have higher chances of leaving than the ones married or divorced
