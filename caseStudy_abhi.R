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
library(outliers)

##### Importing CSV data to DataFrames #####
employee_survey_data<-read.csv("employee_survey_data.csv")
general_data <- read.csv("general_data.csv")
in_time <- read.csv("in_time.csv")
manager_survey_data <- read.csv("manager_survey_data.csv")
out_time <- read.csv("out_time.csv")

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


# Create groups for NumCompaniesWorked
#sort(unique(employeeHr$NumCompaniesWorked))
#employeeHr$CmpWorkGroup = ifelse(employeeHr$NumCompaniesWorked <= 2 , "0_2",
#                                 ifelse(employeeHr$NumCompaniesWorked <= 5 , "3_5",
#                                        ifelse(employeeHr$NumCompaniesWorked <= 8 , "6_8", "over_8")
#                                 ))
#summary(factor(employeeHr$CmpWorkGroup))

# Creating segments of YearsSinceLastPromotion
#sort(unique(employeeHr$YearsSinceLastPromotion))
#employeeHr$LastPromotionSeg = ifelse(employeeHr$YearsSinceLastPromotion <= 3 , "0_2",
#                                     ifelse(employeeHr$YearsSinceLastPromotion <= 5, "3_5",
#                                            ifelse(employeeHr$YearsSinceLastPromotion <= 9 , "6_9", "over_9"
#                                            )))
#summary(factor(employeeHr$LastPromotionSeg))

# Create groups for YearsWithCurrManager
#sort(unique(employeeHr$YearsWithCurrManager))
#employeeHr$CurrentMgrTimeSeg = ifelse(employeeHr$YearsWithCurrManager <= 2 , "0_2",
#                                      ifelse(employeeHr$YearsWithCurrManager <= 5 , "3_5",
#                                             ifelse(employeeHr$YearsWithCurrManager <= 8 , "6_8", 
#                                                    "over_8")
#                                      ))
#summary(factor(employeeHr$CurrentMgrTimeSeg))

# Create groups for YearsAtCompany
#sort(unique(employeeHr$YearsAtCompany))
#employeeHr$CompanyYearSeg = ifelse(employeeHr$YearsAtCompany <= 2 , "0_2",
#                                   ifelse(employeeHr$YearsAtCompany <= 5 , "3_5",
#                                          ifelse(employeeHr$YearsAtCompany <= 8 , "6_8", 
#                                                 ifelse(employeeHr$YearsAtCompany <= 11 , "8_11",
#                                                        "over_11"))))
#summary(factor(employeeHr$CompanyYearSeg))

# Create groups for TotalWorkingYears
#sort(unique(employeeHr$TotalWorkingYears))
#employeeHr$WorkYearSeg = ifelse(employeeHr$TotalWorkingYears <= 3 , "0_3",
#                                ifelse(3 < employeeHr$TotalWorkingYears & employeeHr$TotalWorkingYears <= 7 , "4_7",
#                                       ifelse(7 < employeeHr$TotalWorkingYears & employeeHr$TotalWorkingYears <= 11 , "8_11", 
#                                              ifelse(11 < employeeHr$TotalWorkingYears & employeeHr$TotalWorkingYears <= 15 , "12_15",
#                                                     ifelse(15 < employeeHr$TotalWorkingYears & employeeHr$TotalWorkingYears <= 20 , "16_20", "over_21")))))
#summary(factor(employeeHr$WorkYearSeg))

# Master dataset
View(employeeHr) 
length(names(employeeHr))
#38 Columns

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
          ggplot(employeeHr, aes(x=as.factor(TotalWorkingYears),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(WorkYearSeg),fill=Attrition))+ geom_bar()+bar_theme1)
# People leaving company have median 7 years of experience and have relatively lower overall experience
# This might mean as people get more experienced they tend to stay at same company for longer time
# From segmented Bar plot, we can veerify above hypothesis, that people with lower work exp tend to leave jobs more
# We will use WorkYearSeg for further analysis and drop TotalWorkingYears

plot_grid(ggplot(employeeHr, aes(x=as.factor(YearsSinceLastPromotion),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeeHr, aes(x=as.factor(LastPromotionSeg),fill=Attrition))+ geom_bar()+bar_theme1 )
# Looks like people are trying to change job soon after getting promotion
# that might make sense as well because that means reaching higher salary/ higher grade jump in relatively lower time span
# The segmentated vaiable captures the data variablitily better and smoother, we will use that

plot_grid(ggplot(employeeHr, aes(x=Attrition,y=avg_workhours_per_week)) + geom_boxplot(),
          ggplot(employeeHr, aes(x=avg_workhours_per_week)) + geom_histogram(bins = 40))
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

# Drop Variables which we have created Segmented Variable for
employeeHr <- employeeHr[, !(colnames(employeeHr) %in% 
                               c('Age','incomegroup'
                                 #'NumCompaniesWorked', "YearsSinceLastPromotion",
                                 #"YearsWithCurrManager", "YearsAtCompany", "TotalWorkingYears",
                                 ))]

##### Outlier treatment for contiuous variables #####
list_of_num_cols <- c("DistanceFromHome", "MonthlyIncome", "PercentSalaryHike",
                      "avg_workhours_per_week", "Num_of_days_off")
# Total unique values in each column 
apply(employeeHr[,list_of_num_cols], 2, function(x)length(unique(x)))

# checking if there are outliers in numerical columns
apply(employeeHr[,list_of_num_cols], 2, function(x)length(boxplot.stats(x)$out))

xx = sapply(employeeHr[,list_of_num_cols], 
            function(x) quantile(x,seq(0,1,.01),na.rm = T))

# variables that need outlier treatment
# MonthlyIncome, avg_workhours_per_week

## Imputing Outliers with median value
out_pos_inc <- which(employeeHr$MonthlyIncome %in% boxplot.stats(employeeHr$MonthlyIncome)$out)
employeeHr$MonthlyIncome[out_pos_inc] <- NA
employeeHr$MonthlyIncome[is.na(employeeHr$MonthlyIncome)] <- median(employeeHr$MonthlyIncome, na.rm = TRUE)

out_pos_awh <- which(employeeHr$avg_workhours_per_week %in% boxplot.stats(employeeHr$avg_workhours_per_week)$out)
employeeHr$avg_workhours_per_week[out_pos_awh] <- NA
employeeHr$avg_workhours_per_week[is.na(employeeHr$avg_workhours_per_week)] <- median(employeeHr$avg_workhours_per_week, na.rm = TRUE)

##### Dummy Variable Creation #####
factor_Variables <- c(#"WorkYearSeg", "CompanyYearSeg", "CurrentMgrTimeSeg", "LastPromotionSeg",
                      "AgeGroup", #"CmpWorkGroup",
                      "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance",
                      "BusinessTravel", "Department", "Education", "EducationField",
                      "Gender", "JobLevel", "JobRole", "MaritalStatus", "StockOptionLevel",
                      "JobInvolvement", "PerformanceRating", "overtime" )
fact_table <- employeeHr[,factor_Variables]
fact_table$EnvironmentSatisfaction <- as.factor(fact_table$EnvironmentSatisfaction)
fact_table$JobSatisfaction <- as.factor(fact_table$JobSatisfaction)
fact_table$WorkLifeBalance <- as.factor(fact_table$WorkLifeBalance)
fact_table$Education <- as.factor(fact_table$Education)
fact_table$JobLevel <- as.factor(fact_table$JobLevel)
fact_table$StockOptionLevel <- as.factor(fact_table$StockOptionLevel)
fact_table$JobInvolvement <- as.factor(fact_table$JobInvolvement)
fact_table$PerformanceRating <- as.factor(fact_table$PerformanceRating)
fact_table$overtime <- as.factor(fact_table$overtime)
fact_table$AgeGroup <- as.factor(fact_table$AgeGroup)

# Create Dummy Variables
dummies<- data.frame(sapply(fact_table, 
                            function(x) data.frame(model.matrix(~x-1,data =fact_table))[,-1]))
##### Treating continuous Variables #####
non_fact_table <- employeeHr[,!colnames(employeeHr) %in% factor_Variables]
emp_Attr <- non_fact_table[, 1:2]
cont_var_df <- data.frame(sapply(non_fact_table[, 3:8], 
                                 function(x) scale(x)))

# Creating Final dataframe for building model
final_df <- cbind(emp_Attr,cont_var_df, dummies)
final_df$Attrition <- ifelse(as.character(final_df$Attrition) == "Yes",1,0)

##### Seeing Correlation Matrix #####
cormatt = cor(final_df[2:56])
View(cormatt)

##### Splitting data in train and test #####
set.seed(100)
trainindices= sample(1:nrow(final_df), 0.7*nrow(final_df))
train = final_df[trainindices,]
test = final_df[-trainindices,]


##### Start building Logistic Regression Model here #####

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2954.2 coeff : nullDev 3895.7, resDev 2794.2

# Stepwise selection
model_2 <- stepAIC(model_1, direction="both")

# Let's create a model as suggested by stepAIC method:
model_3 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + AgeGroup.x41_50 + AgeGroup.xover_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_3)
vif(model_3)

# Removing variable EducationField.xLife.Sciences, high p-value=0.089617 and high vif=17.569672
model_4 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + AgeGroup.x41_50 + AgeGroup.xover_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_4)
vif(model_4)

# Removing variable EducationField.xMarketing, high p-value=0.466872
model_5 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + AgeGroup.x41_50 + AgeGroup.xover_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_5)
vif(model_5)

# Removing variable EducationField.xMedical, high p-value=0.178949
model_6 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + AgeGroup.x41_50 + AgeGroup.xover_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_6)
vif(model_6)

# Removing variable EducationField.xOther, high p-value=0.123222
model_7 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + AgeGroup.x41_50 + AgeGroup.xover_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_7)
vif(model_7)

# Removing variable AgeGroup.x41_50, high p-value=0.121648
model_8 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + AgeGroup.xover_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_8)
vif(model_8)

# Removing variable JobRole.xHuman.Resources, high p-value=0.141699
model_9 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + AgeGroup.xover_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_9)
vif(model_9)

# Removing variable StockOptionLevel.x3, high p-value=0.111230
model_10 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + AgeGroup.xover_50 + 
                 AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + JobInvolvement.x3 + 
                 overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_10)
vif(model_10)

# Removing variable JobRole.xManager, high p-value=0.104863
model_11 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  EducationField.xTechnical.Degree + 
                  JobLevel.x5 +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + JobInvolvement.x3 + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_11)
vif(model_11)

# Removing variable EducationField.xTechnical.Degree, high p-value=0.097830
model_12 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobLevel.x5 +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + JobInvolvement.x3 + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_12)
vif(model_12)

# Removing variable MaritalStatus.xMarried, high p-value=0.069247
model_13 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobLevel.x5 +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + JobInvolvement.x3 + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_13)
vif(model_13)

# Removing variable StockOptionLevel.x1, high p-value=0.031645
model_14 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobLevel.x5 +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.x3 + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_14)
vif(model_14)

# Removing variable JobLevel.x5, high p-value=0.016315
model_15 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.x3 + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_15)
vif(model_15)

# Removing variable JobRole.xManufacturing.Director, high p-value=0.007870
model_16 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobInvolvement.x3 + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_16)
vif(model_16)

# Removing variable JobInvolvement.x3, high p-value=0.005755
model_17 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_17)
vif(model_17)

# Removing variable JobSatisfaction.x2
model_18 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_18)
vif(model_18)

# Removing variable JobSatisfaction.x3
model_19 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_19)
vif(model_19)

# Removing variable JobRole.xResearch.Director
model_20 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_20)
vif(model_20)

# Removing variable JobRole.xSales.Executive
model_21 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_21)
vif(model_21)

# Removing variable WorkLifeBalance.x4
model_22 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_22)
vif(model_22)

# Removing variable WorkLifeBalance.x2
model_23 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + AgeGroup.xover_50 + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_23)
vif(model_23)

# Removing variable AgeGroup.xover_50
model_24 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear +
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_24)
vif(model_24)

# Removing variable TrainingTimesLastYear
model_25 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  AgeGroup.xUnder_30 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + 
                  MaritalStatus.xSingle + 
                  overtime, family = "binomial", data = train)
# Let us look at the summary and vif of the model
summary(model_25)
vif(model_25)
