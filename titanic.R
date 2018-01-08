# comment next line
setwd("C:/Users/putripat/Kaggle/Titanic/input")

##### Import Libraries #####
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

##### reading titanic dataset #####
titanic = read.csv("train.csv", stringsAsFactors = F)

# Creating derived metrics
titanic$family_size <- titanic$SibSp+titanic$Parch

titanic$fare_per_person <- titanic$Fare/(titanic$family_size + 1)

titanic$traveling_alone <- ifelse(titanic$family_size == 0, 'Yes', 'No')

# Treating Missing Values 
cols_with_na <- colnames(titanic)[colSums(is.na(titanic)) > 0]

titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

titanic$CabinAvl <- ifelse(titanic$Cabin == "", "No", "Yes")

titanic$title <- unlist(lapply(unlist(lapply(titanic$Name, function(x) unlist(strsplit(x, ","))[2])), function(x) unlist(strsplit(x, "[.]"))[1] ))

titanic$is_Adult <- ifelse(titanic$Age <= 18, "No", "Yes")

str(titanic)

##### EDA ####
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position="none")

plot_grid(ggplot(titanic, aes(x=Sex,fill=factor(Survived)))+ geom_bar(),
          ggplot(titanic, aes(x=Embarked,fill=factor(Survived)))+ geom_bar()+bar_theme1,
          ggplot(titanic, aes(x=factor(Parch),fill=factor(Survived)))+ geom_bar()+bar_theme1,
          ggplot(titanic, aes(x=factor(SibSp),fill=factor(Survived)))+ geom_bar()+bar_theme1,
          ggplot(titanic, aes(x=factor(Pclass),fill=factor(Survived)))+ geom_bar()+bar_theme1,
          align = "h")

plot_grid(ggplot(titanic, aes(x=factor(family_size),fill=factor(Survived)))+ geom_bar()+bar_theme1,
          ggplot(titanic, aes(x=factor(traveling_alone),fill=factor(Survived)))+ geom_bar()+bar_theme1,
          align = "h")
# People who are not traveling alone have a higher chance of survival

##### Data Preparation #####
titanic$Age <- scale(titanic$Age)
titanic$fare_per_person = scale(titanic$fare_per_person)

cor(titanic$Fare, titanic$fare_per_person)

# Removing columns which are not needed
titanic[, !(colnames(employeeHr) %in% c('Name', 'Cabin', 'Fare'))]

# Convert categorical columns to factor
titanic$Pclass = factor(titanic$Pclass)


c("Sex")

