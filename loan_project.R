library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("corrplot")
library(corrplot)

train_loan <- read.csv("C:/Users/moharahman/Desktop/Project With Morphy/train_u6lujuX_CVtuZ9i.csv", header=TRUE,fill = T,na.strings = "")
View(train_loan)

str(train_loan)
train_loan$Gender <- as.factor(train_loan$Gender)
train_loan$Married <- as.factor(train_loan$Married)
train_loan$Dependents <- as.numeric(train_loan$Dependents)
train_loan$Education <- as.factor(train_loan$Education)
train_loan$Self_Employed <- as.factor(train_loan$Self_Employed)
train_loan$Credit_History <- as.factor(train_loan$Credit_History)
train_loan$Property_Area <- as.factor(train_loan$Property_Area)
train_loan$Loan_Status <- as.factor(train_loan$Loan_Status)

summary(train_loan) #show us which variable has NA values

#let's replace the NA values (if any), with most number of values (for non-numeric/factor) 
#or the mean of the corresponding variable (for numeric values)

train_loan <- train_loan %>%
  replace_na(list(Gender='Male', Married='Yes', Self_Employed='No', Credit_History='1'))

train_loan$Dependents[is.na(train_loan$Dependents)] <- round(mean(train_loan$Dependents, na.rm=T), digit=0)

train_loan$LoanAmount[is.na(train_loan$LoanAmount)] <- round(mean(train_loan$LoanAmount, na.rm=T), digit=0)

train_loan$Loan_Amount_Term[is.na(train_loan$Loan_Amount_Term)] <- round(mean(train_loan$Loan_Amount_Term, na.rm=T), digit=0)


#Descriptive Analysis
attach(train_loan)

#Means
round(colMeans(data.frame(Dependents, ApplicantIncome, CoapplicantIncome, LoanAmount, Loan_Amount_Term)), digit=2)

#Correlations
cor_train <- round(cor(data.frame(Dependents, ApplicantIncome, CoapplicantIncome, LoanAmount, Loan_Amount_Term)), digit=2)
cor_train
corrplot(cor_train, method="circle")
corrplot(cor_train, method="pie")
corrplot(cor_train, method="color")

ggplot(data = train_loan, aes(x=ApplicantIncome, y= LoanAmount, color=Married))+
  geom_point(size=4)+
  labs(x= "Applicant's Income",
       y= "Loan Amount",
       title = "Applicant Income & Loan Amount",
       subtitle = "Loan Prediction Project",
       caption = "Dataset: Loan Prediction Problem Dataset(Kaggle)")+
  stat_smooth(method="lm", se=F)+
  facet_wrap(~Education)+
  theme_gray() + 
  theme(legend.position="right")+
  scale_colour_discrete("Married")



