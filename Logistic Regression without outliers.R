#Clean-up Code
rm(list = ls())
gc()
cat("\f")

#Load the Data
source('BabsonAnalytics.R')
library(scales)
Housing_df = read.csv('Housing_Prices_Final.csv')
View(Housing_df)
str(Housing_df)
anyNA(Housing_df)

#Manage the Data
Housing_df$Fuel.Type[Housing_df$Fuel.Type == "2"] = 1
Housing_df$Fuel.Type[Housing_df$Fuel.Type == "3"] = 2
Housing_df$Fuel.Type[Housing_df$Fuel.Type == "4"] = 3
Housing_df$Heat.Type[Housing_df$Heat.Type == "2"] = 1
Housing_df$Heat.Type[Housing_df$Heat.Type == "3"] = 2
Housing_df$Heat.Type[Housing_df$Heat.Type == "4"] = 3

Housing_df$Pct.College <- NULL
Housing_df$New.Construct = as.factor(as.logical(Housing_df$New.Construct))
Housing_df$Waterfront = as.factor(as.logical(Housing_df$Waterfront))
Housing_df$Central.Air = as.logical(Housing_df$Central.Air)
Housing_df$Fuel.Type = as.factor(Housing_df$Fuel.Type)
Housing_df$Heat.Type = as.factor(Housing_df$Heat.Type)
Housing_df$Sewer.Type = as.factor(Housing_df$Sewer.Type)

#Removing Outliers based on Z-scores
Housing_df <- removeOutliers(Housing_df)

#Partition the Data
set.seed(1234)
Split = 0.7
N = nrow(Housing_df)
TrainingSize = round(N*Split)
TrainingCases = sample(N, TrainingSize)
Training = Housing_df[TrainingCases, ]
Test = Housing_df[-TrainingCases, ]

#Build the Model
LogReg = glm(Central.Air ~., data = Training, family = 'binomial')
summary(LogReg)

LogReg = step(LogReg, direction = 'backward')

#Make Predictions
Predictions = predict(LogReg, Test, type = 'response')
PredTF = (Predictions > 0.7)

#Evaluate the Model
Actuals = Test$Central.Air
table(PredTF, Actuals)

library(scales)
ErrorRate = percent(sum(PredTF != Actuals) / length(Actuals), accuracy = 0.1)
Benchmark = Training$Central.Air
ErrorBench = benchmarkErrorRate(Benchmark, Actuals)
ErrorBench = percent(ErrorBench, accuracy = 0.1)

#Sensitivity and Specificity
True_Neg = sum(Actuals == FALSE & PredTF == FALSE)
True_Pos = sum(Actuals == TRUE & PredTF == TRUE)
False_Neg = sum(Actuals == TRUE & PredTF == FALSE)
False_Pos = sum(Actuals == FALSE & PredTF == TRUE)

Sensitivity = percent(True_Pos/(True_Pos + False_Neg), accuracy = 0.1)
Specificity = percent(True_Neg/(True_Neg + False_Pos), accuracy = 0.1)

#ROC chart
ROCChart(Actuals, Predictions)
liftChart(Actuals, Predictions)

