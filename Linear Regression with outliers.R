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
Housing_df$New.Construct = as.logical(Housing_df$New.Construct)
Housing_df$Waterfront = as.logical(Housing_df$Waterfront)
Housing_df$Central.Air = as.logical(Housing_df$Central.Air)
Housing_df$Fuel.Type = as.factor(Housing_df$Fuel.Type)
Housing_df$Heat.Type = as.factor(Housing_df$Heat.Type)
Housing_df$Sewer.Type = as.factor(Housing_df$Sewer.Type)

#Partition the Data
set.seed(1234)

Split = 0.7
N = nrow(Housing_df)
TrainingSize = round(N*Split)
TrainingCases = sample(N, TrainingSize)
Training = Housing_df[TrainingCases, ]
Test = Housing_df[-TrainingCases, ]

#Build the Linear Regression Model
LinReg = lm(Price~ . , data = Training)

summary(LinReg)

options(scipen = 3)

# Forward Stepwise Regression
Intercept_only = lm(Price ~ 1, data = Training)
Intercept_only$coefficient
mean(Training$Price)

All = lm(Price ~ ., data = Training)

Forward = step(Intercept_only, direction = 'forward', scope = formula(All))
Forward$anova

summary(Forward)

#Make Predictions
LinReg = Forward
Predictions = predict(LinReg, Test)

#Evaluate the Model
Actuals = Test$Price
Errors = Actuals - Predictions

RMSE = sqrt(mean(Errors^2))
MAPE = mean(abs(Errors/Actuals))
MAPE = percent(MAPE, accuracy = 0.1)

#Benchmarking
Benchmark = mean(Training$Price)

Errors_Bench = Actuals - Benchmark
RMSE_Bench = sqrt(mean(Errors_Bench^2))
MAPE_Bench = percent(mean(abs(Errors_Bench / Actuals)), accuracy = 0.1)


