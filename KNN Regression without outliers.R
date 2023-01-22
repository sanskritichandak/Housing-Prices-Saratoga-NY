#Clean up code 
rm(list = ls())	
gc()	
cat("\f")

#Load the packages
source("BabsonAnalytics.R")
library(caret)
library(scales)

#Load the data 
Housing_df <- read.csv("Housing_Prices_Final.csv")
View(Housing_df)
str(Housing_df)
anyNA(Housing_df)

#Manage the data  
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

#Removing Outliers based on Z-scores
Housing_df <- removeOutliers(Housing_df)

#Covert all the variables to integers 
Housing_df$Lot.Size <- as.integer(Housing_df$Lot.Size)
Housing_df$Waterfront <- as.integer(Housing_df$Waterfront)
Housing_df$Age <- as.integer(Housing_df$Age)
Housing_df$Land.Value <- as.integer(Housing_df$Land.Value)
Housing_df$New.Construct <- as.integer(Housing_df$New.Construct)
Housing_df$Central.Air <- as.integer(Housing_df$Central.Air)
Housing_df$Fuel.Type <- as.integer(Housing_df$Fuel.Type)
Housing_df$Heat.Type <- as.integer(Housing_df$Heat.Type)
Housing_df$Sewer.Type <- as.integer(Housing_df$Sewer.Type)
Housing_df$Living.Area <- as.integer(Housing_df$Living.Area)
Housing_df$Pct.College <- as.integer(Housing_df$Pct.College)
Housing_df$Bedrooms <- as.integer(Housing_df$Bedrooms)
Housing_df$Fireplaces <- as.integer(Housing_df$Fireplaces)
Housing_df$Bathrooms <- as.integer(Housing_df$Bathrooms)
Housing_df$Rooms <- as.integer(Housing_df$Rooms)

#Standardizing the variables 
Processor <- preProcess(Housing_df[ , -1], c("center", "scale"))
Housing_df <- predict(Processor, Housing_df)

#Partition the data 
set.seed(1234)

Split <- 0.7 
N <- nrow(Housing_df)
TrainingSize <- round(N*Split)
TrainingCases <- sample(N, TrainingSize)
Training <- Housing_df[TrainingCases, ]
Test <- Housing_df[- TrainingCases, ]

#Build the model 
KNNReg <- knnreg(Price ~ Central.Air + Rooms + Bathrooms + Living.Area, data = Training, k = 3)

#Make predictions
Predictions <- predict(KNNReg, Test)

#Evaluate the model 
Actuals <- Test$Price
Errors <- Actuals - Predictions 
RMSE <- sqrt(mean(Errors^2))
MAPE <- mean(abs(Errors/Actuals))
MAPE <- percent(MAPE, accuracy = 0.1)

#Benchmarking 
Benchmark <- mean(Training$Price)
Errors_bench <- Actuals - mean(Training$Price)
RMSE_bench <- sqrt(mean(Errors_bench^2))
MAPE_bench <- mean(abs(Errors_bench/Actuals))
MAPE_bench <- percent(MAPE_bench, accuracy = 0.1)

