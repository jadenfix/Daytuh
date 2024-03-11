#install.packages("xts")
library(xts)
data <- read.csv("data145.csv")


#data$spdividend <- log(data$spdividend)
#data$spearnings <- log(data$spearnings)
#data$cpi <- log(data$cpi)
#data$CPIrent <- log(data$CPIrent)
#data$employeecomp <- log(data$employeecomp)
#data$commercialbankcredit<- log(data$commercialbankcredit)
#data$nonrevolvingconsumercredit <- log(data$nonrevolvingconsumercredit)
#data$incomeonassets <- log(data$incomeonassets)
#data$PCE <- log(data$PCE)
#data$mktvalueprivatedebt <- log(data$mktvalueprivatedebt)
#data$mktvaluegrossfeddebt <- log(data$mktvaluegrossfeddebt)
#data$m1 <- log(data$m1)
#data$m2 <- log(data$m2)

data$date <- as.Date(data$date, format="%d-%b-%Y")
# last 3 rows gone
dim(data)
#as ts 
data <- xts(data[, -1], order.by = data$date)
#install.packages(c("caret", "randomForest", "xgboost", "e2072", "nnet", "kernlab"))
#library(kernlab)
#library(caret)
#library(randomForest)
#library(xgboost)
#library(e2072)
#library(nnet)

set.seed(999)
data$spprice <- lag(data$spprice, k = -36)  # 36 months in a year
na_rows <- apply(is.na(data), 1, any)
last_36_na_rows <- tail(which(na_rows), 36)

# Get the corresponding rows for training
training_rows1 <- (last_36_na_rows - 1)  # Adjust if needed

# Create training and testing sets
training_data1 <- data[-last_36_na_rows, ]
testing_data1 <- data[last_36_na_rows, ]
#
training_data_scaled1 <- scale(training_data_scaled1)
testing_data_scaled1 <- scale(testing_data1)

# Check the dimensions of the training and testing sets
dim(training_data_scaled1)
dim(testing_data_scaled1)

ctrl1 <- trainControl(method = "cv", number = 5, preProcOptions = list(c("center", "scale")))

models1 <- list(
  LinearRegression1 = train(spprice ~ ., data = training_data_scaled1, method = "lm", trControl = ctrl1,
                            tuneGrid=data.frame(c(2,4,6))),
  RandomForest1 = train(spprice ~ ., data = training_data_scaled1, method = "rf", trControl = ctrl1),
  XGBoost1 = train(spprice ~ ., data = training_data_scaled1, method = "xgbTree", trControl = ctrl1),
  SVM1 = train(spprice ~ ., data = training_data_scaled1, method = "svmRadial", trControl = ctrl1))

# Linear Regression
lm_predictions1 <- predict(models1$LinearRegression1, newdata = testing_data_scaled1)

# Random Forest
rf_predictions1 <- predict(models1$RandomForest1, newdata = testing_data_scaled1)

# XGBoost
xgb_predictions1 <- predict(models1$XGBoost1, newdata = testing_data_scaled1)

# SVM
svm_predictions1 <- predict(models1$SVM1, newdata = testing_data_scaled1)

print(lm_predictions1)
print(rf_predictions1)
print(xgb_predictions1)
print(svm_predictions1)

# Optionally, you can create a data frame to store the predictions
# Create a data frame with actual and predicted values
results_df1 <- data.frame(
  LinearRegression = lm_predictions1,
  RandomForest = rf_predictions1,
  XGBoost = xgb_predictions1,
  SVM = svm_predictions1)
print(results_df1)
