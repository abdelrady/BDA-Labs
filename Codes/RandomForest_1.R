# Data Source: https://archive.ics.uci.edu/ml/machine-learning-databases/car/

# install.packages("randomForest")
library(randomForest)

# Load the dataset and explore
cars.data <- read.csv(".\\DemoData\\car.data", header = TRUE)

head(cars.data)

str(cars.data)

summary(cars.data)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(cars.data), 0.7*nrow(cars.data), replace = FALSE)
TrainSet <- cars.data[train,]
ValidSet <- cars.data[-train,]
summary(TrainSet)
summary(ValidSet)


# Create a Random Forest model with default parameters
simple.model <- randomForest(Condition ~ ., data = TrainSet, mtry = 2, importance = TRUE)
simple.model


# Fine tuning parameters of Random Forest model
enhanced.model <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
enhanced.model


# Predicting on train set
predTrain <- predict(enhanced.model, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Condition)  



# Predicting on Validation set
predValidationation <- predict(enhanced.model, ValidSet, type = "class")
# Checking classification accuracy
mean(predValidationation == ValidSet$Condition)                    
table(predValidationation,ValidSet$Condition)


# To check important variables
importance(enhanced.model)        
varImpPlot(enhanced.model)     




# Using For loop to identify the right mtry for model
accuracy=c()
i=2
mtrys = c(2:6)
for (i in mtrys) {
  model3 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValidation <- predict(model3, ValidSet, type = "class")
  accuracy[i-1] = mean(predValidation == ValidSet$Condition)
}

accuracy

plot(mtrys,accuracy)



# Compare with Decision Tree

# install.packages("rpart")
# install.packages("caret")
# install.packages("e1071")

library(rpart)
library(caret)
library(e1071)

# We will compare simple model of Random Forest with Decision Tree model

model_dt = train(Condition ~ ., data = TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$Condition)

mean(model_dt_1 == TrainSet$Condition)



# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$Condition)

mean(model_dt_vs == ValidSet$Condition)


