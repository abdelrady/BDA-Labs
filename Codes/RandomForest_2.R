# Data Source: https://archive.ics.uci.edu/ml/machine-learning-databases/car/

# install.packages("randomForest")
library(randomForest)

# Load the dataset and explore
cars.data <- read.csv(".\\DemoData\\iris.data", header = TRUE)

head(iris.data)

str(iris.data)

summary(iris.data)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(iris.data), 0.7*nrow(iris.data), replace = FALSE)
TrainSet <- iris.data[train,]
ValidSet <- iris.data[-train,]
summary(TrainSet)
summary(ValidSet)


# Create a Random Forest model with default parameters
simple.model <- randomForest(class ~ ., data = TrainSet, importance = TRUE)
simple.model


# Fine tuning parameters of Random Forest model
enhanced.model <- randomForest(class ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)
enhanced.model


# Predicting on train set
predTrain <- predict(enhanced.model, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$class)  
mean(predTrain == TrainSet$class)                    


# Predicting on Validation set
predValidationation <- predict(enhanced.model, ValidSet, type = "class")
# Checking classification accuracy
mean(predValidationation == ValidSet$class)                    
table(predValidationation,ValidSet$class)


# To check important variables
importance(enhanced.model)        
varImpPlot(enhanced.model)     




# Using For loop to identify the right mtry for model
accuracy=c()
i=2
mtrys = c(2:4)
for (i in mtrys) {
  model3 <- randomForest(class ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValidation <- predict(model3, ValidSet, type = "class")
  accuracy[i-1] = mean(predValidation == ValidSet$class)
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

model_dt = train(class ~ ., data = TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$class)

mean(model_dt_1 == TrainSet$class)



# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$class)

mean(model_dt_vs == ValidSet$class)


