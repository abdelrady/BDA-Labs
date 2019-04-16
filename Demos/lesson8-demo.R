data(iris)
head(iris)
#names(iris) <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species')

# min-max normalization
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize the `iris` data
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

iris_norm$Species <- iris$Species
# Summarize `iris_norm`
summary(iris_norm)

 
#Using R's random number generator. 
#The major advantage of setting a seed is that you can get the same sequence 
#of random numbers whenever you supply the same seed in the random number generator.
set.seed(1234)
#Sample with replacement: sample method generates 1 or 2, 150 times. 
#The generation of 1 or 2 is subject to probability weights of 0.67 and 0.33.
#As a result, about 2/3*150 = 100 1's and 50 2's
index <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Compose training set
iris.training <- iris_norm[index==1, 1:4]

# Inspect training set
head(iris.training)

# Compose test set
iris.test <- iris_norm[index==2, 1:4]


# Compose `iris` training labels
iris.trainLabels <- iris_norm[index==1,5]

# Inspect result
print(iris.trainLabels)
summary(iris.trainLabels)

# Compose `iris` test labels
iris.testLabels <- iris_norm[index==2, 5]

# Inspect result
print(iris.testLabels)

# Build the model
library(class)
sqrt(nrow(iris))
#prob = false by default, if prob = false, we get prediction of the class labels
iris_pred <-knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=12, prob = TRUE)


# Inspect `iris_pred`
print(iris_pred)
summary(iris_pred)
#accuracy
table(iris.testLabels, iris_pred)
sum(iris_pred == iris.testLabels)/length(iris.testLabels)

#Demo of ROC Curve
#ROCR currently supports only evaluation of binary classification tasks.
library(ROCR)
library(xlsx)
coronarydata <- read.xlsx(".\\demoData\\risk-of-coronary.xlsx", sheetIndex = 1, header = TRUE)
head(coronarydata)

coronarydata$event1 <- ifelse(coronarydata$event == 1, "pain", "no pain")
mode(coronarydata$event1)

library(rpart)

tree.model <- rpart(event1 ~ age+chol+sex, data = coronarydata)

tree.prob <- predict(tree.model, coronarydata, type='prob')

tree.prob

#create prediction object that can be examined with the plotting function of ROCR
#first argument is predicted probabilities for class Pain, second one is true class labels 
pred <- prediction(predictions = tree.prob[,2], labels = coronarydata$event1)

#compute tpr and fpr from prediction object
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC curve", col = "blue")

#compute area under the curve from prediction object
#perf.auc is a performance object
perf.auc <- performance(pred, measure = "auc")

#the auc is stored as a list in the y.values slot 
#unlist() simplifies lists to a vector of numeric values
unlist(perf.auc@y.values)
