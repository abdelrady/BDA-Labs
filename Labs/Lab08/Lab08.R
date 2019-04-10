# install.packages("ROCR")
library(ROCR)
data <- read.csv(".\\lab08_data.csv")
head(data)


#create prediction object that can be examined with the plotting function of ROCR
#first argument is predicted probabilities for class ActualClass, second one is true class labels 
pred <- prediction(predictions = data$Probability, labels = data$ActualClass)


?performance
#compute tpr and fpr from prediction object
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC curve", col = "blue")

#compute area under the curve from prediction object
#perf.auc is a performance object
perf.auc <- performance(pred, measure = "auc")

#the auc is stored as a list in the y.values slot 
#unlist() simplifies lists to a vector of numeric values
unlist(perf.auc@y.values)

