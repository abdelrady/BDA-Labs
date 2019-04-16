install.packages("naivebayes")
library("naivebayes")

#load the data set
data(iris)
head(iris)

iris

summary(iris$Species)

nb <- naive_bayes(iris$Species ~ ., data = iris)
nb

iris[,-5]

# alternatively:
nb <- naiveBayes(iris[,-5], iris[,5]) #-5 means not including fifth column here

#using the naive bayes' model for prediction on the training data
predict(nb, iris[,-5])

#classification result
table(predict(nb, iris[,-5]), iris[,5])

sum(predict(nb, iris[,-5])!=iris[,5])

