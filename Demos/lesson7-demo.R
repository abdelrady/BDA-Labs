
install.packages("rpart")
install.packages("rpart.plot")

data(iris)
#Using R's random number generator. 
#The major advantage of setting a seed is that you can get the same sequence 
#of random numbers whenever you supply the same seed in the random number generator.
set.seed(1234)
#Sample with replacement: sample method generates 1 or 2, 150 times. 
#The generation of 1 or 2 is subject to probability weights of 0.67 and 0.33.
#As a result, about 2/3*150 = 100 1's and 50 2's
sampleset <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

sampleset
# Compose training set
train <- iris[sampleset == 1, ]
# Compose test set
test <- iris[sampleset == 2, ]
nrow(iris)
nrow(train)
nrow(test)

?rpart

library(rpart)
library(rpart.plot)

#recursive partition and regression trees algorithm
#rpart() function uses the Gini impurity measure to split the node
tree <- rpart(Species ~ Petal.Length+Petal.Width+Sepal.Width+Sepal.Length, train)

#try extra = 1/2/3/4
rpart.plot(tree, extra = 1) 

#try type = 'prob'
predict <-predict(tree, test, type = 'class')
predict

table(predict, test$Species)
