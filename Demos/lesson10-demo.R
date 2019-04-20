iris
dim(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

head(iris)
#this command initializes R's random number generator to a specific sequence
#so set.seed to ensure reproducibility.
?set.seed
set.seed(20)
iris[, 3:4]
irisCluster <- kmeans(iris[, 3:4], 3)
?kmeans
irisCluster


#how good the clustering result?
table(irisCluster$cluster, iris$Species)

#plot the data to see the clusters
irisCluster$cluster.1 <- as.factor(irisCluster$cluster)
irisCluster$cluster.1
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster.1)) + geom_point()


# optional from here

#about between_SS and total_SS
irisCluster$centers
irisCluster$cluster
irisCluster$totss
irisCluster$withinss

mean1 <- mean(iris$Petal.Length)
mean2 <- mean(iris$Petal.Width)

#compute total_SS by hand
sum((iris$Petal.Length - mean1)^2 + (iris$Petal.Width - mean2)^2)


#compute within-cluster sum of squares for cluster 1
mean1 <- irisCluster$centers[1,1]
mean2 <- irisCluster$centers[1,2]
iris.cluster1 <- iris[irisCluster$cluster == 1,]
sum((iris.cluster1$Petal.Length - mean1)^2 + (iris.cluster1$Petal.Width - mean2)^2)

#between_SS = total_SS - total within-cluster sum of squares
#In this example: between_SS / total_SS =  94.3 % 
#it is a  measure of the total variance in the data set that is explained by the clustering 



