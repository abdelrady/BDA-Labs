#Linear Regression lab

#Read in data
selfesteem.data <- read.csv(".\\lab02.csv")
selfesteem.data

#Basic Scatterplot
?plot
plot(selfesteem.data$Height,selfesteem.data$Selfesteem)

#Scatterplot with labs, and controlling axes
plot(selfesteem.data$Height,selfesteem.data$Selfesteem, 
     main="Scatterplot of Person Height versus Self Esteem",
     xlab = "Height", ylab="Self Esteem", 
     xlim=c(55, 75), ylim=c(2.5, 5.5), pch = 8, col="seagreen3",
     cex=1.5, cex.lab = 1.5, cex.main = 1.5)

x.mean <- mean(selfesteem.data$Height)
x.mean
y.mean <- mean(selfesteem.data$Selfesteem)
y.mean

points(65.28, 3.76, col="red", pch=19)

x.sd <- sd(selfesteem.data$Height)
x.sd
y.sd <- sd(selfesteem.data$Selfesteem)
y.sd

#Calculate Sample Correlation
cor(selfesteem.data$Height,selfesteem.data$Selfesteem, use="pairwise.complete.obs")


#Simple Linear Regression
m <- lm(selfesteem.data$Selfesteem~selfesteem.data$Height)
m

#Adding regression line to the current plot
abline(m,col="red")


#Request important summary information from R about the model
summary(m)

#Calculate coefficients by hand

beta1 <- cor(selfesteem.data$Height,selfesteem.data$Selfesteem) * sd(selfesteem.data$Selfesteem)/sd(selfesteem.data$Height)
beta1
beta0 <- mean(selfesteem.data$Selfesteem)-beta1*mean(selfesteem.data$Height)
beta0

fitted(m)

#compute R square by hand
totalss <-sum((selfesteem.data$Selfesteem -mean(selfesteem.data$Selfesteem))^2)
regss <-sum((fitted(m) -mean(selfesteem.data$Selfesteem))^2)
residss <-sum((selfesteem.data$Selfesteem-fitted(m))^2)
rsquare <- regss/totalss
rsquare
