#Linear Regression lab

#Read in data
selfesteem.data <- read.csv(".\\lab02.csv")
selfesteem.data

attach(selfesteem.data)
#Basic Scatterplot
?plot
plot(Height,Selfesteem)

#Scatterplot with labs, and controlling axes
plot(Height,Selfesteem, 
     main="Scatterplot of Person Height versus Self Esteem",
     xlab = "Height", ylab="Self Esteem", 
     xlim=c(min(Height), max(Height)), ylim=c(min(Selfesteem), max(Selfesteem)), pch = 8, col="seagreen3",
     cex=1.5, cex.lab = 1.5, cex.main = 1.5)

x.mean <- mean(selfesteem.data$Height)
x.mean
y.mean <- mean(selfesteem.data$Selfesteem)
y.mean

points(x.mean, y.mean, col="red", pch=19)

x.sd <- sd(Height)
x.sd
y.sd <- sd(Selfesteem)
y.sd

#Calculate Sample Correlation
cor(Height,Selfesteem, use="pairwise.complete.obs")


#Simple Linear Regression
m <- lm(Selfesteem~Height)
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

?fitted
selfesteem.data$Selfesteem
fitted(m)

#compute R square by hand
totalss <-sum((selfesteem.data$Selfesteem -mean(selfesteem.data$Selfesteem))^2)
regss <-sum((fitted(m) -mean(selfesteem.data$Selfesteem))^2)
residss <-sum((selfesteem.data$Selfesteem-fitted(m))^2)
rsquare <- regss/totalss
rsquare
