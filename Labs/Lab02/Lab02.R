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

x.sd <- sd(selfesteem.data$Height)
x.sd
y.sd <- sd(selfesteem.data$Selfesteem)
y.sd

#Calculate Sample Correlation
cor(selfesteem.data$Height,selfesteem.data$Selfesteem, use="pairwise.complete.obs")


#Simple Linear Regression
lm(housedata$HousePrice~housedata$Size)
m<-lm(housedata$HousePrice~housedata$Size)


#Adding regression line to the current plot
abline(m,col="red")