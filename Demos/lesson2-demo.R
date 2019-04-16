#Linear Regression demos

#Read in data
housedata <- read.csv(".\\demoData\\house-price-with-size.csv")
housedata

#Basic Scatterplot
?plot
plot(housedata$Size,housedata$HousePrice)

#Scatterplot with labs, and controlling axes
plot(housedata$Size,housedata$HousePrice, 
main="Scatterplot of House Price versus Size",
xlab = "Size", ylab="House Price", 
xlim=c(1400, 2600), ylim=c(140000, 300000), pch = 8, col="seagreen3",
 cex=1.5, cex.lab = 1.5, cex.main = 1.5)

#Calculate Sample Correlation
cor(housedata$Size,housedata$HousePrice)

cor(housedata$HousePrice,housedata$Size)

#Simple Linear Regression
lm(housedata$HousePrice~housedata$Size)
m<-lm(housedata$HousePrice~housedata$Size)


#Adding regression line to the current plot
abline(m,col="red")


#Request important summary information from R about the model
summary(m)

#Calculate coefficients by hand

beta1 <- cor(housedata$Size,housedata$HousePrice) * sd(housedata$HousePrice)/sd(housedata$Size)
beta1
beta0 <- mean(housedata$HousePrice)-beta1*mean(housedata$Size)
beta0

fitted(m)

#compute R square by hand
totalss <-sum((housedata$HousePrice -mean(housedata$HousePrice))^2)
regss <-sum((fitted(m) -mean(housedata$HousePrice))^2)
residss <-sum((housedata$HousePrice-fitted(m))^2)
rsquare <- regss/totalss
rsquare
