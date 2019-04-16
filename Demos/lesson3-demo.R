#Read in data
library("xlsx")
data <- read.xlsx(".\\demoData\\house-price-with-size.xlsx", sheetIndex = 2, header = TRUE)
data

#Correlation and Scatterplots
cor(data)
plot(data)


#Multiple Linear Regression
m<-lm(data$HousePrice~data$Size+data$Rooms)
summary(m)


#compute R^2
totalss <-sum((housedata$HousePrice -mean(housedata$HousePrice))^2)
regss <-sum((fitted(m) -mean(housedata$HousePrice))^2)
residss <-sum((housedata$HousePrice-fitted(m))^2)
rsquare <- regss/totalss
rsquare
