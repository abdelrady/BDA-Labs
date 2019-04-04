#Linear Regression lab

#Read in data
prestige.data <- read.csv(".\\lab3-data.csv")
prestige.data

#Basic Scatterplot
?plot
plot(prestige.data$PrestigeScore,prestige.data$EducationLevel)

#Scatterplot with labs, and controlling axes
plot(prestige.data$PrestigeScore,prestige.data$EducationLevel, 
     main="Scatterplot of Person Prestige Score Based On Education Level",
     xlab = "EducationLevel", ylab="Prestige Score", 
     xlim=c(10, 90), ylim=c(5, 20), pch = 8, col="seagreen3",
     cex=1.5, cex.lab = 1.5, cex.main = 1.5)

#Calculate Sample Correlation
cor(prestige.data$EducationLevel,prestige.data$PrestigeScore, use="pairwise.complete.obs")


#Simple Linear Regression
m <- lm(prestige.data$PrestigeScore~prestige.data$EducationLevel+prestige.data$Income+prestige.data$PercentOfWomen)
m

#Request important summary information from R about the model
summary(m)

#compute R^2
totalss <-sum((prestige.data$PrestigeScore - mean(prestige.data$PrestigeScore))^2)
regss <-sum((fitted(m) -mean(prestige.data$PrestigeScore))^2)
residss <-sum((prestige.data$PrestigeScore-fitted(m))^2)
rsquare <- regss/totalss
rsquare
