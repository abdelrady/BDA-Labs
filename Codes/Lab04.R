#logistic regression
#Example - Risk of Coronary Event

#read in data
data <- read.csv(".\\lab4-data.csv")
data

# summary for 2 columns (tratement, pain)
summary(data[,c(1,length(data))])

#simple logistic regression model 
m<-glm(data$Pain ~ data$Treatment, family = binomial)
summary(m)


#compute McFadden's pseudo-R squared
#m<-glm(data$event ~ data$chol, family = binomial)
nullm <- glm(data$Pain ~ 1, family="binomial")
rsquare = 1-logLik(m)/logLik(nullm)
rsquare


#multiple logistic regression model 
multi.m<-glm(data$Pain ~ data$Treatment+data$Age+data$Severe, family = binomial)
summary(multi.m)

exp(0.02119)

# Odd per 1 unit increase in treatment
exp(multi.m$coefficients[2])

# Odd per 10 unit increase in age
exp(multi.m$coefficients[3]*10)

# Odd per 1 unit increase in severity
exp(multi.m$coefficients[4])


multi.rsquare = 1-logLik(multi.m)/logLik(nullm)
rsquare
multi.rsquare

