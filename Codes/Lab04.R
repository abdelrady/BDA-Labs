#logistic regression
#Example - Risk of Coronary Event

#read in data
data <- read.csv(".\\lab4-data.csv")
data

attach(data)

# summary for 2 columns (tratement, pain)
summary(data[,c(1,length(data))])

t <- table(Treatment, Pain)
t
addmargins(t)
prop.table(t, 1) # 1 means summarize by column, 2 means summarize by row

# convert strings into numeric for GLM 
data$Treatment <- ifelse(data$Treatment == "New Treatment", 1, 0)
# OR USE: Treatment <- ifelse(Treatment == "New Treatment", 0, 1)

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

multi.m$coefficients

# Odd per 1 unit increase in treatment
exp(multi.m$coefficients[2])

# Odd per 10 unit increase in age
exp(multi.m$coefficients[3]*10)

# Odd per 1 unit increase in severity
exp(multi.m$coefficients[4])


multi.rsquare = 1-logLik(multi.m)/logLik(nullm)
rsquare
multi.rsquare

