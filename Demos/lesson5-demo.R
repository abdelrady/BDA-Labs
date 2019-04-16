data<-read.csv(".\\demoData\\titanic.csv")
head(data)
#view the data in R as excel file
#View(data)

#set up factors(categorical data)
is.factor(data$survived)
data$survived <- as.factor(data$survived)

table(data$survived)
table(data$survived) / length(data$survived)
prop.table(table(data$survived))

table(data$pclass)

# create a frequency bar chart survival

plot(x = data$survived,
     main = "Titanic Survival Rate",
     col = "pink",
     xlab = "survived",
     ylab = "passenger count")

plot(x = data$survived,
     horiz = TRUE,
     main = "Titanic Survival Rate",
     col = "pink",
     ylab = "survived",
     xlab = "passenger count")

#another way to create frequency bar chart
barplot(table(data$survived), 
        main = "Titanic Survival Rate", 
        col = "pink", 
        ylim = c(0, 1000),  
        xlab = "survived",
        ylab = "passenger count")

barplot(table(data$survived), 
        horiz = TRUE,
        main = "Titanic Survival Rate", 
        col = "green", 
        ylab = "survived",
        xlab = "passenger count")

#Creates a pie chart
lables <- names(table(data$pclass))
percentage <- round(table(data$pclass) / length(data$pclass)*100)
newlables <- paste("Passenger Class", lables, percentage, "%")
pie(table(data$pclass), 
    labels = newlables, 
    clockwise = TRUE, 
    col = c("brown","green", "skyblue"))

#Summarizing Quantitative Data
mean(data$age, na.rm = TRUE)
median(data$age, na.rm = TRUE)
min(data$age, na.rm = TRUE)
sd(data$age, na.rm = TRUE)
var(data$age, na.rm = TRUE)
range(data$age, na.rm = TRUE)
summary(data$age)

quantile(data$age, c(0,0.25,0.5,0.75,1), na.rm = TRUE)
IQR(data$age, na.rm = TRUE)


#quantitative univariate analysis
#create a dot plot of age
?rep
plot(x = data$age,
     y = rep(0, nrow(data)), #repeat 0, nrow(data) times
     main = "Distribution of passenger age",
     xlab = "Age",
     ylab = "", #no y label necessary
     yaxt = "n") #to suppress rendering of the y-axis text



#Jitter is a random value that is assigned to the dots to separate them 
#so that they aren't plotted directly on top of each other. 
plot(x = data$age,
     y = jitter(rep(0, nrow(data))),
     main = "Distribution of passenger age",
     xlab = "Age",
     ylab = "", #no y label necessary
     yaxt = "n") #to suppress rendering of the y-axis text

#creat a boxplot of age
boxplot(x = data$age,
     horizontal = TRUE,
     main = "Distribution of passenger age",
     xlab = "Age")

#create a histogram of age
hist(x = data$age,
     breaks = 15, #of bins, default 20
     main = "Distribution of passenger age",
     xlab = "Age")

#create a density plot of age
plot(x = density(na.omit(data$age)), #omit missing age values
     main = "Distribution of passenger age",
     xlab = "Age")


#sex is the row, survived is the column
data$sex <- as.factor(data$sex)
x <- table(data$sex, data$survived)
colnames(x) <- c("perished", "survived")
x

prop.table(x)
prop.table(x, 1)
prop.table(x, 2)

#Qualitative Bivariate Analysis
barplot(x, ylim = c(0, 700),
        col=c("pink", "blue"), 
        beside=TRUE, 
        legend.text=TRUE)

mosaicplot(x,
           main = "Survival By Gender",
           xlab ="gender",
           ylab="survival",
           color = c("pink", "blue")) 

#Quantitative Bivariate Analysis

plot(data$age, data$fare)

#Qualitative and Quantitative Bivariate Analysis

boxplot(data$fare ~ data$pclass,
        xlab = "Class",
        ylab = "Ticket Fare")

#ggplot2
install.packages("ggplot2")
library(ggplot2)

data$Survived <- as.factor(data$survived)
data$Sex <- as.factor(data$sex)
data$Pclass <- as.factor(data$pclass)

ggplot(data, aes(x = data$Survived))+geom_bar()

?theme_bw

ggplot(data, aes(x = data$Sex, fill = data$Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Sex")

ggplot(data, aes(x=data$age, fill=data$Survived)) +
  geom_density(alpha=0.5) +
  labs(x = "Age",
       y = "Survived",
       title = "Titanic Survival Rates by Age")
  