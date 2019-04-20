#vinstall.packages("arules")
library("arules")

#create a sparse matrix
grocery <- read.transactions(".\\grocery.csv",  sep = ",")
summary(grocery)

#plot frequent items with min support = 0.1
itemFrequencyPlot(Groceries, support = 0.15)

#use apriori to generate rules
rules <- apriori(Groceries, 
                 parameter = list(support = 0.006, confidence = 0.25, minlen = 2))


#get top five highest lift rules
inspect(sort(rules, by="lift")[1:5])

#find subset of the rules with berrries appearing in the rule
sub.rules <- subset(rules, rhs %in% "tropical fruit")
inspect(sub.rules)


sub.rules.2 <- subset(rules, (items %in% "berries" | items %in% "yogurt") & lift > 3)
# or
sub.rules.2 <- subset(rules, items %in% c("berries", "yogurt") & lift > 3)
sub.rules.2
inspect(sub.rules.2)



