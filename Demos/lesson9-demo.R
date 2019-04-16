#install.packages("arules")
library("arules")

#create a sparse matrix
grocery <- read.transactions(".\\demoData\\grocery.csv",  sep = ",")
summary(grocery)


#R has this dataset Groceries with 9835 rows
data("Groceries")

summary(Groceries)

#look at first five transactions
inspect(Groceries[1:5])

#visualize the first 10 rows of sparse matrix
image(Groceries[1:10])
##visualize the randomly sampled 100 rows of sparse matrix
image(sample(Groceries,100))

#examine a particular item(a column of data)
#the proportion of transactions that contain the item
itemFrequency(Groceries[, 1:3])

#plot frequent items with min support = 0.1
itemFrequencyPlot(Groceries, support = 0.1)

#plot top 20 frequent items
itemFrequencyPlot(Groceries, topN = 20)

#use apriori to generate rules
rules <- apriori(Groceries, 
                 parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
summary(rules)
inspect(rules[1:5])

#get top five highest lift rules
inspect(sort(rules, by="lift")[1:5])

#find subset of the rules with berrries appearing in the rule
sub.rules <- subset(rules, items %in% "berries")
inspect(sub.rules)
