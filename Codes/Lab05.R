#read in data
data <- read.csv(".\\5000movies.csv")
data
#head(data)

#View(data)

#table(data$original_language)


#set up factors(categorical data)
is.factor(data$original_language)

# 1. create a frequency bar chart for languages
# 1.plot
plot(x = data$original_language,
     main = "Movies Count Per Language",
     col = "pink",
     xlab = "Languages",
     ylab = "Movies count")

# 1.ggplot2
#install.packages("ggplot2")
library(ggplot2)

ggplot(data, aes(x = data$original_language))+theme_bw()+geom_bar()+
  labs(y = "Movies Count", x="Languages",
       title = "Movies Count Per Language")

# 2
# create a denisty chart for vote_average
plot(x = density(data$vote_average), #omit missing age values
     main = "Distribution of Movies Votes",
     xlab = "Vote")

# 2.ggplot2
ggplot(data, aes(x = data$vote_average))+theme_light(base_size = 20)+geom_histogram()+
  labs(y = "Movies Count Percentage", x="Vote Average",
       title = "Distribution of Movies Votes")


# 3.Spoken Language VS. Genre
genre.lang <- table(data$spoken_languages, data$genre)
genre.lang

# 3.1
mosaicplot(genre.lang,
           main = "Genre By Spoken Language",
           xlab ="genre",
           ylab="Language",
           color = colors()[seq(1, to = 1020, by=30)]) 

# 3.2
# convert data to data frame
d.f <- data.frame(data)
d.f

# filter to fetch only Drama/Action movies
action.darama.movies <- d.f[(d.f$genre=="Action" | d.f$genre=="Drama") & (d.f$spoken_languages == "en" | d.f$spoken_languages=="hi"), ]

# compare action/Drama movies between English & Indian languages
ggplot(action.darama.movies, aes(action.darama.movies$spoken_languages, fill = action.darama.movies$genre)) +
  geom_bar(position = "fill") 



# 4 Runtime vs. Revenue plots
hexbinplot(
  x = data$revenue ~ data$runtime,
  data = data,
  xbins = 30,
  main = "Runtime vs. Revenue",
  xlab = "Runtime (minutes)",
  ylab = "Revenue")

# 4.b ggplot
ggplot(
  data = data,
  aes(x = data$runtime, y = data$revenue)) +
  #stat_density2d(aes(fill = ..level..), geom = "polygon") + 
  #geom_density2d() + 
  stat_bin2d() + 
  #stat_binhex() + 
  ggtitle("Runtime vs. Revenue") +
  xlab("Runtime (minutes)") +
  ylab("Revenue")

ggplot(data, aes(x=data$runtime, y=data$budget)) + 
  geom_violin()


# 5. Genre VS. Budget
# a
plot(
  x = data$genre,
  y = data$budget,
  las = 2
)

# b
ggplot(data, aes(x=data$genre, y=data$budget)) + 
  geom_violin()

# Qualitative and Quantitative Bivariate Analysis
boxplot(data$budget ~ data$runtime,
        xlab = "runtime",
        ylab = "Budget")




install.packages("hexbin")
library(hexbin)

ggplot(DS0012, aes(x = gender, y = BMI, fill = gender)) + geom_boxplot() +
  facet_wrap(~ age.category, ncol = 5)

genre.lang.frame <- data.frame(genre.lang)

ggplot(genre.lang.frame, aes(x=genre.lang.frame$genre, fill=genre.lang.frame$spoken_languages)) +
  geom_bar() + 
  labs(x = "Genre",
       y = "Labguage",
       title = "Genre By Spoken Language")

famous.langs <- filter(data, spoken_languages == "en")

ggplot(data, aes(data$spoken_languages, fill = data$genre)) +
  geom_bar(position = "fill") +
  facet_wrap(~data$spoken_languages)
