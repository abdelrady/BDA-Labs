#install.packages("tm")
#install.packages("SnowballC")
#install.packages("pdftools") #for reading pdfs

library(tm)#Framework for textmining.
library(SnowballC)#Provides wordStem() for stemming.

pdf.loc <- file.path(paste(getwd(),"/demoData", sep=""), "I have a dream.pdf")
pdf.loc

#create a uniform resource identifier source
#A uniform resource identifier source interprets each URI as a document.
cname <- URISource(pdf.loc) 
cname

corpus.pdf <- Corpus(cname, readerControl=list(reader=readPDF))
inspect(corpus.pdf)
corpus.pdf[[1]]$content #to view the content extracted
corpus.pdf[[1]]$meta 

dtm <- DocumentTermMatrix(corpus.pdf) #Create Document Term Matrix
freq <- colSums(as.matrix(dtm)) #Term frequencies
freq
ord <- order(freq) #Ordering the frequencies
freq[tail(ord)] #Most frequent terms
findFreqTerms(dtm, lowfreq = 10) #get the terms that appear at least 10 times


# getTransformations()

#Preprocessing
# Convert to lower case
#tolower is a function we can wrapped with content_transformer
corpus.tranf <- tm_map(corpus.pdf, content_transformer(tolower))
# Remove Punctuation
corpus.tranf <- tm_map(corpus.tranf, removePunctuation) 
# Remove stop words
# stopwords("english")
corpus.tranf <- tm_map(corpus.tranf, removeWords, stopwords("english"))
# specify your stopwords as a character vector
corpus.tranf <- tm_map(corpus.tranf, removeWords, c("since", "let", "yeah", "yes")) 
# Perform Stemmings
#corpus.tranf <- tm_map(corpus.tranf, stemDocument, language = "english")


dtm <- DocumentTermMatrix(corpus.tranf) #Create Document Term Matrix
inspect(dtm[1:1,20:30])
freq <- colSums(as.matrix(dtm)) #Term frequencies
ord <- order(freq) #Ordering the frequencies
freq[tail(ord)] #Most frequent terms
findFreqTerms(dtm, lowfreq = 5) #get the terms that appear at least 5 times

# Create a Word Cloud Plot
# install.packages("wordcloud")
library(wordcloud)
wordcloud(names(freq), freq, min.freq=10, colors=brewer.pal(6, "Dark2"))

#text mining - clustering
dir = DirSource(paste(getwd(),"/demoData/news",sep="")) #read all txt files in news folder of current working directory
corpus = Corpus(dir, readerControl=list(reader=readPlain))
ndocs <- length(corpus)
ndocs
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 100% of the documents
maxTermFreq <- ndocs * 1.0
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))
?weightTfIdf
dtm <- weightTfIdf(dtm)
dtm.matrix = as.matrix(dtm)
#write.csv(dtm.matrix, "test.csv") #save matrix to file

### don't forget to normalize the vectors so Euclidean makes sense
?apply
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(dtm.matrix)


### cluster into 2 clusters
cl <- kmeans(m_norm, 2)
cl
cl$cluster

### --- kNN Text Classification   ----
Doc1 <- "I spent 10K on my car. Compared to the prices of most cars in their class it was cheap. It is a red car so I like it and it has a lot of space."
Doc2 <- "I checked the car prices and I could not find a red car for under 10K. So the price was good even though it had a hole. I heard that it belonged to a movie star." 
Doc3 <- "I like the red color, so I would buy a red car even if the car's price is over 10K."
Doc4 <- "I don't like red cars. The insurance for red cars is higher regardless of the price and I would not spend more than 10K. I like black cars."
Doc5 <- "A red giant star can curve the space to form a black hole. In absence of stars the space is flat."
Doc6 <- "With exception of the stars the space is filled with blackness making the black holes even harder to see."
Doc7 <- "Our sun is a small star and it will not end as a black hole. It does not have enough mass to curve the space."
Doc8 <- "Very few stars will end as black holes but still the space contains large number of black holes."

doc <- c(Doc1,Doc2,Doc3,Doc4,Doc5,Doc6,Doc7,Doc8)
corpus <- Corpus(VectorSource(doc))

# Preprocessing
corpus.temp <- tm_map(corpus, removePunctuation) # Remove Punctuation
corpus.temp <- tm_map(corpus.temp, stemDocument, language = "english")# Perform Stemming
dtm <- as.matrix(DocumentTermMatrix(corpus.temp)) # Document term matrix

# Text Classification
library(class) # Using kNN 
train.doc <- dtm[c(1,2,5,6),] # Dataset for which classification is already known
test.doc <- dtm[c(3,4,7,8),] # Dataset you are trying to classify
Tags <- factor(c(rep("cars",2), rep("space",2))) # Tags - Correct answers for the training dataset
prob.test<- knn(train.doc, test.doc, Tags, k = 3, prob=TRUE) # k-number of neighbors considered
prob.test
# Display Classification Results
a <- c(3,4,7,8) #document ids
b <- prob.test #predicts by the algorithm
c <- attributes(prob.test)$prob #proportion of the votes for the winning class
result <- data.frame(Doc=a, Predict=b,Prob=c)
result

