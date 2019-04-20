#install.packages("tm")
#install.packages("SnowballC")
#install.packages("pdftools") #for reading pdfs

library(tm)#Framework for textmining.
library(SnowballC)#Provides wordStem() for stemming.

Doc1.Train.Source <- DirSource(paste(getwd(),"/DemoData/News2/20news-bydate-train/sci.space",sep=""))
Doc1.Train <- Corpus(URISource(Doc1.Train.Source$filelist[1:100]), readerControl=list(reader=readPlain))

Doc2.Train.Source <- DirSource(paste(getwd(),"/DemoData/News2/20news-bydate-train/rec.autos",sep=""))
Doc2.Train <- Corpus(URISource(Doc1.Train.Source$filelist[1:100]), readerControl=list(reader=readPlain))

Doc1.Test.Source <- DirSource(paste(getwd(),"/DemoData/News2/20news-bydate-test/sci.space",sep=""))
Doc1.Test <- Corpus(URISource(Doc1.Train.Source$filelist[1:100]), readerControl=list(reader=readPlain))

Doc2.Test.Source <- DirSource(paste(getwd(),"/DemoData/News2/20news-bydate-test/rec.autos",sep=""))
Doc2.Test <- Corpus(URISource(Doc1.Train.Source$filelist[1:100]), readerControl=list(reader=readPlain))

# merge 4 corpus
merge.corpus <- c(Doc1.Train,Doc1.Test,Doc2.Train,Doc2.Test)
merge.corpus

#Preprocessing
# Convert to lower case
merge.corpus.tranf <- tm_map(merge.corpus, content_transformer(tolower))
# Remove Punctuation
merge.corpus.tranf <- tm_map(merge.corpus.tranf, removePunctuation) 
# Remove stop words
merge.corpus.tranf <- tm_map(merge.corpus.tranf, removeWords, stopwords("english"))
# specify your stopwords as a character vector
merge.corpus.tranf <- tm_map(merge.corpus.tranf, removeWords, c("since", "let", "yes", "every", "yeah")) 
# stem
#merge.corpus.tranf <- tm_map(merge.corpus.tranf, stemDocument, language = "english")# Perform Stemming
#dtm <- as.matrix(DocumentTermMatrix(merge.corpus.tranf)) # Document term matrix
#dtm

# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- 5
dtm = DocumentTermMatrix(merge.corpus,
                         control = list(
                           wordLengths=c(2, Inf),
                           bounds = list(global = c(minTermFreq, Inf))
                         ))
dtm.matrix = as.matrix(dtm)


train.doc <- dtm.matrix[c(1:100,201:300),]
test.doc <- dtm.matrix[c(101:200,301:400),]
Tags <- factor(c(rep("Sci",100), rep("Rec",100)))

dim(train.doc)
dim(test.doc)
length(Tags)

library(class) # Using kNN 

prob.test<- knn(train.doc, test.doc, Tags, k = 3, prob=TRUE)
prob.test

# Display Classification Results
a <- c(1:400) #document ids
b <- prob.test #predicts by the algorithm
c <- attributes(prob.test)$prob #proportion of the votes for the winning class
d <- c(rep("Sci", 100), rep("Rec", 100)) # comparison between predicted & actual 

result <- data.frame(Doc=a, Predict=b,Prob=c,Correct= (b == d))
result







