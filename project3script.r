#install and open libraries
install.packages("devtools")
install.packages("readr")
install.packages("wordcloud")
install.packages("corpus")
install.packages("tm")
install.packages("syuzhet")
library(readr)
library(wordcloud)
library(corpus)
library(tm)
library(syuzhet)


#load the text into a data frame
text <- read_lines("TwentyThousandLeagues.txt")
textdf <- data.frame(text)

# Create VCorpus of the text and inspect it
textCorpus <- VCorpus(DirSource("text/",ignore.case = TRUE,mode="text"))
textCorpus

inspect(textCorpus)
str(textCorpus)

#extract the document
textCorpus1 <- textCorpus[[1]]
textCorpus1

inspect(textCorpus1)
str(textCorpus1)

#get the document term matrix
dtm <- DocumentTermMatrix(textCorpus)
dtm
inspect(dtm)
str(dtm)

#get the term frequency
tf <- termFreq(textCorpus1)
tf
str(tf)

#convert to lowercase
textCorpus <- tm_map(textCorpus,content_transformer(tolower))
textCorpus
inspect(textCorpus)



#remove punctuation
removePunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
textCorpus <- tm_map(textCorpus,content_transformer(removePunct))


#extract the document which is now lowercase and no punctuation
textCorpus1 <- textCorpus[[1]]
textCorpus1

inspect(textCorpus1)
str(textCorpus1)

#get the new document term matrix
dtm <- DocumentTermMatrix(textCorpus)
dtm
inspect(dtm)
str(dtm)

#remove stopwords
stopwords <- c(tm::stopwords('english'))
stopwords
textCorpus <- tm_map(textCorpus,removeWords,stopwords)

#extract the document which is now with no stopwords
textCorpus1 <- textCorpus[[1]]
textCorpus1$content

inspect(textCorpus1)
str(textCorpus1)

#get the new document term matrix
dtm <- DocumentTermMatrix(textCorpus)
dtm
inspect(dtm)
str(dtm)

#find frequent terms
freqTerms <- findFreqTerms(dtm,lowfreq=25)
freqTerms


#find words associated with others
captainAssoc <- findAssocs(dtm,"ship",0.1)
captainAssoc

#get the term document matrix
tdm <- TermDocumentMatrix(textCorpus)
tdm
inspect(tdm)
str(tdm)

#find the summed term frequency of text
stoptf <- rowSums(as.matrix(tdm))
stoptf

stoptfsub <- subset(stoptf, stoptf >= 60)
stoptfsub

#remove sparse terms
tdm <- removeSparseTerms(tdm,sparse=0.5)
tdm
inspect(tdm)
str(tdm)

#create a dendrogram
distMatrix <- dist(tdm)
dendrogram <- hclust(distMatrix,method="ward.D2")
plot(dendrogram)

#sort frequent terms
stoptf <- sort(stoptf,decreasing = TRUE)
stoptf

#create wordcloud
colpal <- brewer.pal(9,"BuGn")
wordcloud(words = names(stoptf),freq=stoptf,min.freq=10,random.order=F,colors=colpal)

#find column frequencies
coltf <- colSums(as.matrix(dtm))
coltf
#order by frqequencies in descending order
order <- order(coltf,decreasing=TRUE)

#find the most and least frequent terms
coltf[head(order)]
coltf[tail(order)]

#sentiment analysis
textdfcontent <- as.data.frame(textdf$text)
textdfcontent

sentiment <- get_nrc_sentiment(textdfcontent)

