#install and open libraries
install.packages("devtools")
install.packages("readr")
install.packages("wordcloud")
install.packages("corpus")
install.packages("tm")
install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("NLP")
install.packages("tokenizers")

library(readr)
library(wordcloud)
library(corpus)
library(tm)
library(syuzhet)
library(tokenizers)
library(stringr)



#load the text into a data frame
text <- read_lines("TwentyThousandLeagues.txt")
textdf <- data.frame(text)

# Create VCorpus of the text and inspect it
textCorpus <- VCorpus(DirSource("/Users/eptisam/Big Data Text/Big-Data-Text-Analytics-Project",ignore.case = TRUE,mode="text"))
textCorpus

inspect(textCorpus)
str(textCorpus)

#extract the document
textCorpus1 <- textCorpus[[3]]
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

#b Find the ten longest word and ten longest sentence
data <- data.frame(text = sapply(textCorpus1, as.character), stringsAsFactors = FALSE)
#Convert Corpus into data frame

LongestWordsList<-data.frame(Row=c(1,2,3,4,5,6,7,8,9,10))
#Create a dataframe to store the ten longest word

words <- tokenize_words(data[[1]])
longestWordLength = 0;
longestWord = '';

#seperates the data frame into words. Iterates the text file to find the largest one
#Does this ten times to get the the ten largest words
for(a in 1:11){
  for (i in 1:length(words)){
    if(length(tokenize_words(words[[i]])!=0)){
      words2<- tokenize_words(words[[i]])
      for (x in 1:length(words2)){
        if ((str_length(words2[[x]])>longestWordLength) && (is.element(words2[[x]],unlist(LongestWordsList))==FALSE)){
          longestWordLength= str_length(words2[[x]])
          longestWord=words2[[x]]
          LongestWordsList[a,"Row"]<-longestWord
        }
      }
    } 
  }
  longestWordLength = 0;
  longestWord = '';
}
#prints the ten largest words
LongestWordsList

#Finding the ten largest sentences
SentencesList<-data.frame(Row=c(1,2,3,4,5,6,7,8,9,10))
bdy2 <- paste(data[[1]], collapse=" ")
Sentence <- tokenize_sentences(bdy2)
Sentence2<- tokenize_sentences(Sentence[[1]])

longestSentLength = 0;
longestSent = '';

#seperates the data frame into sentences Iterates the text file to find the largest one
#Does this ten times to get the the ten largest sentences
for(a in 1:11){
  for (i in 1:length(Sentence[[1]])){
    var <- tokenize_words(Sentence2[[i]])
    if (length(var[[1]])>longestSentLength && (is.element(Sentence2[[i]], unlist(SentencesList))==FALSE)){
      #cat(Sentence2[[i]])
     # cat("                           ")
      longestSentLength=length(var[[1]])
      longestSent=Sentence2[[i]]
      SentencesList[a,"Row"]<-longestSent
    }
  }
  longestSentLength = 0;
  longestSent = '';
}
SentencesList

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

