#install and open libraries
install.packages("devtools")
install.packages("readr")
install.packages("wordcloud")
install.packages("quanteda")
install.packages("corpus")
install.packages("tm")
install.packages("syuzhet")
install.packages("zipfR")
install.packages ("ngram")
install.packages("knitr")


library(readr)
library(wordcloud)
library(corpus)
library(tm)
library(syuzhet)
library(quanteda)
library(zipfR)
library(ngram)
library(knitr)

#load the text into a data frame
text <- read_lines("text/TwentyThousandLeagues.txt")
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
freqTerms <- findFreqTerms(dtm,lowfreq=100)
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

#tokenization
tokens <- tokens(textCorpus$content)
tokens

#construct a dfm of the text
dfm <- dfm(tokens,toLower=TRUE)
dfm

#sentiment analysis
textdfcontent <- as.data.frame(textCorpus1$content)
textdfcontent

sentiment <- get_nrc_sentiment(textCorpus1$content)
sentiment

#analysis of sentiments, shows how many sentiments
#in a given sentence and how many of each type of sentiment
numSentiments <- rowSums(sentiment)
numSentiments

sentimentCount <- colSums(sentiment)
sentimentCount

#text weighing of dfm
weighteddfm <- docfreq(dfm)
weighteddfm

weights <- dfm_weight(dfm)
weights

tfidf <- dfm_tfidf(dfm,scheme_tf="count",scheme_df="inverse")
tfidf


# E - zipfr

# prepares the info for and create the frequency spectrum (spc)
testMattdm <- as.matrix(tdm)
tabMattdm <- as.matrix(table(testMattdm))
str(tabMattdm)

tabMattdm.df <- as.data.frame(tabMattdm)
tabMattdm.df
write.table(tabMattdm.df, file = "testTab.txt", sep = "\t",
            row.names = TRUE, col.names = FALSE)
tstTab <- read.delim("testTab.txt", header = FALSE, sep = "\t")
colnames(tstTab)[1:2] <-c("m","Vm")
write.table(tstTab, file = "test2Tab.txt", sep = "\t", 
            row.names = FALSE, col.names = TRUE)
# create spc
freq.spc <- read.spc("test2Tab.txt")

## analysis of spc 
# summary of the spc
summary(freq.spc)

# sample size
N(freq.spc)

# vocabulary size
V(freq.spc)

# Num of types per freq
Vm(freq.spc, 1)
Vm(freq.spc, 1:5)

# measure of productivity (num of hapaa legomena/ sample size)
Vm(freq.spc, 1)/ N(freq.spc)

# plot of the spc
plot(freq.spc)
plot(freq.spc, log ="x")

# scatterplot of the spc
with(freq.spc, plot(m, Vm, main="Frequency Spectrum"))

# F - NGram

# prepares the text for ngrams
sens <- read_lines("sampleSen.txt")
sens.df <- data.frame(sens)
sens.new <- vector()

for(i in sens) {
  senStr <- preprocess(i, case = "lower", remove.punct = TRUE)
  senStr
  sens.new <- c(sens.new, senStr)
}
sens.new

sens.size6 <- character(0)
for(j in sens.new) {
  sen.split <- strsplit(j, " ")[[1]]
  for(word in sen.split) {
    if (nchar(word) >= 7) {
      sens.size6 <- paste(sens.size6, word, sep = " ")
    }
  }
}
sens.size6

# Creates bigrams of all words in the 10
# sentences longer than 6 char long
sen2.ng <- ngram(sens.size6, n=2)
sen2.ng
print(sen2.ng, output="full")

# Creates trigrams of all words in the 10
# sentences longer than 6 char long
sen3.ng <- ngram(sens.size6, n=3)
sen3.ng
print(sen3.ng, output="full")

#TODO: 
#look into results for text weighing to finish parts a and c (Neel)
#look into why findAssocs function returns 0 under 'captain' (Neel)
#parts e, f, and g (Andrew and Fleu)
#parts b and d (Eptisam)
#push your code to git and remember to pull the repo before changing anything! (everyone)
#put your resuts in the report (everyone)
