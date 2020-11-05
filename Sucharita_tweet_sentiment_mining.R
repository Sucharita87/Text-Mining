# extract tweets and conduct sentiment analysis
library(tm)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
tweet<-read.csv("F://ExcelR//Assignment//Text Mining//Tweets.csv")
View(tweet)
class(tweet)
dim(tweet)
text<-as.character(tweet$text) # use just the text column
View(text)
length(text)
text<-Corpus(VectorSource(text))
class(text) # simple corpus, corpus class
inspect(text[1]) # View the first tweet 
inspect(text[10]) # view 10th tweet

# data cleaning
text1<-tm_map(text,tolower) # have to remove punctuation, numbers etc separately
text1<-tm_map(text1,removePunctuation)
text1<-tm_map(text1,removeNumbers)
text1<-tm_map(text1,removeWords, stopwords('english'))
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
text1 <- tm_map(text1, content_transformer(removeURL))
text1<-tm_map(text1, stripWhitespace)
inspect(text1[1]) 
inspect(text1[10])

# TDM/DTM
tdm<-TermDocumentMatrix(text1)
tdm
dtm<-t(tdm)
dtm
tdm<-as.matrix(tdm)
dim(tdm) # 4818, 2131
View(tdm[1:20, 1:10]) # selecting to view few rows and columns
#tdm1 <- TermDocumentMatrix(text1,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))#,stemming=T))
#dtm1<-t(tdm1)

# lOADING +VE AND -VE words  
pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt


# Making positive wordcloud function 
makeposwordc = function(tweet){
  freq = sort(rowSums(as.matrix(tweet)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}
# Positive word cloud - TF - Unigram
makeposwordc(tdm)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TF")


pos_words_bar_plot <- function(tweet){
  pos.matches = match(colnames(tweet), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(tweet, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(pos_words_freq,5), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}

# Frequency Barplot - Positive words - TFIDF - Unigram
pos_words_bar_plot(dtm) # thank, good, happy, great love



# Making negatice wordcloud function
makenegwordc = function(tweet){	
  freq = sort(rowSums(as.matrix(tweet)),decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  windows()
  wordcloud(names,freq_neg,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}


# Negative word cloud - TF - unigam
makenegwordc(tdm) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TF")


neg_words_bar_plot <- function(tweet){
  neg.matches = match(colnames(tweet), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(tweet, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,5), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}

# Frequency Barplot -negative words - Unigram - TF
neg_words_bar_plot(dtm) # hard, sorry, sad, shocked, break


#plots
count<-rowSums(tdm) # text plot of frequency of words repeated in each row
count_high<-subset(count, count>=25) # consider those words which are repeated >=25 times
barplot(count_high, las=2, col = rainbow(150))

count_max<-subset(count,count>=80) # max repeated words
count_max
barplot(count_max, las=2, col = rainbow(150))

count_small <- subset(count, count >= 10)
count_small
barplot(count_small, las=2, col = rainbow(30))

# wordcloud
count_all <- sort(rowSums(tdm), decreasing = TRUE)
count_all
wordcloud(words = names(count_high), freq = count_high)
wordcloud(words = names(count_all), freq = count_all)
wordcloud(words = names(count_high), freq = count_high, random.order = F, colors = rainbow(50))

# create df, using wordcloud2 library
df <- data.frame(names(count_small), count_small) # dataframe of word repeated >=10 times
colnames(df) <- c('word', 'freq')
View(df)
write.csv(df, "df.csv")
getwd() # get the saved path of the above csv file

# sentiment mining

install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)

emotion = readLines("F://ExcelR//Assignment//Text Mining//Tweets.csv")
x <- iconv(emotion, "UTF-8")

sentiment <- get_nrc_sentiment(x)
head(sentiment)

x[5] # for 5th tweet check the below mentions sentiment count
get_nrc_sentiment('unfriendly')
get_nrc_sentiment('happy')
barplot(colSums(sentiment), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')


