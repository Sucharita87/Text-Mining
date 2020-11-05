install.packages(RColorBrewer)
library("RColorBrewer")
display.brewer.all()
library(readtext)
library(tm)
library(wordcloud)
library(wordcloud2)
samsung <-read.table(file= "F://ExcelR//Assignment//Text Mining//samsung.txt", header = TRUE , sep = " ", stringsAsFactors = FALSE,col.names= c("id", "review"))
class(samsung)
View(samsung)
dim(samsung)
review<-as.character(samsung$review)
review<-Corpus(VectorSource(review))
class(review)
inspect(review[5])
inspect(review[19])

#data cleaning
review1<-tm_map(review, tolower)
review1<-tm_map(review1, removeNumbers)
review1<-tm_map(review1, removePunctuation)
review1<-tm_map(review1, removeWords, stopwords('english'))
review1<-tm_map(review1, stripWhitespace)
review1<-tm_map(review1, removeWords, 'samsung') # as it is  asamsung review, so can exclude this word
# shortWordRemover <- function(x) gsub('\\b\\w{1,5}\\b','',x)
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
review1<-tm_map(review1, removeURL)
inspect(review1[5])
inspect(review1[19])

#tdm/dtm
tdm<-TermDocumentMatrix(review1)
tdm
dtm<-t(tdm)
tdm<-as.matrix(tdm)
dim(tdm)
View(tdm[1:20,1:10])

#plots
count<-rowSums(tdm)
count
count_high<-subset(count, count>=5) # iterated count with >15, >10, >5, to get better feedbacks
count_high
barplot(count_high, las = 2, col = topo.colors(5), horiz = TRUE)

#wordcloud
count_all<-sort(rowSums(tdm), decreasing = TRUE)
count_all        
wordcloud(words = names(count_all), freq = count_all, colors = topo.colors(5))
wordcloud(words = names(count_high), freq = count_high, random.order = F, colors = topo.colors(5))

#create df
sam_review<-data.frame(names(count_all),count_all)
colnames(sam_review)<- c('word', 'freq')
View(sam_review)
write.csv(sam_review, "sam_review.csv")
getwd()

# emotion mining
install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)
emotion<-readLines("F://ExcelR//Assignment//Text Mining//sam_review.csv")
x<-iconv(emotion, "UTF-8")
sentiment<-get_nrc_sentiment(x)
head(sentiment)
barplot(colSums(sentiment), las = 2, col = topo.colors(20), horiz = TRUE, xlab = 'Count', main = 'Emotion scores')
