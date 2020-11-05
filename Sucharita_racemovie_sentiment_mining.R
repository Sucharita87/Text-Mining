install.packages(RColorBrewer)
library("RColorBrewer")
display.brewer.all()
library(readtext)
library(tm)
library(wordcloud)
library(wordcloud2)
race <-read.table(file= "F://ExcelR//Assignment//Text Mining//race.txt", header = TRUE , sep = "", stringsAsFactors = FALSE,col.names= c("id", "review"))
class(race)
View(race)
dim(race)
review<-as.character(race$review)
review<-Corpus(VectorSource(review))
class(review)
inspect(review[1])
inspect(review[37])

#data cleaning
review1<-tm_map(review, tolower)
review1<-tm_map(review1, removeNumbers)
review1<-tm_map(review1, removePunctuation)
review1<-tm_map(review1, removeWords, stopwords('english'))
review1<-tm_map(review1, stripWhitespace)
# shortWordRemover <- function(x) gsub('\\b\\w{1,5}\\b','',x)
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
review1<-tm_map(review1, removeURL)
inspect(review1[1])
inspect(review1[37])

#tdm/dtm
tdm<-TermDocumentMatrix(review1)
tdm
dtm<-t(tdm)
tdm<-as.matrix(tdm)
dim(tdm)
View(tdm[1:20,1:20])

#plots
count<-rowSums(tdm)
count
count_high<-subset(count, count>=5) # iterated count with >15, >10, >5, to get better feedbacks
count_high
barplot(count_high, las = 2, col = topo.colors(5), horiz = TRUE, xlab = "count", main = "RACE movie review")

#wordcloud
count_all<-sort(rowSums(tdm), decreasing = TRUE)
count_all        
wordcloud(words = names(count_all), freq = count_all, colors = topo.colors(5))
wordcloud(words = names(count_high), freq = count_high, random.order = F, colors = topo.colors(5))

#create df
race_review<-data.frame(names(count_all),count_all)
colnames(race_review)<- c('word', 'freq')
View(race_review)
write.csv(race_review, "race_review.csv")
getwd()

# emotion mining
install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)
emotion<-readLines("F://ExcelR//Assignment//Text Mining//race_review.csv")
x<-iconv(emotion, "UTF-8")
sentiment<-get_nrc_sentiment(x)
head(sentiment)
barplot(colSums(sentiment), las = 2, col = topo.colors(20), horiz = TRUE, xlab = 'Count', main = 'Emotion scores')
