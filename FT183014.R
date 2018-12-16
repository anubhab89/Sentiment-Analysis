memory.limit(10000)
#Check R version
#R.version
#Load Library
library(SnowballC)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
library(dplyr)
library(syuzhet)
library(plyr)
library(grid)
library(gridExtra)
#Set Path
setwd("C:\\Users\\Administrator\\Desktop")
#Connect to twitter
require(twitteR)
require(RCurl)
consumer_key <- 'qphOKHTyzytPVVcaex4BrZ4ZO'
consumer_secret <- 'SitJMkcLnLUEvirvGtYPsuD5kgdWqFZ4EerLVJSThxc7CRkJrC'
access_token <- '161544752-HHH6j4MKpk5SVRYQ5POHRLPFUfUbMHjhMhYAQOL7'
access_secret <- 'Okd9rQoRWmetiGK0D338NMaFJnvuL96hLSG5qRfulvZOu'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
data_bse <- searchTwitter("#bse",n=2500,since = "2012-01-01", lang="en")
length.df_bse <- length(data_bse)
length.df_bse
data_bse.df<-ldply(data_bse,function(t) t$toDataFrame())


#Formatting
#Formatting date

data_bse.df$created<-as.Date(data_bse.df$created)

#Formatting text
d_text = sapply(data_bse,function(x)x$getText())
#Remove people name
d_text1 <- gsub("@[^[:space:]]*", "", d_text)  
#Remove HTML
d_text2 <-  gsub("http[^[:space:]]*", "", d_text1)  
#Remove Anything except English Language and Space
d_text3 <- gsub("[^[:alpha:][:space:]]*", "", d_text2)   
#Some more data cleaning
d_text4 <- gsub("RT","",d_text3)
d_text5 <- gsub("\n","",d_text4)


#write to csv
data=data.table("Tweet"= d_text5, "created" = data_bse.df$created) 
write.csv(data,"bse_tweets.csv")

#Load excel file
tweets.df <- read.csv("bse_tweets.csv")
names(tweets.df)
str(tweets.df)
tweets.df$Tweet <- as.character(tweets.df$Tweet)
tweets.df$X = NULL
#Data Cleanising
#Build Corpus
myCorpus<- Corpus(VectorSource(tweets.df$Tweet)) 
#convert to LowerCase
myCorpus <- tm_map(myCorpus,tolower)
#remove stopwords
myCorpus <- tm_map(myCorpus,removeWords, c(stopwords("english"),"kickoutcancer","stocks","stockmarket","bcanawareness","thakur","shri","going","ajay","spselva","amp","rose","cueswe","madampceo","cancerawareness","breast","breastcancer","see","fut","providing","just","feb","find","get","fut","bourses","bse","can","one","comes","mdampceo","take","face","dewan","cro","shasun","guys"))
#remove numbers
myCorpus <- tm_map(myCorpus,removeNumbers)
#remove Punctuation
myCorpus <- tm_map(myCorpus,removePunctuation)
#remove whitespace
myCorpus <- tm_map(myCorpus,stripWhitespace)
myCorpusCopy<- myCorpus
#find most frequent term
dtm <- TermDocumentMatrix(myCorpus)
(freq.terms <- findFreqTerms(dtm, lowfreq = 30))
term.freq <- rowSums(as.matrix(dtm))
term.freq <- subset(term.freq, term.freq > 30)
df <- data.frame(term = names(term.freq), freq= term.freq)

#Plotting frequency of most used term
ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts")) 

#plotting the word cloud
dtm <- DocumentTermMatrix(myCorpus)
m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing=TRUE)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word,d$freq,min.freq=10, colors=rainbow(7))

#Find association with a specific keyword in the tweets

list1<- findAssocs(dtm, "nifty", 0.21)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
#corrdf1

list2<- findAssocs(dtm,"nse",0.2)
corrdf2 <- t(data.frame(t(sapply(list2,c))))
#corrdf2

list3<- findAssocs(dtm,"market",0.23)
corrdf3 <- t(data.frame(t(sapply(list3,c))))
#corrdf3

#list4<- findAssocs(dtm,"inr",0.33)
#corrdf4 <- t(data.frame(t(sapply(list4,c))))
#corrdf4

#list5<- findAssocs(dtm,"bank",0.2)
#corrdf5 <- t(data.frame(t(sapply(list5,c))))
#corrdf5

list6<- findAssocs(dtm,"nasdaq",0.24)
corrdf6 <- t(data.frame(t(sapply(list6,c))))
#corrdf6

list7<- findAssocs(dtm,"tips",0.2)
corrdf7 <- t(data.frame(t(sapply(list7,c))))
#corrdf7

list8<- findAssocs(dtm,"pnb",0.26)
corrdf8 <- t(data.frame(t(sapply(list8,c))))
#corrdf8

#Barplot
barplot(t(as.matrix(corrdf1)),beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "NIFTY",border = "black")
#barplot(t(as.matrix(corrdf2)),beside=TRUE,xlab = "Words",ylab = "Corr",col = "red",main = "NSE",border = "black")
barplot(t(as.matrix(corrdf3)),beside=TRUE,xlab = "Words",ylab = "Corr",col = "yellow",main = "MARKET",border = "black")
#barplot(t(as.matrix(corrdf4)),beside=TRUE,xlab = "Words",ylab = "Corr",col = "yellow",main = "INR",border = "black")
barplot(t(as.matrix(corrdf8)),beside=TRUE,xlab = "Words",ylab = "Corr",col = "yellow",main = "PNB",border = "black")
barplot(t(as.matrix(corrdf6)),beside=TRUE,xlab = "Words",ylab = "Corr",col = "yellow",main = "NASDAQ",border = "black")
barplot(t(as.matrix(corrdf7)),beside=TRUE,xlab = "Words",ylab = "Corr",col = "yellow",main = "TIPS",border = "black")

#Topic Modelling to identify latent/hidden topics using LDA technique
tdm <- as.DocumentTermMatrix(dtm)
rowTotals <- apply(tdm , 1, sum)
NullDocs <- tdm[rowTotals==0, ]
tdm   <- tdm[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  tweets.df <- tweets.df[-as.numeric(NullDocs$dimnames$Docs),]
}

lda <- LDA(tdm, k = 5) # find 5 topic
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
topics<- topics(lda)
topics<- data.frame(date=(tweets.df$created), topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack")

#Sentiment Analysis: understanding emotional valence in tweets using syuzhet
mysentiment<-get_nrc_sentiment((tweets.df$Tweet))
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)
xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness",
           "Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis)
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional valence", ylab = "Score", main = "Twitter sentiment", 
        sub = "BSE", col = colors, border = "black", xpd = F, ylim = yRange,
        axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")

mysentimentvalues <- data.frame(get_sentiment((tweets.df$Tweet)))
colnames(mysentimentvalues)<-"polarity"
mysentimentvalues$date <- tweets.df$created
result <- aggregate(polarity ~ date, data = mysentimentvalues, sum)
names(result)
result[0]=NULL
result
plot(result$date,result$polarity,order(result$date),type='l')
result1 <- aggregate(polarity ~ date, data = mysentimentvalues, mean)
plot(result1, type = "l")


