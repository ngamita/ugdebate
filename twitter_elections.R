library(ggplot2)
library(lubridate)
library(scales)

# Get the files names
setwd('~/UGDebate16v2_announce/')
files = list.files(pattern="*.csv")

# First apply read.csv, then rbind
tweets= do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

# Convert time zone to EAT, API dumps the tweets in GMT. 
tweets$timestamp <- ymd_hms(tweets$created)
tweets$timestamp <- as.POSIXct(tweets$timestamp, tz="Africa/Nairobi")
# tweets$timestamp <- with_tz(tweets$timestamp, "Uganda/Kampala")
# https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
tweets$timestamp <- format(tweets$timestamp, tz="Africa/Nairobi",usetz=TRUE)

ggplot(data = tweets, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

ggplot(data = tweets, aes(x = wday(timestamp, label = TRUE))) +
  geom_histogram(breaks = seq(0.5, 7.5, by =1), aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Day of the Week") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Visualise the timelines tweet, convert/create to timeonly column. 
tweets$timeonly <- as.numeric(tweets$timestamp - trunc(tweets$timestamp, 'days'))

#Testing all tweets got timestamps. 
tweets[(minute(tweets$timestamp) == 0 & second(tweets$timestamp) == 0),11] <- NA
mean(is.na(tweets$timeonly))


# Visualize the tweets over hours + mins 
# TODOL FIX Richard Ngamita
class(tweets$timeonly) <- "POSIXct"
ggplot(data = tweets, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..), binwidth=0.1) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_x_datetime(breaks = date_breaks("3 hours"), 
                   labels = date_format("%H:00")) +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Hashtag usage, retweets , mentions etc 
ggplot(tweets, aes(factor(grepl("#", tweets$text)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets with Hashtags") +
  scale_x_discrete(labels=c("No hashtags", "Tweets with hashtags"))


# Measure of Originality. 
# Speaking of retweets, one of the columns in the CSV file from 
#Twitter codes whether the tweet is a retweet or not, so it is not tough
# to see how many tweets are retweeted vs. original content.

ggplot(tweets, aes(factor((isRetweet)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Retweeted Tweets") +
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))

# now let’s look at the replying habits. 
# There is another column in the CSV file that codes whether the tweet is in reply to another tweet.

ggplot(tweets, aes(factor(!is.na(replyToSID)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Replied Tweets") +
  scale_x_discrete(labels=c("Not in reply", "Replied tweets"))



# Re-categorizing tweets. 
tweets$type <- "tweet"
tweets[(tweets$isRetweet),20] <- "RT" # remove !is.na incase of issues.
tweets[(!is.na(tweets$replyToSID)),20] <- "reply"
tweets$type <- as.factor(tweets$type)
tweets$type = factor(tweets$type,levels(tweets$type)[c(3,1,2)])








# Habits across 
ggplot(data = tweets, aes(x = timestamp, fill = type), bins = 200) +
  geom_histogram() +
  xlab("Time") + ylab("Number of tweets") +
  scale_fill_manual(values = c("midnightblue", "deepskyblue4", "aquamarine3"))


ggplot(data = tweets, aes(x = timestamp, fill = type)) +
  geom_bar(position = "fill") +
  xlab("Time") + ylab("Proportion of tweets") +
  scale_fill_manual(values = c("midnightblue", "deepskyblue4", "aquamarine3"))


# Characters in each tweet. 
tweets$charsintweet <- sapply(tweets$text, function(x) nchar(iconv(x)))

ggplot(data = tweets, aes(x = charsintweet)) +
  geom_histogram(aes(fill = ..count..), binwidth = 1) +
  theme(legend.position = "none") +
  xlab("Characters per Tweet") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")



# nnly clean Original tweets

table(tweets$type)
tweets[tweets$type == 'tweet',]
o_tweets <- tweets[tweets$type == 'tweet',]
# dim(o_tweets)
tweets <- o_tweets

library(tm)
library(stringr)
library(wordcloud)
library(SnowballC)

# u_tweets <- read.csv('u_tweets_2.csv', stringsAsFactors=FALSE)
u_txt <- tweets$text


# remove retweet entities
u_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", u_txt)

# remove at people
u_txt = gsub("@\\w+", "", u_txt)

# remove punctuation
u_txt = gsub("[[:punct:]]", "", u_txt)

# remove numbers
u_txt = gsub("[[:digit:]]", "", u_txt)

# remove html links
u_txt = gsub("http\\w+", "", u_txt)

# remove unnecessary spaces
u_txt = gsub("[ \t]{2,}", "", u_txt)
u_txt = gsub("^\\s+|\\s+$", "", u_txt)

# Define error handling function. 
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

# lower case using try.error with sapply
u_txt = sapply(u_txt, catch.error)

# remove NAs in some_txt
u_txt = u_txt[!is.na(u_txt)]
names(u_txt) = NULL

if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)


# Run Naive Bayes classifier on tweets. 
# Perform Sentiment Analysis
# classify emotion
class_emo = classify_emotion(u_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(u_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# Create data frame with the results and obtain some general statistics
# data frame with results
sent_df = data.frame(text=u_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


dev.off()
# Let’s do some plots of the obtained results
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets UGDebate16 \n(classification by emotion)") +
  theme(plot.title = element_text(size=12, face="bold"))

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets #UGDebate16 \n(classification by polarity)") +
  theme(plot.title = element_text(size=12, face="bold"))

# Separate the text by emotions and visualize the words with a comparison cloud
# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = u_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
#emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tm_map(corpus, removePunctuation)
tm_map(corpus, removeWords, c(stopwords("english"),"you","the","for"), lazy=TRUE) 
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

comparison.cloud(tdm, random.order=FALSE,colors = c("#00B2FF", "red","#FF0099","#6600CC"), max.words=500)


pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)
wordcloud(words = corpus, scale=c(5,0.1), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

#friends <- str_extract_all(u_txt, "@\\w+")
#namesCorpus <- Corpus(VectorSource(friends))

#set.seed(146)
#wordcloud(words = namesCorpus, scale=c(3,0.5), max.words=100, random.order=FALSE, 
#          rot.per=0.10, use.r.layout=FALSE, colors=pal)







tweets <- read.csv('pre-election_17_02_2016_12pm')
tweets <- read.csv('u_tweets_9pm_2.csv')

f <- file.choose()
d <- read.csv(f)
tweets <- d







library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )
tweets$text <- iconv(tweets$text,"WINDOWS-1252","UTF-8")
mySentiment <- get_nrc_sentiment(tweets$text)
head(mySentiment)
tweets <- cbind(tweets, mySentiment)


# Total sentiment scores as per 8 emotions. 

sentimentTotals <- data.frame(colSums(tweets[,c(22:29)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

# sentimentTotals<-arrange(sentimentTotals, desc(sentimentTotals$count))

ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")


# Count screen name. Who the top people tweeting at the UGDebates16 
counts=table(tweets$screenName)
barplot(counts)

# Let's do something hacky:
# Limit the data set to show only folk who tweeted twice or more in the sample
cc=subset(counts,counts>100)
cc_df <- as.data.frame(as.table(cc))
names(cc_df) <- c('screenname', 'counttweets')
#barplot(cc,las=2,cex.names =0.3, main="Top tweeps", 
#xlab="Tweeps", ylab="Total tweets")
#candidatesdf<-arrange(candidatesdf,desc(mentions))


cc_df<-arrange(cc_df,desc(counttweets))
cc_df <- cc_df[1:30,]
barplot(cc_df$counttweets, las=2, names.arg=cc_df$screenname, axes=FALSE)
barplot(cc_df$counttweets, las=2, cex.names=0.8, names.arg=cc_df$screenname, axes=TRUE)

# Arrange in Descending. 
library(dplyr)
library(tidyr)
#supposing you want to arrange column 'c' in descending order and 'd' in ascending order. name of data frame is df
## first doing descending
df<-arrange(cc_df,desc(counttweets))
df <- df[1:30, ]

barplot(df$counttweets, las=2, names.arg=df$screenname, axes=FALSE)
barplot(df$counttweets, las=2, cex.names=0.8, names.arg=df$screenname, axes=TRUE)













library(SnowballC)

## Option 1: retrieve tweets from Twitter
library(twitteR)
library(tm)



# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets$text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy=TRUE)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation, lazy=TRUE) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers, lazy=TRUE)

# remove funny alnum shit.
#removelALNUM <- function(x) gsub("[^[:alnum:]///' ]", "", x)

# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL), lazy=TRUE)  #??
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via", "you")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("ugdebate", "uganda"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords, lazy=TRUE)
#

myCorpus <- tm_map(myCorpus, PlainTextDocument)
#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
#myCorpus <- tm_map(myCorpus, stemDocument, lazy=TRUE)



# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(myCorpus[[i]]))
}

#myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)


#mbabaziCases <- tm_map(myCorpusCopy,grep, pattern = "\\<mbabazi")
#sum(unlist(miningCases))

## count frequency of "miners"
#minerCases <- tm_map(myCorpusCopy, grep, pattern = "\\<miners")
#sum(unlist(minerCases))


# # replace "miners" with "mining"
# myCorpus <- tm_map(myCorpus, gsub, pattern = "miners", replacement = "mining")

#tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

## Freqency words and Association
idx <- which(dimnames(tdm)$Terms == "ugandadecides")
inspect(tdm[idx + (0:5), 101:110])

(freq.terms <- findFreqTerms(tdm, lowfreq=15))


term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=5)
df <- data.frame(term = names(term.freq), freq = term.freq)

#ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()

findAssocs(tdm, "mbabazi", 0.2)


# Word Associations. 
library(graph)
library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.12, weighting = T)

# Errors


library(wordcloud)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)




# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D2")

plot(fit)
rect.hclust(fit, k = 10) # cut tree into 6 clusters 




m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

library(fpc)









dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm <- dtm[rowTotals> 0, ]   

lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 4) # first 4 terms of every topic
term

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
require(data.table) #fore IDate

topic <- topics(lda, 1)

tweets$timestamp <- ymd_hms(tweets$created)
tweets$timestamp <- as.POSIXct(tweets$timestamp, tz="Africa/Nairobi")
# tweets$timestamp <- with_tz(tweets$timestamp, "Uganda/Kampala")
# https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
tweets$timestamp <- format(tweets$timestamp, tz="Africa/Nairobi",usetz=TRUE)
# Make sure you already converted tz -> timestamp
topics <- data.table(date=as.POSIXct(tweets$timestamp), topic)
#dt = data.table(idate1=as.ITime(tweets$created))

qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")






# Re-categorizing tweets per candidate. 
# Create per candidate columns to avoid overwrites. 
library(stringr)
tweets$besigye <- FALSE
tweets[(str_detect(tweets$text, c('besigye'))),19] <- TRUE

tweets$museveni <- FALSE
tweets[(str_detect(tweets$text, c('museveni'))),20] <- TRUE

tweets$mbabazi <- FALSE
tweets[(str_detect(tweets$text, c('mbabazi'))),21] <- TRUE

tweets$mabirizi <- FALSE
tweets[(str_detect(tweets$text, c('mabirizi'))),22] <- TRUE

tweets$kyalya <- FALSE
tweets[(str_detect(tweets$text, c('kyalya'))),23] <- TRUE

tweets$bwanika <- FALSE
tweets[(str_detect(tweets$text, c('bwanika'))),24] <- TRUE

tweets$biraaro <- FALSE
tweets[(str_detect(tweets$text, c('biraaro'))),25] <- TRUE

tweets$baryamureeba <- FALSE
tweets[(str_detect(tweets$text, c('baryamureeba'))),26] <- TRUE






