install.packages("SnowballC")
install.packages("tm")
install.packages("twitteR")
install.packages("syuzhet")
install.packages("httr")
install.packages("ROAuth")
install.packages("httpuv")
install.packages("openssl")
install.packages("base64enc")
install.packages("devtools")
install.packages("wordcloud2")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("corpus")
install.packages("RColorBrewer")
install.packages("XQuartz")
library(XQuartz)

library(SnowballC)
library(tm)
library(twitteR)
library(syuzhet)
library(httr)
library(httpuv)
library(openssl)
library(base64enc)
library(devtools)
library(wordcloud2)
library(RColorBrewer)

## Connecting to Twitter APIs

consumer_key <- "bMXLK2PbP0paYlAeKvBWCYpSA"
consumer_secret <- "Dk9gOBDhMx4SfdbjN9k5xfVPbL0QNYMoD1p8IOJOU0zlxiyoNv"

access_token<- "286503514-ZYAYpu8eT4CUBU63hINjxMCfxFkRqyvbKV7hqhdn"
access_secret<- "bZHRU6zVMuL6XavRjilnIOlZl69xwXB8SSMClM2sL1YGT"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
##--------------------------------------------------------------------------------

ST_tweets<- searchTwitter("Government Reparations", n = 75, since = "2018-01-01")

ST_tweets.df <- twListToDF(ST_tweets)

write.csv(ST_tweets.df, file = "ShotTracker_Tweets.csv")

## Get only Text

get_ST_txt<- sapply(ST_tweets, function(x) x$getText())

head(get_ST_txt)

## Clean Text/Remove html links


ST_tweets_clean <- gsub("(RT|via)((?:\\b\\W*\\@\\w+)+)","", get_ST_txt )


ST_tweets_clean <- gsub("http[^[:blank:]]+", " ", ST_tweets_clean )

ST_tweets_clean <- gsub("[^[:alnum:]]", " ", ST_tweets_clean) 

head(ST_tweets_clean)

write.csv(ST_tweets_clean, "ST_tweets.csv")

ST_twt_data<- ST_tweets_clean

## Creating Wordcorpus and Cleaning

library(corpus)

ST_twt_data <- VCorpus(VectorSource(ST_twt_data))
ST_twt_data <- tm_map(ST_twt_data, removePunctuation)
ST_twt_data <- tm_map(ST_twt_data, content_transformer(tolower))
ST_twt_data <- tm_map(ST_twt_data, stripWhitespace)
ST_twt_data <- tm_map(ST_twt_data, removeWords, stopwords("english"))

head(ST_twt_data)

#Build WordCloud 

library(wordcloud2)
pallet <- brewer.pal(0, "Accent")

wordcloud(ST_twt_data, min.freq = 5, max.words = Inf, width = 1000, height = 1000, 
          random.order = FALSE, colors = pallet )


#Get Sentiment 

library(syuzhet)

ST_sntmt <- get_nrc_sentiment(ST_tweets_clean)

ST_sntmt

ST_sntmt.df<- data.frame(colSums(ST_sntmt[,]))
View(ST_sntmt.df)


names(ST_sntmt.df)<- "Scores"

ST_sntmt.df <-cbind("Sentiment" = rownames(ST_sntmt.df),ST_sntmt.df)

rownames(ST_sntmt.df)<- NULL

View(ST_sntmt.df)

# Plot data
library(ggplot2)

plot_sentiment<-ggplot(ST_sntmt.df, aes(x=Sentiment, y=Scores)) + geom_bar(aes(fill= Sentiment), stat = 'Identity') + xlab("Sentiment") + ylab("Scores") + ggtitle("Sentimental Analysis")

plot_sentiment


##plot_sentiment <- ggplot(data=ST_sntmt.df, aes(x=Sentiment, y=Scores)) 
## + geom_bar(aes(fill= Sentiment), stat='Identity') + 
##theme(legend.position = "none") + xlab("Sentiment") 
##+ ylab("Scores") + ggtitle("Sentimental Analysis")