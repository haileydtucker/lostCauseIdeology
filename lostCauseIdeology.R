#Code by Hailey Tucker 
#Last Updated: May 15, 2020

#Installing and Loading Required Packages -------------------
#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("stopwords")
#install.packages("wordcloud")
library(plyr)
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("twitteR")
library("ROAuth")
library(dplyr) 
library(purrr) #writing neat functions
library(tidyr) #manipulate table data
library(lubridate) #manipulate dates
library(scales) #for plotting
library(ggplot2) #for plotting
library(tidytext) #manipulating text data and natural language processing
library(sentimentr) #natural language processing
library(lexicon) #sentiment dictionaries
library(stringr) #manipulating text data
library(wordcloud2) #create wordclouds
library(data.table)
library(plotly)
library(rlange)
library(readxl)
library(readr)
library(stopwords)
library(SnowballC)
library(wordcloud)
#install.packages('rtweet')
library(rtweet)
#install.packages('maps')
library(maps)
library(writexl)

#Connecting to Twitter Account in order to extract tweets
#API  / Consumer key:xxxxxxxxxxxx
#API / Consumer secret key:xxxxxxxxxxxx
#Access Token:xxxxxxxxxxxxxx
#Access token secret: xxxxxxxxxxxx
consumer_key <- 'xxxxxxxxxxxxxx'
consumer_secret <- 'xxxxxxxxxxxxxx'
access_token <- 'xxxxxxxxxxxxxx'
access_secret <- 'xxxxxxxxxxxxxx'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

twitter_token <- create_token(
  app = 'lost_cause_sentiment',
  consumer_key,
  consumer_secret,
  access_token,
  access_secret,
  set_renv = TRUE)

# Creating Data Frame Dataframe-------------
 


# Note: "monument removal" will search for either monument or removal anywhere in text, not necessarily the phrase as a whole

list_words <- c("#lostcause","#LostCause",
                "#whitesupremacy", "#ConfederateStatesofAmerica", "#confederatememorial",
                "#UnitetheRight","#slavery","#CharlestonChurchShooting","#cultrualheritage",
                "Monument confederate","monument confederate","monument removal","Monument removal", '"confederate monuments"',
                "Confederate monuments","confederate","Confederate","confederate flag","Confederate flag", "confederate cause",
                "Historical statues confederate","historical statues confederate", "'lost cause'","'Lost Cause'",
                "white supremacy", "Confederate States of America", "confederate memorial",
                "Unite the Right","slavery", "Charleston church shooting","cultrual heritage",
                "public monuments confederate","#monumentremoval","monument removal",
                "#MonumentRemoval", "#confederatemonuments",
                "#ConfederateMonuments","#confederate","#Confederate", "#confederateflag","#ConfederateFlag",
                "#confederatecause","#HistoricalStatues","#historicalstatues")

tweet_df<- NULL # blank df for search_tweets method
full_tweet <- NULL # blank full_tweet for search_tweeets method

# Extraction using search_tweets
for (i in 1:length(list_words)){ #Start at position 5 on 04 May 2020 
  tweets <- search_tweets(list_words[i], n=500, include_rts=FALSE, lang="en")
  #tweets <- searchTwitter(list_words[i], n = 3200, since = '2015-05-01', until = '2018-01-01', lang = "en",
  #                        include_rts = FALSE)
  tweets_df <- as.data.frame(tweets)
  full_tweet = rbind(full_tweet, tweets_df)
}

full_tweet <- rbind(full_tweet, tweets_collection) # Add in any tweets collected while playing around with possible parameters

set.seed(42) # Randomizing order for DMML 
rows <- sample(nrow(full_tweet)) 
full_tweet_dmml <- full_tweet[rows, ] 

write_xlsx(full_tweet, "Documents\\full_tweet_May13.xlsx") # Writing to excel file 
dmml_project <- apply(full_tweet_dmml,2,as.character)
write.csv(dmml_project, file = "dmml_tweets.csv")  #writing to csv

full_tweet_copy_may13 <- full_tweet
full_test <- full_tweet

un <- unique(full_test)
# Extraction using searchTwitter (this has ability to search with timeframe, can't parse out retweets)

tweet_df_searchT<- NULL # blank df for search_tweets method
full_tweet_searchT <- NULL # blank full_tweet for search_tweeets method

for (i in 1:length(list_words)){ # Start at position 5 on 04 May 2020 
  tweets <- searchTwitter(list_words[i], n = 500, since = '2015-05-01', until = '2018-01-01', lang = "en")
  tweets_df_searchT <- as.data.frame(tweets)
  full_tweet_searchT = rbind(full_tweet_searchT, tweets_df_searchT)
}

write_xlsx(full_tweet_searchT, "Documents\\full_tweet_May11_searchT.xlsx") # Writing to excel file

# 1) *Most Frequently Used Words -------------

df <- full_tweet # Creating copy for sentiment analysis 
df_organic <- df[df$isRetweet==FALSE, ] # Remove retweets

df$text <-  gsub("https\\S*", "", df$text)
df$text <-  gsub("@\\S*", "", df$text) 
df$text  <-  gsub("amp", "", df$text) 
df$text  <-  gsub("[\r\n]", "", df$text)
df$text  <-  gsub("[[:punct:]]", "", df$text)

# removing stop words
tweets_freq_words <- df %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets_freq_words <- tweets_freq_words %>%
  anti_join(stop_words)

tweets_freq_words_stop<-subset(tweets_freq_words, word!="im") # removing other words

#Creating Y/N Flag for Converence Game (Yes) or Non-conference game (No)
without_q_terms <- tweets_freq_words

without_q_terms<-subset(without_q_terms, ! word %in% list_words)

# plotting most frequent words
tweets_freq_words_stop %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the Lost Cause Tweets") +
  theme(axis.text=element_text(size=12),
               axis.title=element_text(size=14,face="bold"))

# plotting most frequent words (WITHOUT QUERY WORDS)
without_q_terms %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the Lost Cause Tweets (without using common query words") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

other_words_to_remove <- c("flag", "lost", "white", "monument", "supremacy", "im", "unite", "whitesupremacy",
                           "rally", "flags", "monuments", "youre")

without_next<-subset(without_q_terms, ! word %in% other_words_to_remove)

# Running graph again 

# plotting most frequent words (WITHOUT QUERY WORDS)
without_next %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the Lost Cause Tweets (without using common query words") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# Most Frequent Hashtags 

df <- full_tweet

hash_freq <- df %>%
  select(hashtags) %>%
  unnest_tokens(word, hashtags)
hash_freq <- hash_freq %>%
  anti_join(stop_words)
hash_freq<-na.omit(hash_freq)

# plotting most frequent words 
hash_freq %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent hashtags found in the Lost Cause Tweets") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
# Printing examples tweets with Jesus hashtags

christian_t <- filter(df, grepl('bible', hashtags))
john3 <- filter(df, grepl('John3', hashtags))
jesuschrist <- filter(df, grepl('JesusChrist', hashtags))
apologetics <- filter(df, grepl('Apologetics', hashtags))

 #(DONE)
# 2) Sentiment Analysis------------------
df <- full_tweet # Creating copy for sentiment analysis 
df_organic <- df[df$isRetweet==FALSE, ] # Remove retweets
df_organic <- subset(df, is.na(df$reply_to_status_id)) # Remove replies

df_organic$clean<- gsub("<[^>]+>", "",df_organic$text) # Creating clean text

df_organic$clean_tweets <- df_organic$clean #Creating new column of clean_tweets
df_organic$description_text<-get_sentences(df_organic$clean_tweets)

sent_for_each_line<-sentiment(df_organic$description_text) # See sentiments for each line

sentiments <- sentiment_by(df_organic$description_text) # Sentiments by each description

#Adding ID to df so I can join sentiment labels with actual data to pull out examples 
element_id <- 1:nrow(df_organic)
df_organic <- cbind(element_id=element_id, df_organic)

sentiment_df<-setDF(sentiments) # Convert sentiment data table to data frame

get_sentiment_class <- function(sentiment_score){ #F unction that generates a sentiment class based off of sentiment score
  sentiment_class = "Positive"
  
  if (sentiment_score < -0.3){
    sentiment_class = "Negative"
  }
  
  else if(sentiment_score <0.03){
    sentiment_class = "Neutral"
  }
  
  sentiment_class
  
}

sentiment_df$sentiment_class<-sapply(sentiment_df$ave_sentiment,get_sentiment_class) # Adding a sentiment class attribute 

#Print resulting sentiment
sentiment_df[,4:5]

# Finding Examples
# Joining df and Sentiment_df 
df_for_examples <- join(df_organic, sentiment_df, by = 'element_id')
df_for_examples <- df_for_examples[,c('text', 'clean_tweets', 'ave_sentiment','sentiment_class')]

pos_examples <- df_for_examples[which(df_for_examples$sentiment_class == "Positive"),]  
  # pulling out positive tweets
                                                                      # for examples 
neg_examples <- df_for_examples[which(df_for_examples$sentiment_class == "Negative"),] # pulling out Negative tweets
# for examples
neu_examples <- df_for_examples[which(df_for_examples$sentiment_class == "Neutral"),] # pulling out Neutral  tweets
# for examples

sentiment_summary <- count(sentiment_df, sentiment_class) # Draw a bar graph

sentiment_summary

ggplot(sentiment_summary, aes(x=reorder(sentiment_class,-n), y=n)) + geom_col(fill = "#640b0b") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Sentiment Type") + ylab("Count") + ggtitle("Description counts of Tweets")#DON'T DO THIS 
# 3) *Emotion Analysis for All Product Descriptions-------------------------
# Create a dataframe for emotions by review 
emotions <- emotion_by(df_organic$description_text, by = element_id)
emotion_df <- setDF(emotions)
emotion_df

emotion_summary = subset (aggregate ( emotion_count ~ emotion_type, emotion_df, sum), emotion_count > 0) # aggregate by emotion types and remove 0 values 

emotion_summary = subset(emotion_summary, emotion_summary$emotion_count > 5000) # subset for most important emotions

# Drawing a pie chart for emotions (bar graph??)
pie(emotion_summary$emotion_count, emotion_summary$emotion_type, col=c("#640b0b","#a2854a","#376ca4","#007063","#c8c8c8","Black","White","Grey"), cex = 1.2, main = "Emotions for all descriptions")

plotSentiment(emotion_df)
ggplot(emotion_summary, aes(x=reorder(emotion_type,-emotion_count), y=emotion_count)) + geom_col(fill = "#640b0b") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Emotion Type") + ylab("Count") + ggtitle("Description counts by emotion")


# Getting examples
emotion_examples <- join(df, emotion_df, by = 'element_id')
emotion_examples <- emotion_examples[,c('Products', 'Product.Class', 'prod_descriptions', 'ave_emotion','emotion_type')]
write.csv(emotion_examples, file = "emotion_examples.csv")






 
# 4) *TF - IDF / Most Important Words --------

df_tf <- full_tweet

df_tf <- df_tf[df_tf$isRetweet==FALSE, ] # Remove retweets
df_tf <- subset(df_tf, is.na(df_tf$reply_to_status_id)) # Remove replies

df_tf$clean <- gsub("<[^>]+>", "",df_tf$text)
df_tf$clean<- gsub("<[^>]+>", "",df_tf$text) # Creating clean text

dfcorpus <- df_tf$clean
dfcorpus <- as.data.frame(dfcorpus)
dfcorpus <- VCorpus(VectorSource(dfcorpus))
dfcorpus <- tm_map(dfcorpus, content_transformer(tolower))

dfcorpus <- tm_map(dfcorpus, removePunctuation)

dfcorpus <- tm_map(dfcorpus,removeWords,stopwords())
dfcorpus <- tm_map(dfcorpus,removeWords,c("use"))

dfcorpus <- tm_map(dfcorpus,stemDocument)

tweet_text <- TermDocumentMatrix(dfcorpus)
inspect(tweet_text)

matrix <- as.matrix(tweet_text)
words <- sort(rowSums(matrix),decreasing=TRUE)
df2 <- data.frame(word = names(words), freq = words)

imp_san <- c("one","like","get","get","amp") # Taking out stop words 

df2<-subset(df2, ! word %in% imp_san)

wordcloud(words = df2$word,freq = df2$freq,
          min.freq = 10,
          max.words = 50,
          random.order = FALSE,
          colors = brewer.pal(8,"Dark2"))

# Making bar chart 

df2_test <- subset(df2, df2$freq > 1000)

ggplot(df2_test, aes(x=reorder(word,-freq), y=freq)) + geom_col(fill = "#640b0b") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Word") + ylab("Weighted Count (Importance)") + ggtitle("Words by importance")


# User-Defined Location Analysis --------

# importing state file 
states_cor <- read_excel("~/Documents/states_cor.xlsx")

states_cor <- as.data.frame(states_cor)
names(states_cor)[1] <- "State"
names(states_cor)[2] <- "freq"
states_cor$freq <- as.numeric(states_cor$freq)

states_cor <- subset (states_cor, states_cor$freq > 85) #subsetting to get top 12 states

# Plotting 
ggplot(states_cor, aes(x=reorder(State,-freq), y=freq)) + geom_col(fill = "#640b0b") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("State") + ylab("Tweets per state") + ggtitle("Which States are Tweeting the most?")


