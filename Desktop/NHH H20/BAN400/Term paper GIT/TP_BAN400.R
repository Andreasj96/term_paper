#TERM PAPER BAN400

library(shiny)
library(rtweet)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(SentimentAnalysis)

#reference
#http://blueanalysis.com/iulianserban/Files/twitter_report.pdf
#http://cs229.stanford.edu/proj2011/GoelMittal-StockMarketPredictionUsingTwitterSentimentAnalysis.pdf

#example
#https://longhowlam.shinyapps.io/TweetAnalyzer/


#input window
  #parameter : search key : ticker symbol or cashtag  or company name
  #            number of tweets: 0~18000
  #            time period: last n days ?
  #            dictionary: harvard or lm 
  #            uni_gram or bi_gram



#search key must be included in this dataset
ticker_symbol <- 
  read.csv("us_stock_code.csv")%>%
  select(code)


#extract data
  #search_tweets() an api token is needed!!
  #get_mentions()


tw_df<-
  search_tweets(
  "$AAPL",             #=search key 
  n = 1000,            #number of tweets
  type = "recent",
  include_rts = FALSE, #exclude retweet
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = NULL,
  retryonratelimit = FALSE,
  verbose = TRUE,
  lang = "en" )%>%
  mutate(created_day = date(created),
         created_time = time(created))

#preprocess function
pre_df <- function(df){
df$text %>% 
  gsub("[[:punct:]]", " ", .) %>%        #remove punctuation
  gsub("[[:digit:]]", "", .) %>%         #remove digits
  tolower() %>%                          #convert to lower case
  removeWords(., stopwords("en")) %>%    #remove standard stopwords
  gsub('\\b\\w{1,2}\\b','', .) %>%       #remove words of length 1-2
  gsub('\\b\\w{21,}\\b','', .) %>%       #remove words of length 21 or more
  gsub("\\s(\\s*)", " ", .) %>%          #remove excess whitespace
  trimws()                               #remove first space
}

#dtm funtion
make_dtm <- function(df){
  df$text%>%
    DocumentTermMatrix(VCorpus(VectorSource()),
                       control = list(stemming = TRUE )) #needed?
}


#sentiment analysis 



#timelines
tw_df %>% 
  group_by(created_day, created_time)%>%
  mutate(tw_count = n())%>%
  ggplot(aes(x=created_day,y = created_time, fill = tw_count))+
  geom_point()


