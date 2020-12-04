#TERM PAPER BAN400

library(shiny)
library(rtweet)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(SentimentAnalysis)
library(ggplot2)
library(wordcloud)
library(edgar)
library(edgarWebR)
library(riingo)
library(anytime)
library(tm)
library(wordcloud)

#reference
#tweets sentiment analysis http://blueanalysis.com/iulianserban/Files/twitter_report.pdf
#tweets sentiment analysis http://cs229.stanford.edu/proj2011/GoelMittal-StockMarketPredictionUsingTwitterSentimentAnalysis.pdf
#edgar api doc https://developer.edgar-online.com/docs 

#example
#https://longhowlam.shinyapps.io/TweetAnalyzer/


#input window
  #parameter : search key : ticker symbol or cashtag  or company name
  #            average purchase price:  numbe
  #            total shares :  number

  #            number of tweets: 0~18000
  #            time period: last n days ??
  #            dictionary: harvard or lm 
  #            uni_gram or bi_gram
  
#Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Stock Trade Advice"),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    # enter search key
                    textInput(inputId = "search_key", 
                              label = strong("Search key"),
                              value = "Enter ticker symbol"),
                                # search key must be a ticker symbol
                    
                    #check if the user hold ths stock
                    checkboxInput("hold", label = "Hold this stock", value = TRUE),
                    
                    #display only if the chcekbox"hold" is checked
                    conditionalPanel(condition = "input.hold == true",
                                     numericInput("shares", label = h3("Shares"), value = 100),
                                     numericInput("price", label = h3("Average purchase price"), value = 100)),
                    
                    
                    # select number of tweets to be retrived
                    sliderInput("number_tweets", label = h3("number of tweets to be retrived"), 
                                min = 500, max = 18000, value = 10000), 
                                #min should be a reasonable value

                    #select sentiment dictionary
                    selectInput(inputId = "dictionary", label = h3("Sentiment dictionary"),
                                choices = c("Harvard","LM"),
                                selected = "Harvard"),
                    
                    #select sentiment method
                    #selectInput(inputId = "method", label = h3("Sentiment method"),
                     #           choices = c("Uni_gram","Bi_gram"),
                    #            selected = "Uni_gram")
                    
                   
                    #)
                  #),
                  
                    # Output: Description, lineplot, and reference
                  mainPanel(
                    tabsetPanel(
                      id = "output",
                      tabPanel("Twitter sentiment analysis", DT::dataTableOutput("mytable1")),
                      tabPanel("10-K sentiment analysis", DT::dataTableOutput("mytable2")),
                      tabPanel("Stock", T::dataTableOutput("mytable3")),
                      tabPanel("trade advise", DT::dataTableOutput("mytable4"))
                    ))
  )))



#search key must be in this dataset
ticker_symbol <- supported_tickers()%>%
  filter(exchange == "AMEX" | exchange == "NASDAQ" | exchange == "NYSE" )%>%
  select(ticker)%>%
  as.matrix()%>%
  as.vector()%>%
  paste0("$", .)%>%
  tolower()



#extract data 

token <- create_token(
  app = "homework",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "") 
  #this is a private token
  
extra_tweets <-function(input.search_key){
  usefual_search_key <- c( paste("\"$",toupper(input.search_key),"\""),
                           paste("\"$",tolower(input.search_key),"\""))
  #handle different format                
  
  search_tweets(
  usefual_search_key,                          #search key 
  n =18000,                                    #number of tweets
  type = "recent",                             #type of tweets
  include_rts = FALSE,                         #exclude retweet
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = token,
  retryonratelimit = FALSE,
  verbose = TRUE,
  lang = "en" )%>%
  mutate(created_day = as.date(created_at),    #transform date and time
         created_time = hour(created_at))%>%
  select(-created_at)                          #delete column created_at
}



#output-timelines - how many tweets retrived per day
plot_tweets_dt <- function(df){
  df <- df %>%
    group_by(created_day, created_time)%>%
    summarize(tw_count = n())
  
  plot <- df %>%
    ggplot(aes(x=created_day,y = created_time, fill = tw_count))+
    geom_point(alpha=0.5,show.legend = F)
  
  print(plot) # bubble plot based on created day and time
  }

plot_tweets_d <- function(df){
  df <- df %>%
    group_by(created_day)%>%
    summarize(tw_count = n())
  
  plot <- df %>%
    ggplot(aes(x=created_day,y = tw_count))+
    geom_bar(alpha=0.8,show.legend = F)+
    theme_classic()
  
  print(plot)
}

#preprocess function
cleaning_tw_df <- function(df){
  extra_hashtag <- 
    DocumentTermMatrix(Corpus(VectorSource(df$hashtags)),
                       control = list( removePunctuation = T,
                                       stripWhitespace = T,
                                       tolower = T ))%>%
    as.matrix()%>%
    as.data.frame()%>%
    colnames()
  
  extra_ticker <- 
    DocumentTermMatrix(Corpus(VectorSource(df$symbols)),
                       control = list( removePunctuation = T,
                                       stripWhitespace = T,
                                       tolower = T ))%>%
    as.matrix()%>%
    as.data.frame()%>%
    colnames()
  
 cleaned_tw <- 
   df$text %>% 
   gsub("\n"," ", .) %>%                  #remove \n
                                          #remove urls
   gsub("[[:punct:]]", " ", .) %>%        #remove punctuation
   tolower() %>%                          #convert to lower case 
   gsub("amp"," ", .)%>%                  #remove amp 
   removeWords(.,extra_hashtag)%>%        #remove hashtags
   removeWords(.,extra_ticker)%>%         #remove tickers symbols
   gsub("[[:digit:]]", "", .) %>%         #remove digits
   removeWords(., stopwords("en")) %>%    #remove standard stopwords
   gsub('\\b\\w{1,2}\\b','', .) %>%       #remove words of length 1-2
   gsub('\\b\\w{21,}\\b','', .) %>%       #remove words of length 21 or more
   gsub("\\s(\\s*)", " ", .) %>%          #remove excess whitespace
   trimws()                               #remove first space
 
 print(cleaned_tw)
}

cleaned_tw <- cleaning_tw_df(tw_df)


#output-wordcloud words appeared most frequently agmong tweets retrived
make_tweet_wordcloud<- function(df){
    x <- 
      DocumentTermMatrix(Corpus(VectorSource(df)),
                       control = list(stemming = T,
                                      bounds = list(global = c(5,500)))) %>%
      as.matrix()%>%
      as.data.frame()%>%
      colSums()
    
    plot_wordcloud <-
      wordcloud(words = names(x),
              freq = x,
              min.freq =20,
              max.words = 50, 
              colors = brewer.pal(5, "Set1"))
    print(plot_wordcloud)
    
}


#output-shortterm sentiment analysis base on twitter 
  #sentiment analysis base on different dictionary
  daily_sentiment_analysis <- function(df){
    if(input.dictionary == "Harvard"){
    daily_harvard_sentiment(df)
  }else{
    daily_lm_sentiment(df)
    }
  }

  # harvard sentiment function
  daily_harvard_sentiment <- funtion(df){
    
    daily_score_H <- df %>%
      mutate(harvard_score = analyzeSentiment(text)$SentimentGI)%>%
      groupby(created_day)%>%
      summarize( daily_harvard_score = mean (harvard_score))
    
    if(unique(daily_score_H$created_day) > 1){
      daily_score_H%>%
        ggplot(aes(x= created_day, y= daily_harvard_score))+
        geom_point(show.legend = F, col = "red")+
        geom_line()+
        scale_x_continuous(name="Date", limits=c(min(created_day)+1, max(created_day)))
        geom_smooth(alpha = 0, method = "lm")
    }else{
      daily_score_H%>%
        ggplot(aes(x= created_day, y= daily_harvard_score))+
        geom_point(show.legend = F, col = "red")+
        geom_line()+
        geom_smooth(alpha = 0, method = "lm")
    }
  }
  
  # LM sentiment function
  daily_lm_sentiment <-  funtion(df){
    daily_score_LM <- df %>%
      mutate(lm_score = analyzeSentiment(text)$SentimentLM)%>%
      groupby(created_day)%>%
      summarize( daily_lm_score = mean (lm_score))
    
    if(unique(df$created_day) > 1){
      daily_score_LM%>%
        ggplot(aes(x= created_day, y= daily_lm_score))+
        geom_point(show.legend = F, col = "red")+
        geom_line()+
        scale_x_continuous(name="Date", limits=c(min(created_day)+1, max(created_day)))
        geom_smooth(alpha = 0, method = "lm")
    }else{
      daily_score_LM%>%
        ggplot(aes(x= created_day, y= daily_lm_score))+
        geom_point(show.legend = F, col = "red")+
        geom_line()++
        geom_smooth(alpha = 0, method = "lm")
    }
  }
  

#output-longterm sentiment analysis base on 10-k 
  #get MDA part of the lastest 10-k
  get_MDA <- function(input.search_key){
    useful_search_key <- toupper(input.search_key)
    
    test_10k <- 
      company_filings(
        useful_search_key,
        ownership = FALSE,
        type = "10-K",
        before = "20201231",
        count = 10
      )%>%
      filter(year(filing_date ) == 2020)%>%
      select(href)%>%
      as.matrix()%>%
      as.vector()%>%
      filing_documents()%>%
      filter(type == "10-K")%>%
      select(href)%>%
      as.matrix()%>%
      as.vector()%>%
      parse_filing()%>%
      filter(item.name == "Item 7. Management's Discussion and Analysis of Financial Condition and Results of Operations")
  }
  
  test_10k <- get_MDA("aapl")

  #get cleaned MDA
  get_cleaned_MDA <- function(df){
    customed_word <- c("company", "10-K", "form", "FORM", "item", "ITEM")
    
    df%>%
      select(text)%>%
      as.matrix()%>%
      as.vector()%>%
      paste(collapse = " ")%>%
      gsub("[[:punct:]]", " ", .) %>% 
      gsub("[[:digit:]]", "", .) %>% 
      tolower() %>% 
      removeWords(., stopwords("en")) %>% 
      removeWords(.,customed_word)%>%
      gsub('\\b\\w{1,2}\\b','', .) %>% 
      gsub("\\s(\\s*)", " ", .) %>%
      trimws()
  }
  
  test_cleaned_MDA <- get_cleaned_MDA(test_10k)
  #make a wordcloud
  make_MDA_wordcloud<- function(df){
    x <- 
      DocumentTermMatrix(Corpus(VectorSource(df)),
                         control = list(stemming = F,
                                        bounds = list(global = c(1,100)))) %>%
      as.matrix()%>%
      as.data.frame()%>%
      colSums()
    
    plot_wordcloud <-
      wordcloud(words = names(x),
                freq = x,
                min.freq =5,
                max.words = 50, 
                colors = brewer.pal(5, "Set1"))
    print(plot_wordcloud)
    
  }
  
  make_MDA_wordcloud(test_cleaned_MDA)
  
  #sentiment analysis based on LM
  MDA_sentiment_LM <- analyzeSentiment(test_cleaned_MDA)$SentimentLM
  
  

  
  
