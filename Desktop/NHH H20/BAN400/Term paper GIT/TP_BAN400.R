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
  filter(exchange == "AMEX" | exchange == "NASDAQ" | exchange == "NYSE" )



#extract data

token <- create_token(
  app = "homework",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "") 
  #this is a private token
  
extra_tweets <-function(input.search_key){
  search_tweets(
  input.search_key,             #search key 
  n =1000,       #number of tweets
  type = "recent",
  include_rts = FALSE,          #exclude retweet
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = token,
  retryonratelimit = FALSE,
  verbose = TRUE,
  lang = "en" )%>%
  mutate(created_day = as.date(created_at),
         created_time = hour(created_at))%>%
  select(-created_at)
}

test_data <- extra_tweets("$AAPL")


#output-timelines - how many tweets retrived per day
plot_tweets_dt <- function(df){
  df <- df %>%
    group_by(created_day, created_time)%>%
    summarize(tw_count = n())
  
  plot <- df %>%
    ggplot(aes(x=created_day,y = created_time, fill = tw_count))+
    geom_point(alpha=0.5,show.legend = F)
  
  print(plot)
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
df$text %>% 
  gsub("[[:punct:]]", " ", .) %>%        #remove punctuation
  gsub("[[:digit:]]", "", .) %>%         #remove digits
  tolower() %>%                          #convert to lower case
  removeWords(., stopwords("en")) %>%    #remove standard stopwords
  removeWords(., ticker_symbol)          #remove tickers
  gsub('\\b\\w{1,2}\\b','', .) %>%       #remove words of length 1-2
  gsub('\\b\\w{21,}\\b','', .) %>%       #remove words of length 21 or more
  gsub("\\s(\\s*)", " ", .) %>%          #remove excess whitespace
  trimws()                               #remove first space
}

##output-wordcloud words appeared most frequently agmong tweets retrived
make_wordcloud<- function(df){
    dtm <- DocumentTermMatrix(Corpus(VectorSource(df)),
                       control = list(stemming = T,
                                      bounds = list(global = c(1,3)))) 
    dtm.matrix<- as.matrix(dtm)
    
    dtm.df <- as.data.frame(dtm.matrix)
    
    x <- colSums(dtm.df)
    
    wordcloud(words = names(x),
              freq = x,
              min.freq =5)
    
    }


#output-shortterm sentiment analysis base on twitter 
  #sentiment analysis base on different dictionary
  sentiment_analysis <- function(df){
    if(input$dictionary == "Harvard"){
    harvard_sentiment(df)
  }else{
    lm_sentiment(df)
    }
  }

  # harvard sentiment function
  harvard_sentiment <- funtion(df){
    if(unique(df$created_day) > 1){
      df %>%
        mutate(harvard_score = analyzeSentiment(text)$SentimentGI)%>%
        groupby(created_day)%>%
        summarize( daily_harvard_score = mean (harvard_score))%>%
        ggplot(aes(x= created_day, y= daily_harvard_score))+
        geom_point(show.legend = F, col = "red")+
        geom_line()+
        scale_x_continuous(name="Date", limits=c(min(created_day)+1, max(created_day)))
        geom_smooth(alpha = 0, method = "lm")
    }else{
      df %>%
        mutate(harvard_score = analyzeSentiment(text)$SentimentGI)%>%
        groupby(created_day)%>%
        summarize( daily_harvard_score = mean (harvard_score))%>%
        ggplot(aes(x= created_day, y= daily_harvard_score))+
        geom_point(show.legend = F, col = "red")+
        geom_line()+
        geom_smooth(alpha = 0, method = "lm")
    }
  }
  
  # LM sentiment function
  lm_sentiment <-  funtion(df){
    if(unique(df$created_day) > 1){
      df %>%
        mutate(lm_score = analyzeSentiment(text)$SentimentLM)%>%
        groupby(created_day)%>%
        summarize( daily_lm_score = mean (lm_score))%>%
        ggplot(aes(x= created_day, y= daily_lm_score))+
        geom_point(show.legend = F, col = "red")+
        geom_line()+
        scale_x_continuous(name="Date", limits=c(min(created_day)+1, max(created_day)))
        geom_smooth(alpha = 0, method = "lm")
    }else{
      df %>%
        mutate(lm_score = analyzeSentiment(text)$SentimentLM)%>%
        groupby(created_day)%>%
        summarize( daily_lm_score = mean (lm_score))%>%
        ggplot(aes(x= created_day, y= daily_lm_score))+
        geom_point(show.legend = F, col = "red")+
        geom_line()++
        geom_smooth(alpha = 0, method = "lm")
    }
  }
  

#output-longterm sentiment analysis base on 10-k 
  

  
  get_cik <- function(df){
      
      cik_web_front <-c("https://datafied.api.edgar-online.com/v2/companies?Appkey=7d405ce3e8ddb45e62da90edcc563c54&primarysymbols=")
      cik_web_ticker <-c("aapl")  #web_ticker <- tolower(df)
      cik_web_back <- c("&deleted=false&sortby=primarysymbol%20asc")
      
      readLines(paste(cik_web_front,cik_web_ticker,cik_web_back))%>%
      gsub(".*cik.+\"(\\d{10})\".+","\\1", .)
  
    }
    
    getMgmtDisc(cik.no = 320193, filing.year = 2020)
    getFilings(cik.no = 320193 , form.type = "10-K", filing.year = 2020, downl.permit = "n")
    
    readLines("https://datafied.api.edgar-online.com/v2/companies?primarysymbols=aapl&appkey={7d405ce3e8ddb45e62da90edcc563c54}")
  
