#TERM PAPER BAN400

#Library############################################################################################
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
library(rlist)

#Reference ########################################################################################   
#tweets sentiment analysis http://blueanalysis.com/iulianserban/Files/twitter_report.pdf
#tweets sentiment analysis http://cs229.stanford.edu/proj2011/GoelMittal-StockMarketPredictionUsingTwitterSentimentAnalysis.pdf
#edgar api doc https://developer.edgar-online.com/docs 

#Example
#https://longhowlam.shinyapps.io/TweetAnalyzer/


#Input window
#parameter : search key : ticker symbol or cashtag  or company name
#            average purchase price:  numbe
#            total shares :  number
#            number of tweets: 0~18000
#            dictionary: harvard or lm 


#Define UI###############################################################################################
ui <- fluidPage( titlePanel("Stock Trade Advises"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     # enter search key
                     textInput(inputId = "search_key", 
                               label = "Search key",
                               value = "AAPL"),
                     # search key must be a ticker symbol
                     
                     #check if the user hold ths stock
                     checkboxInput("hold", label = "Hold this stock", value = F),
                     
                     #display only if the chcekbox"hold" is checked
                     conditionalPanel(condition = "input.hold === true",
                                      numericInput("shares", label = "Holding shares", value = 100),
                                      numericInput("price", label = "Average purchase price", value = 100)),
                     
                     
                     # select number of tweets to be retrived
                     sliderInput(inputId = "number_tweets", label = "Number of tweets to be retrived", 
                                 min = 500, max = 18000, value = 1000, step = 500), 
                     #min should be a reasonable value
                     
                     #select sentiment dictionary
                     selectInput(inputId = "dictionary", label = "Sentiment dictionary for tweets",
                                 choices = c("Harvard","LM"),
                                 selected = "Harvard"),
                     
                     #select sentiment analysis frequent?
                     selectInput(inputId = "frequent", label = "Sentiment analysis frequent for tweets",
                                 choices = c("Daily","Hourly"),
                                 selected = "Hourly")
                     
                   ),
                   
                   
                   
                   # Output: Description, lineplot, and reference
                   mainPanel(
                     tabsetPanel(
                       type = "tabs",
                       id = "output",
                       tabPanel("Twitter sentiment analysis", 
                                plotOutput("tweets_number_plot"),
                                plotOutput("tweets_wordcloud_plot"),
                                plotOutput("tweets_sentiment_plot")),
                       tabPanel("10-K sentiment analysis", 
                                plotOutput("MDA_wordcloud_plot"),
                                tableOutput("MDA_sentiment_plot")),
                       tabPanel("Real_time stock price", plotOutput("stock_price")),
                       tabPanel("Trade advises", textOutput("trade_advises"))
                     )
                   )
                 )
)





#Define server(not finished)############################################################################
server <- function(input, output) {
  
  # Subset data
  # data extracting and search_key must be a vaild ticker symbol
  
  get_cleaned_MDAs <- reactive({
    get_cleaned_MDA(get_MDA(input$search_key))
  })
  
  
  # plot-how many tweets retrived per day
  
  extract_tweets <-reactive({
    req(input$search_key)
    validate(need(toupper(input$search_key) %in% ticker_symbol),
             "Error: Please enter a vaild search key.")
    extract_tweet(input$search_key)
  })
  
  output$tweet_number_plot <- renderPlot({
    plot_tweets_dt(extract_tweets())
  })
  
  # plot-wordcloud
  cleaned_tweets <- reactive({
    cleaning_tw_df(extract_tweets())
  })
  
  input_number_tweets <- reactive({
    input$number_tweets
  })
  
  output$tweets_wordcloud_plot <- renderPlot({
    make_tweet_wordcloud(cleaned_tweets())
  })
  
  # plot-tweets sentiment
  output$tweets_sentiment_plot <- renderPlot({
    tweet_sentiment_analysis(cleaned_tweets())
  })
  
  # plot-10kMDA wordcloud
  output$MDA_wordcloud_plot <- renderPlot({
    make_MDA_wordcloud(get_cleaned_MDAs())
  })
  
  # plot-10kMDA sentiment
  output$MDA_wordcloud_plot <- renderTable({
    MDA_sentiment_LM(get_cleaned_MDAs())
  })
  
  
}


shinyApp(ui = ui, server = server)
##########################################################################################################


#Define all needed functions##############################################################################

#Search key must be in this dataset
ticker_symbol <- supported_tickers()%>%
  filter(exchange == "AMEX" | exchange == "NASDAQ" | exchange == "NYSE" )%>%
  select(ticker)%>%
  as.matrix()%>%
  as.vector()

vaild_ticker_symbol <- function(df){
  
  print(toupper(df) %in% ticker_symbol)
  
}

#Extract data 
extract_tweet <-function(df){
  
  token <- create_token(
    app = "homework",
    consumer_key="",
    consumer_secret="",
    access_token="",
    access_secret="" ) 
  #this is a private token expired on 20210102
  
  usefual_search_key <- c( paste("$",toupper(df),sep = ""))
  
  
  search_tweets(
    usefual_search_key,                          #search key 
    n =input$number_tweets,                      #number of tweets  shall be input$number_tweets
    type = "recent",                             #type of tweets
    include_rts = FALSE,                         #exclude retweet
    geocode = NULL,
    max_id = NULL,
    parse = TRUE,
    token = token,
    retryonratelimit = FALSE,
    verbose = TRUE,
    lang = "en" )%>%
    mutate(created_day = as.Date(created_at),    #created day
           created_time = hour(created_at),      #created time
           created_datetime = 
             floor_date(created_at,'hour'))%>%   #created day&time
    select(-created_at)                          #delete column created_at
  
}

#output-timelines - how many tweets retrived per day
plot_tweet_dt <- function(df){
  df <- df %>%
    group_by(created_day, created_time)%>%
    summarize(tw_count = n())
  
  plot <- df %>%
    ggplot(aes(x=created_day,y = created_time, size = tw_count))+
    geom_point(alpha=0.5,show.legend = F)
  
  print(plot) # bubble plot based on created day and time
}

plot_tweet_d <- function(df){
  df <- df %>%
    group_by(created_day)%>%
    summarize(tw_count = n())
  
  plot <- df %>%
    ggplot(aes(x=created_day,y = tw_count))+
    geom_bar(stat="identity", alpha=0.8,show.legend = F)+
    theme_classic()
  
  print(plot)
}

#Preprocess function
cleaning_tw_df <- function(df){
  extract_hashtag <- 
    DocumentTermMatrix(Corpus(VectorSource(df$hashtags)),
                       control = list( removePunctuation = T,
                                       stripWhitespace = T,
                                       tolower = T ))%>%
    as.matrix()%>%
    as.data.frame()%>%
    colnames()
  #extract hashtags cantained in each tweets
  
  extract_ticker <- 
    DocumentTermMatrix(Corpus(VectorSource(df$symbols)),
                       control = list( removePunctuation = T,
                                       stripWhitespace = T,
                                       tolower = T ))%>%
    as.matrix()%>%
    as.data.frame()%>%
    colnames()
  #extract ticker symbols cantained in each tweets
  
  extract_url1 <-
    df$urls_t.co%>%
    unlist()%>%
    .[is.na(.) == F]
  #extract url cantained in each tweets
  
  extract_url2 <-
    df$media_t.co%>%
    unlist()%>%
    .[is.na(.) == F]
  #extract another url cantained in each tweets
  
  cleaned_text <- 
    df$text %>% 
    removeWords(.,extract_url1)%>%         #remove url1
    removeWords(.,extract_url2)%>%         #remove url2
    gsub("U0001.{4}", " ", .)%>%           #doesnot work!!!
    gsub("U0001", " ", .)%>%               #doesnot work!!!
    gsub("[[:punct:]]", " ", .) %>%        #remove punctuation
    tolower() %>%                          #convert to lower case 
    gsub("amp"," ", .)%>%                  #remove amp
    gsub("https", " ", .)%>%               #remove https
    removeWords(.,extract_hashtag)%>%      #remove hashtags
    removeWords(.,extract_ticker)%>%       #remove tickers symbols
    gsub("[[:digit:]]", "", .) %>%         #remove digits
    removeWords(., stopwords("en")) %>%    #remove standard stopwords
    gsub('\\b\\w{1,2}\\b','', .) %>%       #remove words of length 1-2
    gsub("\\s(\\s*)", " ", .) %>%          #remove excess whitespace
    trimws()                               #remove first space
  #get cleaned tweet text
  
  cleaned_tw <- 
    df%>%
    mutate(text = cleaned_text)%>%
    select(user_id, text, source, favorite_count, retweet_count, quote_count, reply_count, followers_count,
           favourites_count, created_day, created_time, created_datetime)
  #replace text with cleaned text and select valuabe columns
  
}


#Output-wordcloud words appeared most frequently agmong tweets retrived
make_tweet_wordcloud<- function(df){
  x <- 
    DocumentTermMatrix(Corpus(VectorSource(df$text)),
                       control = list(stemming = T,
                                      bounds = list(global = c(5,input_number_tweets())))) %>%   #500 = input.number_tweets
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

#Output-short term sentiment analysis base on twitter
#sentiment analysis
tweet_sentiment_analysis <- function(df){
  if(input$frequent == "Daily"){
    daily_sentiment_analysis(df)
  }else{hourly_sentiment_analysis(df)
  }
}


#daily sentiment analysis base on different dictionary
daily_sentiment_analysis <- function(df){
  if(input$dictionary == "Harvard"){
    daily_harvard_sentiment(df)
  }else{
    daily_lm_sentiment(df)
  }
}

# harvard sentiment function
daily_harvard_sentiment <- function(df){
  
  daily_score_H <- df %>%
    mutate(harvard_score = analyzeSentiment(text)$SentimentGI)%>%
    group_by(created_day)%>%
    summarize( daily_harvard_score = mean (harvard_score, na.rm = T))
  
  if(length(unique(daily_score_H$created_day)) > 2){
    daily_score_H%>%
      ggplot(aes(x= created_day, y= daily_harvard_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      scale_x_date(limits=c(min(daily_score_H$created_day)+1, max(daily_score_H$created_day)))+
      geom_smooth(alpha = 0, method = "lm")+
      labs(x="Created Day",y="Daily Sentiment Score",title="Sentiment Analysis based on Harvard Dictionary")
  }else{if(length(unique(daily_score_H$created_day)) > 1)
  {daily_score_H%>%
      ggplot(aes(x= created_day, y= daily_harvard_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      scale_x_date(limits= c(max(daily_score_H$created_day)))+
      labs(x="Created Day",y="Daily Sentiment Score",title="Sentiment Analysis based on Harvard Dictionary")
  }else{
    daily_score_H%>%
      ggplot(aes(x= created_day, y= daily_harvard_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      labs(x="Created Day",y="Daily Sentiment Score",title="Sentiment Analysis based on Harvard Dictionary")
  }
  }
}

# LM sentiment function
daily_lm_sentiment <-  function(df){
  
  daily_score_LM <- df %>%
    mutate(lm_score = analyzeSentiment(text)$SentimentLM)%>%
    group_by(created_day)%>%
    summarize( daily_lm_score = mean (lm_score, na.rm = T))
  
  if(length(unique(daily_score_LM$created_day)) > 2){
    daily_score_LM%>%
      ggplot(aes(x= created_day, y= daily_lm_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      scale_x_date(limits=c(min(daily_score_LM$created_day)+1, max(daily_score_LM$created_day)))+
      geom_smooth(alpha = 0, method = "lm")+
      labs(x="Created Day",y="Daily Sentiment Score",title="Sentiment Analysis based on LM Dictionary")
  }else{if(length(unique(daily_score_LM$created_day)) > 1)
  {daily_score_LM%>%
      ggplot(aes(x= created_day, y= daily_lm_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      scale_x_date(limits= max(daily_score_LM$created_day))+
      labs(x="Created Day",y="Daily Sentiment Score",title="Sentiment Analysis based on LM Dictionary")
  }else{
    daily_score_LM%>%
      ggplot(aes(x= created_day, y= daily_lm_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      labs(x="Created Day",y="Daily Sentiment Score",title="Sentiment Analysis based on LM Dictionary")
  }
  }
}

#hourly sentiment analysis base on different dictionary 
hourly_sentiment_analysis <- function(df){
  if(input.dictionary == "Harvard"){
    hourly_harvard_sentiment(df)
  }else{
    hourly_lm_sentiment(df)
  }
}

# harvard sentiment function
hourly_harvard_sentiment <- function(df){
  
  hourly_score_H <- df %>%
    mutate(harvard_score = analyzeSentiment(text)$SentimentGI)%>%
    group_by(created_datetime)%>%
    summarize( hourly_harvard_score = mean (harvard_score, na.rm = T))
  
  if(length(unique(hourly_score_H$created_datetime)) > 2){
    hourly_score_H%>%
      ggplot(aes(x= created_datetime, y= hourly_harvard_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      scale_x_datetime(limits=c(hourly_score_H$created_datetime[2], 
                                hourly_score_H$created_datetime[length(hourly_score_H$created_datetime)]))+
      geom_smooth(alpha = 0, method = "lm")+
      labs(x="Created Date&Time",y="Hourly Sentiment Score",title="Hourly Sentiment Analysis based on Harvard Dictionary")
  }else{if(length(unique(hourly_score_H$created_day)) > 1)
  {hourly_score_H%>%
      ggplot(aes(x= created_datetime, y= hourly_harvard_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      scale_x_datetime(limits= c(max(daily_score_H$created_datetime)))+
      labs(x="Created Day&Time",y="Hourly Sentiment Score",title="Hourly Sentiment Analysis based on Harvard Dictionary")
  }else{
    hourly_score_H%>%
      ggplot(aes(x= created_datetime, y= hourly_harvard_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      labs(x="Created Day&Time",y="Hourly Sentiment Score",title="Hourly Sentiment Analysis based on Harvard Dictionary")
  }
  }
}

# LM sentiment function
hourly_lm_sentiment <- function(df){
  
  hourly_score_LM <- df %>%
    mutate(lm_score = analyzeSentiment(text)$SentimentLM)%>%
    group_by(created_datetime)%>%
    summarize( hourly_lm_score = mean (lm_score, na.rm = T))
  
  if(length(unique(hourly_score_LM$created_datetime)) > 2){
    hourly_score_LM%>%
      ggplot(aes(x= created_datetime, y= hourly_lm_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      scale_x_datetime(limits=c(hourly_score_LM$created_datetime[2], 
                                max(hourly_score_LM$created_datetime)))+
      geom_smooth(alpha = 0, method = "lm")+
      labs(x="Created Date&Time",y="Hourly Sentiment Score",title="Hourly Sentiment Analysis based on LM Dictionary")
  }else{if(length(unique(hourly_score_LM$created_datetime)) > 1)
  {hourly_score_LM%>%
      ggplot(aes(x= created_datetime, y= hourly_lm_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      scale_x_datetime(limits= c(max(daily_score_LM$created_datetime)))+
      labs(x="Created Day&Time",y="Hourly Sentiment Score",title="Hourly Sentiment Analysis based on LM Dictionary")
  }else{
    hourly_score_LM%>%
      ggplot(aes(x= created_datetime, y= hourly_lm_score))+
      geom_point(show.legend = F, col = "red")+
      geom_line()+
      labs(x="Created Day&Time",y="Hourly Sentiment Score",title="Hourly Sentiment Analysis based on LM Dictionary")
  }
  }
}

#Output-longterm sentiment analysis base on 10-k 
#get MDA part of the lastest 10-k
get_MDA <- function(df){
  useful_search_key <- toupper(df)
  
  test_10k <- 
    company_filings(
      useful_search_key,
      ownership = FALSE,
      type = "10-K",
      before = Sys.Date(),
      count = 10
    )%>%
    filter(year(filing_date ) == year(Sys.Date()))%>%
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

#sentiment analysis based on LM
MDA_sentiment_LM <- function(df){
  analyzeSentiment(df)$SentimentLM
}





