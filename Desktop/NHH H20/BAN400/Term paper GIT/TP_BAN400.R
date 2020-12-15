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
library(SnowballC)
library(quantmod)
library(tseries) 

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
                     checkboxInput("hold", label = "Shareholding", value = F),
                     
                     #display only if the chcekbox"hold" is checked
                     conditionalPanel(condition = "input.hold === true",
                                      numericInput("shares", label = "Holding shares", value = 100),
                                      numericInput("price", label = "Average purchase price", value = 100)),
                     
                     
                     # select number of tweets to be retrived
                     sliderInput(inputId = "number_tweets", label = "Number of tweets to be retrived", 
                                 min = 500, max = 18000, value = 500, step = 500), 
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
                                tableOutput("MDA_sentiment_table")),
                       tabPanel("Real_time stock price", 
                                plotOutput("stock_price_plot")),
                       tabPanel("Trade advises", textOutput("trade_advises"))
                     )
                   )
                 )
)





#Define server(finished)############################################################################
server <- function(input, output) {
  
  #1 PREPARATION(define all needed functions)
  
    #1.1  extract data
    extract_tweets <-reactive({
      #validation
      #req(input$search_key)
      #validate(need(toupper(input$search_key) %in% ticker_symbol),
      #       "Error: Please enter a vaild search key.")
      
      #name the extract_tweet function
      extract_tweet <- function(df){
        token <- create_token(app = "homework",
                              consumer_key="S9tfz9SiSDhPj3ogykznmCQ9y",
                              consumer_secret="GttpgpiOcTBMuEL22y5w3mKymNORWKyuaIRpB34uMUjqSifWef",
                              access_token="1294199419097636864-uJ3M5RfVtYn9EZqVn0t6StWaVnWJxg",
                              access_secret="7UoNPbQsuN4wRmkLSlElWBuCW6j99FBBVl1OcpkUWHWWe" ) 
        #this is a private token expired on 20210102
        
        usefual_search_key <- c( paste("$",toupper(df),sep = ""))
        
        
        search_tweets(
          usefual_search_key,                          #search key 
          n = input$number_tweets,                      #number of tweets  shall be input$number_tweets
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
      
      #then use the function
      extract_tweet(input$search_key)
      
    })
    
    
    #Search key must be in this dataset
    #ticker_symbol <- supported_tickers()%>%
    #  filter(exchange == "AMEX" | exchange == "NASDAQ" | exchange == "NYSE" )%>%
    #  select(ticker)%>%
    #  as.matrix()%>%
    #  as.vector()
    
    #vaild_ticker_symbol <- function(df){
    #  
    #  print(toupper(df) %in% ticker_symbol)
    #  
    #}
  
    #1.2 Plot function-how many tweets retrived per day
    plot_tweet_dt <- function(df){
      useful_df <- df %>%
        group_by(created_day, created_time)%>%
        summarize(tw_count = n())
      
      plot <- useful_df %>%
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
  
    #1.3 Preprocess function
    cleaning_tw_df <- function(df){
      extract_hashtag <- 
        DocumentTermMatrix(Corpus(VectorSource(df$hashtags)),
                           control = list( removePunctuation = T,
                                           stripWhitespace = T,
                                           tolower = T ))%>%
        as.matrix()%>%
        as.data.frame()%>%
        colnames()%>%
        unique()
      #extract hashtags cantained in each tweets
      
      extract_ticker <- 
        DocumentTermMatrix(Corpus(VectorSource(df$symbols)),
                           control = list( removePunctuation = T,
                                           stripWhitespace = T,
                                           tolower = T ))%>%
        as.matrix()%>%
        as.data.frame()%>%
        colnames()%>%
        unique()
      #extract ticker symbols cantained in each tweets
      
      remove_URL <- function(df){
        
        data <- NA
        
        for (i in 1:nrow(df)){
          extractURL1 <-
            df$urls_t.co[i]%>%
            unlist()
          
          
          extractURL2 <-
            df$media_t.co[i]%>%
            unlist()
          
          combineURL <- c(extractURL1,extractURL2)%>%
            unique()
          
          data[i] <- removeWords( df$text[i], combineURL)
        }
        print(data)
      }
      
      cleaned_text <- 
        df%>%
        remove_URL()%>%
        as.vector()%>%
        tolower() %>%                          #convert to lower case 
        gsub("[[:punct:]]", " ", .) %>%        #remove punctuation
        gsub("amp"," ", .)%>%                  #remove amp
        gsub("https", " ", .)%>%               #remove https
        removeWords(.,extract_hashtag)%>%      #remove hashtags
        removeWords(.,extract_ticker)%>%       #remove tickers symbols
        gsub("[[:digit:]]", "", .) %>%         #remove digits
        removeWords(., stopwords("en")) %>%    #remove standard stopwords
        gsub('\\b\\w{1,2}\\b','', .) %>%       #remove words of length 1-2
        gsub("\\s(\\s*)", " ", .) %>%          #remove excess whitespace
        trimws()%>%                            #remove first space
        as.matrix()%>%
        as.data.frame()
      
      colnames(cleaned_text) <-c( "text")
      #get cleaned tweet text
      
      cleaned_tw <- 
        df%>%
        select(-text)%>%
        cbind(cleaned_text)%>%
        select(user_id, text, source, favorite_count, retweet_count, quote_count, reply_count, followers_count,
               favourites_count, created_day, created_time, created_datetime)
      #replace text with cleaned text and select valuabe columns
    }
  
    #1.4 Plot function-wordcloud words appeared most frequently agmong tweets retrived
    make_tweet_wordcloud<- function(df){
      x <- 
        DocumentTermMatrix(Corpus(VectorSource(df$text)),
                           control = list(stemming = T,
                                          bounds = list(global = c(5,input$number_tweets/2)))) %>%   #500 = input.number_tweets
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
    
    #1.5 sentiment analysis function
    tweet_sentiment_analysis <- function(df){
      if(input$frequent == "Daily"){
        daily_sentiment_analysis(df)
      }else{hourly_sentiment_analysis(df)
      }
    }
  
      #1.5.1 daily sentiment analysis base on different dictionary
      daily_sentiment_analysis <- function(df){
        if(input$dictionary == "Harvard"){
          daily_harvard_sentiment(df)
        }else{
          daily_lm_sentiment(df)
        }
      }
      
      #1.5.2  harvard sentiment function
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
      
      #1.5.3 LM sentiment function
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
      
      #1.5.4 hourly sentiment analysis base on different dictionary 
      hourly_sentiment_analysis <- function(df){
        if(input$dictionary == "Harvard"){
          hourly_harvard_sentiment(df)
        }else{
          hourly_lm_sentiment(df)
        }
      }
      
      #1.5.5 harvard sentiment function
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
      
      #1.5.6 LM sentiment function
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
  
    #1.6 get MDA part of the lastest 10-k
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
        filter(year(filing_date) == max(year(filing_date)))%>%
        select(href)%>%
        as.matrix()%>%
        as.vector()%>%
        filing_documents()%>%
        filter(type == "10-K")%>%
        select(href)%>%
        as.matrix()%>%
        as.vector()%>%
        parse_filing()%>%
        filter(item.name == "Item 7" | item.name == "item 7" | 
                 item.name == "Item7" | item.name == "Item7" |
                 item.name == "ITEM7" | item.name == "ITEM 7" |
                 item.name == "Item 7. Management's Discussion and Analysis of Financial Condition and Results of Operations" |
                 item.name == toupper("Item 7. Management's Discussion and Analysis of Financial Condition and Results of Operations"))
    }
    
    #1.7 get cleaned MDA
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
  
    #1.8 make a wordcloud
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
  
    #1.9 sentiment analysis based on LM
    MDA_sentiment_LM <- function(df){
      s <- analyzeSentiment(df)[c(1,8,9,10)]%>%
        as.data.frame()
    }
    
  #2 OUTPUT
    #2.1 tweets part
  
      #2.1.1 clean data
      cleaned_tweets <- reactive({
        cleaning_tw_df(extract_tweets())
      })
      
      #2.1.2 plot-how many tweets retrived per day&time
      output$tweets_number_plot <- renderPlot({
        plot_tweet_dt(extract_tweets())
      })
      
      #2.1.3 plot-wordcloud
      output$tweets_wordcloud_plot <- renderPlot({
        make_tweet_wordcloud(cleaned_tweets())
      })
      
      #2.1.4 plot-tweets sentiment
      output$tweets_sentiment_plot <- renderPlot({
        tweet_sentiment_analysis(cleaned_tweets())
      })
  
    #2.2 10-k MDA  part
  
      #2.2.1 get cleaned text of 10k MDA (works) 
      get_cleaned_MDAs <-reactive({
        get_cleaned_MDA(get_MDA(input$search_key))})
      
      #2.2.2 plot-10kMDA wordcloud (works)
      output$MDA_wordcloud_plot <- renderPlot({
        make_MDA_wordcloud(get_cleaned_MDAs())
      })
      
      #2.2.3 plot-10kMDA sentiment(works)
      output$MDA_sentiment_table <- renderTable({
        MDA_sentiment_LM(get_cleaned_MDAs())
      })
  
    
    #2.3 Real_time stock price part
      stock_price <- function(df) {
        
        temp_data <- getSymbols(df ,src="yahoo",from=Sys.Date()-90,to=Sys.Date(), auto.assign = F)
        plot <- quantmod::chartSeries(temp_data,up.col='red', dn.col='green',theme="white",
                                      TA=c(addBBands(),addMACD(),addADX(),addVo()))
        
      }
      
      # plot stock price plot
      output$stock_price_plot <- renderPlot({
        stock_price(input$search_key)
      })
  
       #3.1 trade advise part
       stock_signal <- function(df){
        yourstock <- c(toupper(df))
        stockprice <- get.hist.quote(instrument = yourstock, start = Sys.Date()-60, end = Sys.Date(),quote = "AdjClose")
        macd_data <- MACD(stockprice, percent = F) 
        DIFF <- macd_data$macd[length(macd_data$macd)]
        DEA <- macd_data$signal[length(macd_data$signal)]
        macd <- 2 * (DIFF - DEA)
        
        if(DIFF > 0 & DEA > 0 & macd > 0 ){
          print("suggest to buy")
          
        }else if(DIFF < 0 & DEA < 0 & macd < 0){
          print("suggest to sell")
        }else{
          print("tight")
        }}
        
        # present advise base on analyze
        output$trade_advises <- renderText ({
          stock_signal(input$search_key)
        })
        
      
      
      
      
}

shinyApp(ui = ui, server = server)
  
  
  


########################################################################################

# Stock Price Prediction Using Sentiment Analysis: Boeing Stock

library(twitteR)
library(httr)
library(stringr)
library(ggplot2)

key = "S9tfz9SiSDhPj3ogykznmCQ9y"
secret =
  "GttpgpiOcTBMuEL22y5w3mKymNORWKyuaIRpB34uMUjqSifWef"
mytoken = "1294199419097636864-uJ3M5RfVtYn9EZqVn0t6StWaVnWJxg"
secrettoken =
  "7UoNPbQsuN4wRmkLSlElWBuCW6j99FBBVl1OcpkUWHWWe"

# keep this order of arguments
setup_twitter_oauth(key, secret, mytoken, secrettoken)

tweets = searchTwitter("@Boeing OR #Boeing", n = 4500, since = "2020-12-08", lang =
                         "en")

#convert list to data frame
tweets.df <- twListToDF(tweets)

library(dplyr)

tweets.nodups.df <- distinct(tweets.df, text, .keep_all =
                               TRUE)

tweets.nodups.df$text <- gsub('…', '',
                              tweets.nodups.df$text)

tweets.nodups.df <- plyr::rename(tweets.nodups.df,
                                 c("created" = "Date")) #rename created to Date

tweets.nodups.df$Date <- as.Date(tweets.nodups.df$Date)
#convert from datetime to date format

#create text list with tweets for sentiment analysis
tweets_text <- lapply(tweets, function(x) x$getText())

#fix Mac encoding issue with
tweets_text <- sapply(tweets_text,function(row)
  iconv(row, to = 'UTF-8-MAC', sub = 'byte'))

#removing duplicate tweets (retweets) from list
tweets_nodups_text <- unique(tweets_text)

library(NLP)
library(tm)

#Create tweet corpus
r_stats_text_corpus <-
  Corpus(VectorSource(tweets_nodups_text))

#Clean up corpus in prepartion for word cloud
#Encoding corrections for Mac
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(tolower)) #Transform all text to lower case
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              removePunctuation) #remove all punctuation
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              function(x)removeWords(x,stopwords())) #remove all stop words

library(wordcloud)
#Create color word cloud
wordcloud(r_stats_text_corpus, min.freq = 10, max.words =
            150, colors=brewer.pal(8, "Dark2"))

score.sentiment = function(sentences, pos.words,
                           neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words,
                                     neg.words) {
    
    # clean up sentences with R's regex-driven global
    #substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr
    #package
    word.list = str_split(sentence, '\\s+')

    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive &
    #negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or
    #NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated
    #as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#The positive and negative words lexicons are stored in a
#local directory. Txt files are supplied separately.


hu.liu.pos = scan('positive-words.txt', what =
                    'character', comment.char = ';')
hu.liu.neg = scan('negative-words.txt', what =
                    'character', comment.char = ';')


pos.words <- c(hu.liu.pos)
neg.words <- c(hu.liu.neg)

#run the sentiment function on the text of the tweets
boeing.scores <- score.sentiment(tweets_nodups_text,
                                 pos.words, neg.words, .progress='none')



#merge the results back with the original file
boeing.score.merge <- merge(boeing.scores,
                            tweets.nodups.df, by = 'text')

#Histogram of sentiment for all tweets
hist(boeing.score.merge$score,xlab=" ",main="Sentiment of
tweets that mention Boeing", border="black",col="skyblue")

#scatter plot of tweet date vs sentiment score
plot(boeing.score.merge$Date, boeing.score.merge$score,
     xlab = "Date", ylab = "Sentiment Score", main = "Sentiment of tweets
that mention Boeing by Date")

#taken from https://www.r-bloggers.com/twitter-sentimentanalysis-with-r/
#total evaluation: positive / negative / neutral
stat <- boeing.score.merge$score
stat <- mutate(boeing.score.merge,
               tweet=ifelse(boeing.score.merge$score > 0, 'positive',
                            ifelse(boeing.score.merge$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, Date)
by.tweet <- dplyr::summarise(by.tweet, number=n())

#Sentiment (positive, negative and neutral) over time
ggplot(by.tweet, aes(Date, number)) +
  geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x =
          element_text(angle=90, vjust=1))

#Read stock price CSV in
stock_prices <- read.csv("BA.csv")

#Format date so R knows this is a date field
stock_prices$Date <- as.Date(stock_prices$Date,
                             "%Y-%m-%d")
Daily.change<- c(-2.13,-6.58,6.33,-2.52,-6.38)
stock_prices<-cbind(stock_prices,Daily.change)

#Left join the sentiment analysis with the stock prices
tweet_stock <- left_join(boeing.score.merge,
                         stock_prices, by = "Date")


#eliminate rows with no daily change
#eliminates weekend tweets
weekday_tweet_stock <- subset(tweet_stock,
                              !is.na(Daily.change))


#Raw plot of sentiment score versus daily change in stock
#price
plot(jitter(weekday_tweet_stock$score),
     weekday_tweet_stock$Daily.change, xlab = "Sentiment Score", ylab =
       "Daily Change in Stock Price")

#The below was modified from a LinkedIn PPT describing
#sentiment analysis in R


#Create indicator fields to flag tweets as positive,
#negative or neutral based on sentiment score
weekday_tweet_stock$pos <-
  as.numeric(weekday_tweet_stock$score >= 1)
weekday_tweet_stock$neg <-
  as.numeric(weekday_tweet_stock$score <= -1)
weekday_tweet_stock$neu <-
  as.numeric(weekday_tweet_stock$score == 0)

#Transform file from one row per tweet to one row per day
#summarizing the total positive, negative and netural tweets per day 
tweet_stock_df <- ddply(weekday_tweet_stock, c('Date',
                                               'Daily.change'), plyr::summarise, pos.count = sum(pos),
                        neg.count = sum(neg), neu.count = sum(neu))

tweet_stock_df$all.count <- tweet_stock_df$pos.count +
  tweet_stock_df$neg.count + tweet_stock_df$neu.count

#calculate the percent of tweets that were negative on
#each day
tweet_stock_df$percent.neg <-
  round((tweet_stock_df$neg.count / tweet_stock_df$all.count) * 100)

#Simple correlation
cor(tweet_stock_df$percent.neg,
    tweet_stock_df$Daily.change, use = "complete")

glm_model <- glm(tweet_stock_df$Daily.change ~
                   tweet_stock_df$percent.neg)
summary(glm_model)

#plot of % positive tweets vs daily change in stock price
#with linear regression line overlaid
plot(tweet_stock_df$percent.neg,
     tweet_stock_df$Daily.change, ylab = "Daily Change in Stock Price",
     xlab = "Percent of Negative Tweets", main = "% Negative Tweets vs Daily
Stock Price Change for Boeing")
abline(glm_model)

#calculate the percent of tweets that were positive on
#each day
tweet_stock_df$percent.pos <-
  round((tweet_stock_df$pos.count / tweet_stock_df$all.count) * 100)

#Simple correlation
cor(tweet_stock_df$percent.pos,
    tweet_stock_df$Daily.change, use = "complete")

glm_model <- glm(tweet_stock_df$Daily.change ~
                   tweet_stock_df$percent.pos)
summary(glm_model)

#plot of % positive tweets vs daily change in stock price
#with linear regression line overlaid
plot(tweet_stock_df$percent.pos,
     tweet_stock_df$Daily.change, ylab = "Daily Change in Stock Price",
     xlab = "Percent of Positive Tweets", main = "% Positive Tweets vs Daily
Stock Price Change for Boeing")
abline(glm_model)


# advise function
stock_signal <- function(df){
  yourstock <- c(toupper(df))
  stockprice <- get.hist.quote(instrument = yourstock, start = Sys.Date()-60, end = Sys.Date(),quote = "AdjClose")
  macd_data <- MACD(stockprice, percent = F) 
  DIFF <- macd_data$macd[length(macd_data$macd)]
  DEA <- macd_data$signal[length(macd_data$signal)]
  macd <- 2 * (DIFF - DEA)
  
  if(DIFF > 0 & DEA > 0 & macd > 0 ){
    print("suggest to buy")
  }else{if(DIFF < 0 & DEA < 0 & macd < 0)
    print("suggest to sell")
  }else{
    print("tight")
  }
}


# present advise base on analyze
output$trade_advises <- renderText({
  stock_signal(input$search_key)
})
                   
