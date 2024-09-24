################################################################################
#
#' Dashboarding Polarized Views on Artificial Intelligence (Focus on Web Scraping)
#' 
#  ########################################################
#  

#'PACKAGES INSTALLATION
library(tidyverse) #The core tidyverse includes the packages that you're likely to use in everyday data analyses
library(RedditExtractoR) #Reddit data extraction toolkit
library(vosonSML) #'For collecting social media data (including Youtube and Reddit data) and generating networks for analysis 
library(tidytext) #'For text mining using `dplyr`, `ggplot2`, and other Tidy tools. Uses here for text analysis and sentiment analysis
library(SentimentAnalysis) #'Apowerful toolchain facilitating the sentiment analysis of textual contents in R.
library(syuzhet) #'Extracts sentiment and sentiment-derived plot arcs from text using a variety of sentiment dictionaries.
library(shiny) #'web application framework for R
library(shinydashboard) #'For using Shiny to create dashboards
library(VOSONDash) #'an interactive R Shiny web application for the visualisation and analysis of social network data.


##############################################################
# PART A: FETCHING DATA FROM REDDIT
##' To analyse the extent of polarisation regarding AI, we would extract comments and posts associated with AI on Reddit. To get posts and comments from reddit, we will rely on the `RedditExtractoR` package. Using this package, we can extract relevant posts (thread) and their associated comments by first collecting the links/urls to the post, followed by the extraction of the comments of each associated urls.

###' A post (thread) is relevant when the central theme of discussion is AI, especially the benefits or harms associated with its usage, adoption, emergence, or reliance.
###' 
###' To select threads that meet this criteria, we will restrict our search to sub-reddits (communities) that are particularly concerned/interested in post/issues associated with AI.
###' 
###' Due to the size of post and comments on reddit, we will restrict our search to just one sub-reddit that is very active and have significant number of members.
###' 
###' To this end, we narrow our pick to `ArtificialInteligence` (r/ArtificialInteligence) with 676k members. The discussions here is centered on the many different facets of the AI community.

#' Using the `find_thread_urls()` function in the `RedditExtractor` package, there are two strategies to search URLs to reddit threads of interest: by keywords and by home page. Using a set of keywords Can help narrow down the search to a topic of interest that crosses multiple subreddits whereas searching by home page can help  find, for example, top posts within a specific subreddit. 
#' 
#' 
#' Since we are interested in posts in certain subreddits, we will adopt the second strategy

#' 
#' Extract urls for `ArtificialInteligence` subreddit 
urlsArtificialIntelligence <- find_thread_urls(keywords=NA, 
                                               subreddit="ArtificialInteligence", 
                                               sort_by="hot", #'new', 'top', and 'rising' options are also acceptable
                                               period="all") #A string representing the period of interest (hour, day, week, month, year, all)


#' Having extracted the urls, the next thing is to extract the contents of each of the posts (thread) associated with the urls pulled.
#' 
#' This can be achieved using the `get_thread_content()` function from the `RedditExtractoR` package.
#' 
#'   


#' Extract contents of `ArtificialInteligence` subreddit 
content_ArtificialIntelligence <- get_thread_content (url = urlsArtificialIntelligence$url)


#' The ensuring object is a list we two dataframe. We extract only the dataframe with comments as follows:
#' 
comments_Artificialinteligence <- tibble::as_tibble(content_ArtificialIntelligence$comments) 

#Save the comments
saveRDS(comments_Artificialinteligence,
        file="comments_Artificialinteligence.RDS")

##############################################################
#'Sentiment analysis
##############################################################
#' Data preprocessing
#' First i removed deleted comments and select some columns that we will be working with. I also create two aditional columns for year and month from the date column

comments_Artificialinteligence_upt <- comments_Artificialinteligence%>%
  filter(comment != '[removed]')%>% #Remove comments that have been deleted
  select(date:comment_id, url, -timestamp) %>%
  arrange(date) %>%
  mutate(
    date = as_date(date),  # Convert to date format
    year = year(date),     # Extract year
    month = month(date, label = TRUE, abbr = FALSE)
  ) %>%
  select(date, year, month, everything())  # Reorder columns


#'There are several packages that are used for sentiment analysis. The `tidytext` package is the perhaps the most popular.

#' However, The `tidytext` package in R is typically used for text mining and sentiment analysis through tokenization, which means breaking the text into individual words (tokens) before analysis.  

#' Tokenising the comments is not the most efficient use of time given the number of rows of comments.

#' Thus, the `analyzeSentiment()` in `SentimentAnalysis` package. In other words, this packages can be used without breaking the sentence into tokens.

reddit_sentiment <- analyzeSentiment(comments_Artificialinteligence_upt$comment,
                                     removeStopwords= F,
                                     language = 'english')


#' Besides the `SentimentAnalysis` packag, I also employ the  `get_nrc_sentiment()` in  the `syuzhet`package to generate the emotions associated with each of the comments.
#' 

reddit_emotion_sentiment <- get_nrc_sentiment(comments_Artificialinteligence_upt$comment,
                                              lowercase = TRUE,
                                              language = 'english')


##Combine the sentiment analysis dataset
sentiment_analysis <- bind_cols(reddit_sentiment,
                                reddit_emotion_sentiment)

#'Merge the Sentiment analysis dataframe generated with the original dataset. This will enable use present the sentiment analysis alongside the components of each of the comments (such as date, year, month, upvote, etc.)
comment_and_sentiments <- comments_Artificialinteligence_upt%>%
  select(date:downvotes, comment, url)%>%
  bind_cols(sentiment_analysis)

####Save
saveRDS(comment_and_sentiments,
        file="comment_and_sentiments.RDS")
