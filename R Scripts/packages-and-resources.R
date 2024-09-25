###############################################################
#Day 3, Session two: Dashboarding Polarized Views on Artificial Intelligence (Focus on Web Scraping)
# ########################################################

#For hands-on practice, participant should install the following R packages:

install.packages(c("tidyverse",
                 "RedditExtractoR",
                 "vosonSML",
                 "tidytext",
                 "SentimentAnalysis",
                 "syuzhet",
                 "shiny",
                 "VOSONDash",
                 "shinydashboard",
                 "shinythemes",
                 "stopwords",
                 "rsconnect",
                   "DT",
                   "plotly"
                 ))

#'1. `tidyverse` - The core tidyverse includes the packages that you're likely to use in everyday data analyses.

#'2. `RedditExtractoR` - Reddit data extraction toolkit

#'3. `vosonSML` - For collecting social media data (including Youtube and Reddit data) and generating networks for analysis 

#'4. `SentimentAnalysis` - a powerful toolchain facilitating the sentiment analysis of textual contents in R. This implementation utilizes various existing dictionaries, such as Harvard-IV, Henry’s Financial dictionary (Henry 2008), Loughran-McDonald Financial dictionary (Loughran and McDonald 2011), and QDAP dictionary from the package qdapDictionaries 

#'5. `syuzhet` - Extracts sentiment and sentiment-derived plot arcs from text using a variety of sentiment dictionaries. 
#'The `get_nrc_sentiment()` in the package is particularly important as it implements Saif Mohammad’s NRC Emotion lexicon. “The NRC emotion lexicon is a list of words and their associations with eight emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive)”.

#'6. `tidytext` - For text mining using `dplyr`, `ggplot2`, and other Tidy tools. Uses here for text analysis and sentiment analysis

#'7. `shiny` - web application framework for R

#'8. `VOSONDash` - an interactive R Shiny web application for the visualisation and analysis of social network data.

#'9. `shinythemes` - Themes for shiny

#'10. `shinydashboard` - For using Shiny to create dashboards



#' RECOMMENDED MATERIALS:
  #1: Text Mining with R: A Tidy Approach
  #' Link to ebook built with `bookdown`: https://www.tidytextmining.com/ 
  #' Focus: 2 Sentiment analysis with tidy data (https://www.tidytextmining.com/sentiment)

  #'2: R for Data Science (Second edition)
  #' Link to ebook: https://r4ds.hadley.nz/ 
  #' Focus: 24  Web scraping (https://r4ds.hadley.nz/webscraping)

  #'3:  Shiny Dashboard
  #'Link to resource (https://rstudio.github.io/shinydashboard/index.html)
  
  #'4:  Mastering Shiny
  #'Link to ebook (https://mastering-shiny.org/)
