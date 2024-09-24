################################################################################
#
#' Dashboarding Polarized Views on Artificial Intelligence (Focus on Web Scraping)
#' 
#  ########################################################
#  

#'Required packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(RedditExtractoR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinythemes)
library(rsconnect)
library(DT)
library(plotly)


#'
#'##############################################################
#Dashboarding
##############################################################
#'Import data
#'#We first import the dataset
comment_and_sentiments <- read_rds("https://smalljosephd.github.io/files/data/large_reddit_ai_comments_and_sentiments.RDS")
#I am using a dataset with 40k comments

#'The shiny App dashboard comprises of three components: The User Interface (ui.R); the server function (server.R); and the shinyApp function (which fuses the user interface and the server components)
#'
#'The `UI` is the frontend that accepts user input values. 
#'On the other hand, the `server` is the backend that process these input values to finally produce output results that are displayed on he web
#'

#' The ui.R component is made up of three parts: a header, a sidebar, and a body. 

#'Here's the header
header <- dashboardHeader(title = "AI-Pol-V",
                          titleWidth = 180)
#'Here's the sidebar 
sidebar <- dashboardSidebar(
  sidebarMenu(
    width = 150,
    menuItem("Sentiments Analysis A",
             tabName = "sentiment",
             icon = icon("bar-chart")),
    menuItem("Sentiment Analysis B",
             tabName = "others",
             icon = icon("line-chart"),
             badgeLabel = "new",
             badgeColor = "green"),
    menuItem("Reddit comment extractor",
             tabName = "extract",
             icon = icon("search"),
             badgeLabel = "",
             badgeColor = "green"),
    menuItem("Source Code", 
             tabName = "source_code", 
             icon = icon("code")),
    menuItem("Github", icon = icon("github"),
             href = "https://github.com/SmallJosePhD/Dashboarding-Polarized-Views-on-AI"),
    menuItem("Website", icon = icon("globe"),
             href = "https://smalljosephd.github.io"),
    menuItem("X (Twitter)", icon = icon("twitter"),
             href = "https://twitter.com/SmallJosePhD1"),
    menuItem("Google Scholar", icon = icon("graduation-cap"),
             href = "https://scholar.google.com/citations?hl=en&user=ghb58Y0AAAAJ"),
    menuItem("LinkedIn", icon = icon("linkedin"),
             href = "https://linkedin.com/in/josephdavid970")
    )
  )

#'and then the body
body <- dashboardBody(
  tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
        }
    '))),
  tabItems(
    # Source Code Tab
    tabItem(tabName = "source_code",
            fluidRow(
              box(title = "Source Code", 
                  status = "primary",
                  solidHeader = TRUE, 
                  width = 12,
                  pre(textOutput("source_code"))
              ))
    ),
    # First tab content
    tabItem(tabName = "sentiment",
            box(
              title = h3("Polarized Views on Artificial Intelligence"),
              width = NULL, 
              solidHeader = TRUE,
              "AI-Polar-V tracks the polarised views of individuals on Reddit on the emergence, adoption, and reliance (on) Artificial Intelligence."),
            fluidRow(
              box(title = tagList("The problem", icon("bug")),
                  width = 4,
                  solidHeader = TRUE,
                  height = 208,
                  #background = "black",
                  "The problem..."),
              box(title = tagList("Our approach", icon("tasks")), 
                  width = 4, 
                  solidHeader = TRUE,
                  height = 208,
                  # background = "light-blue",
                  "I adopted the sentiment analysis approach to track/determine the extent to which views on AI emergence, usage and reliance is polarized."),
              box(title = tagList("Our findings", icon("pie-chart")), 
                  width = 4, 
                  solidHeader = TRUE,
                  height = 208,
                  #background = "maroon",
                  "Preliminary findings based on the sentiment analysis conducted reveal an interesting pattern with regards to how individuals, based on comments on Reddit, view the emergence, adoption and reliance on Artiticial Intelligence. Specifically, the outcome demonstrate that ....")),
            fluidRow(
              box(title = "Sentiment Analysis (Emotions)",
                  status = "primary",
                  "The sentiments towards AI is more positive as ...",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("sentimentPlot", height = 300)),
              box(
                title = "Period",
                status = "primary",
                "Use the slider to change the year.",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                sliderInput("yearInput", "Select Year:",
                            min = min(comment_and_sentiments$year),
                            max = max(comment_and_sentiments$year),
                            value = min(comment_and_sentiments$year),
                            step = 1,
                            sep = "")
              )),
            fluidRow(
              valueBoxOutput("threads",
                             width = 6),
              valueBoxOutput("comments",
                             width = 6),
            )),
    # Second tab content
    tabItem(tabName = "others",
            fluidRow(
              box(title = "Sentiment Analysis B", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  plotOutput("sentimentPlotB", height = 300)),
              box(
                title = "Period",
                status = "primary",
                "Use the slider to change the year.",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                sliderInput("yearInput", "Select Year:",
                            min = min(comment_and_sentiments$year),
                            max = max(comment_and_sentiments$year),
                            value = min(comment_and_sentiments$year),
                            step = 1,
                            sep = "")
              )
              
            ),
            fluidRow(
              valueBoxOutput("threadsB",
                             width = 6),
              valueBoxOutput("commentsB",
                             width = 6),
            )),
    #Third tab
    tabItem(tabName = "extract",
            fluidRow(
              box(
                title = "Enter Reddit URL",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                textInput("reddit_url", "Reddit URL", placeholder = "Enter Reddit URL here"),
                actionButton("extract_btn", "Extract Comments"),
                style = "text-align: center;"
              )
            ),
            fluidRow(
              box(
                title = "Extracted Comments", 
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                DTOutput("comments_table")   # This will render the table of comments
              )
            ))
  ))

#' I decided to create different objects for the components of the ui. This is to enable that our code is free from error 
#' Now define UI for the dashboard.
ui <- dashboardPage(header,
                    sidebar,
                    body,
                    skin = "blue")

# Now Define the server logic
server <- function(input, output) {
  output$sentimentPlot <- renderPlotly({
    aggregate_scores <- comment_and_sentiments %>%
      filter(year == input$yearInput) %>%
      select(anger:positive) %>%
      summarise(across(everything(), sum))%>%
      pivot_longer(cols = everything(), names_to = "Sentiment", values_to = "Score")%>%
      mutate(Sentiment = str_to_sentence(Sentiment))

    # ggplot(aggregate_scores,
    #        aes(x = Sentiment, y = Score, fill = Sentiment)) +
    #   geom_bar(stat = "identity") +
    #   theme_minimal() +
    #   labs(title = "Sentiment on AI Discussions on Reddit", x = "Sentiment", y = "Aggregate Score") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")  # Rotate x-axis labels
    # 
  # Create plotly bar plot with tooltips
    #Set up color
    mycolors <- c("red","green","blue","purple","darkorange","#950","pink","brown","skyblue","#FFC125")
    
    plot_ly(
    aggregate_scores,
    x = ~Sentiment,            # X-axis with sentiment labels
    y = ~Score,                # Y-axis with the sentiment scores
    type = 'bar',              # Bar chart type
    #hoverinfo = 'text',        # Show text on hover
    #text = ~paste("Sentiment:", Sentiment, "<br>Score:", Score),  # Hover text
    marker = list(color = mycolors)  # Color based on sentiment
  ) %>%
    layout(
      title = "Sentiment on AI Discussions on Reddit",
      xaxis = list(title = "Sentiment"),
      yaxis = list(title = "Aggregate Score"),
      margin = list(b = 100),  # Add margin for rotated labels
      showlegend = FALSE       # Remove legend
    )
})
  #THis is the output for out second tab
  output$sentimentPlotB <- renderPlot({
    aggregate_scoresB <- comment_and_sentiments %>%
      filter(year == input$yearInput) %>%
      select(contains('Sentiment'))%>%
      summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))%>%
      pivot_longer(cols = everything(), names_to = "Sentiment", values_to = "Score")
    
    ggplot(aggregate_scoresB,
           aes(x = Sentiment, y = Score, fill = Sentiment)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Sentiment on AI Discussions on Reddit", x = "Sentiment", y = "Aggregate Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")  # Rotate x-axis labels
  })
  
  output$threads <- renderValueBox({
    threads <- comment_and_sentiments %>%
      distinct(url)%>%
      nrow()
    
    valueBox(
      paste0(sum(threads)),
      subtitle = "Reddit threads",
      icon = icon("area-chart"),
      color = "light-blue")
  })
  
  output$comments <- renderValueBox({
    comments <- comment_and_sentiments %>%
      summarize(row_count = n())
    
    valueBox(
      paste0(sum(comments$row_count)),
      subtitle = "Total comments",
      icon = icon("comment"),
      color = "light-blue")
  })
  
  output$threadsB <- renderValueBox({
    threadsB <- comment_and_sentiments %>%
      distinct(url)%>%
      nrow()
    
    valueBox(
      paste0(sum(threadsB)),
      subtitle = "Reddit threads",
      icon = icon("area-chart"),
      color = "light-blue")
  })
  
  output$commentsB <- renderValueBox({
    commentsB <- comment_and_sentiments %>%
      summarize(row_count = n())
    
    valueBox(
      paste0(sum(commentsB$row_count)),
      subtitle = "Total comments",
      icon = icon("comment"),
      color = "light-blue")
  })
  
  
  # Reactive event when the extract button is pressed
  comments_data <- eventReactive(input$extract_btn, {
    req(input$reddit_url)   # Ensure URL is provided
    
    # Fetch comments from the Reddit thread using the provided URL
    tryCatch({
      # Use get_thread_content to get the comments from the provided Reddit URL
      thread_content <- get_thread_content(input$reddit_url)
      
      # Access the comments data frame inside the returned list
      comments_df <- thread_content[["comments"]]
      
      # If no comments are found, return a data frame with a message
      if (is.null(comments_df) || nrow(comments_df) == 0) {
        return(data.frame(Author = "No Data", Comment = "No comments found"))
      }
      
      # Return a data frame with relevant comment information
      return(data.frame(
        Author = comments_df$author,
        Comment = comments_df$comment,
        Date = comments_df$date,
        Upvotes = comments_df$upvotes,
        Downvotes = comments_df$downvotes,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      # In case of an error, return an error message in the table
      return(data.frame(Author = "Error", Comment = "Invalid Reddit URL or Data Extraction Failed"))
    })
  })
  
  # Render the comments as a table
  output$comments_table <- renderDT({
    comments_data()   # Render the data frame as a DataTable
  }, options = list(pageLength = 5))  # Optionally limit the number of comments per page for readability
  
  # Render the source code
  output$source_code <- renderText({
    paste(readLines("reddit_dashboard.R"), collapse = "\n")
  })
  
}

# We can preview our dashboard (the UI) in the console using the shinyApp() function.
shinyApp(ui = ui, 
         server = server)
