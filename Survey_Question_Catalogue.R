library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(psych)
library(gt)
library(shinyjs)
library(wordcloud2)
library(syuzhet)


# *********************** LOAD & CLEAN DATA ***************************
rm(list = ls())

# setwd("C:/Users/OEM/OneDrive/Documents/R/Survey_Question_Catalogue")

questions <- read.csv("Questions.csv", header = TRUE)

# surveys <- list.files(path = "C:/Users/OEM/OneDrive/Documents/R/Survey_Question_Catalogue/Fake_Survey_Data", pattern = '.csv$', full.names = T) %>%
#   map(read_csv)

surveys <- list.files(pattern = '.csv$', full.names = T) %>%
  map(read_csv)

questions <- questions %>% mutate_all(na_if,"")
questions$Date <- paste0(questions$Date, "/01/01")
questions$Date <- as.Date(questions$Date)

uniqueTags <- list(questions$Tag1, questions$Tag2, questions$Tag3, questions$Tag4)
uniqueTags <- Reduce(c,uniqueTags)
uniqueTags <- unique(uniqueTags)
uniqueTags <- uniqueTags[!is.na(uniqueTags)]

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      #mainPanel {
        margin-left: 1vw;
      }
      #questionNameText {
        font-weight: 700;
        font-family: 'Poppins', sans-serif;
      }
      #surveyNameText2 {
        font-family: 'Poppins', sans-serif;
      }
      #tableCol {
        width: 100%;
      }
      #preCol {
        width: 100%;
      }
      #table {
        padding:0.8vw;
        box-shadow: rgba(0, 0, 0, 0.24) 0px 3px 4px;
        background-color: #ecf0f185;
        border: 1px solid transparent;
        width: 75%;
        margin-bottom: 0.4vw;
      }
      #wordcloud {
        margin-bottom: 1vw;
      }
      #plot {
        margin-left: -2vw;
      }
      img {
        width: 55vw;
      }
      pre {
        font-family: 'Archivo Black', sans-serif;
        font-weight: bold;
        font-size: 1.3vw;
        text-align: center;
        color: #FFFFFF;
        background-color: #098ebb;
        width: 6vw;
        height: 4.5vw;
        padding:0.6vw;
        margin: 0.2vw;
        margin-bottom: 0.4vw;
        line-height:1.5vw;
        border:0px solid rgba(0,0,0,0.15);
        border-radius:4px;
        box-shadow: rgba(0, 0, 0, 0.24) 0px 3px 8px;
        float: left;
      }
      .filter-option-inner-inner{
        color: white;
      }
      "))
  ),
  theme = shinytheme("flatly"),
  navbarPage("Survey Questions Catalogue",
      tabPanel("Question Viewer",
          sidebarLayout(
              sidebarPanel(
                  width = "3",
                    pickerInput("uniqueTags", "Select one or more Tags", sort(uniqueTags), options = list(`actions-box` = TRUE), selected = uniqueTags, multiple=TRUE),
                    sliderInput("date", "Select date range", min = min(questions$Date, na.rm = TRUE), max = max(questions$Date, na.rm = TRUE), value = c(min(questions$Date, na.rm = TRUE), max(questions$Date, na.rm = TRUE))),
                    selectInput("location", "Select a location", choices = unique(sort(questions$Location)), selected = "All"),
                    selectInput("survey", "Select a survey", choices = c("All Surveys", unique(sort(questions$Survey_Name))), selected = "All"),
                    selectInput("quest","Select a question", choices = c(unique(questions$Question)), selected = sample(questions$Question, 1)),
                    h5(em(textOutput(outputId = "surveyNameText"))),
                    h5(em(textOutput(outputId = "locationText"))),
                    h5(em(textOutput(outputId = "sampleSizeText"))),
                    h5(em(textOutput(outputId = "dateText"))),
                    h5(em(textOutput(outputId = "dataTypeText"))),
                    h4(em(htmlOutput("qualityText"))),
                    h4(em(uiOutput("url"))),
                    headerPanel(""),
              ),
              mainPanel(
                id = "mainPanel",
                width = "7",
                h2(textOutput(outputId = "questionNameText")),
                h4(em(textOutput(outputId = "surveyNameText2"))),
                HTML("<hr>"),
                column(6,
                  id = "preCol",
                  verbatimTextOutput(outputId = "nTextBox"),
                  verbatimTextOutput(outputId = "meanTextBox"),
                  verbatimTextOutput(outputId = "medianTextBox"),
                  verbatimTextOutput(outputId = "sdTextBox"),
                  verbatimTextOutput(outputId = "madTextBox"),
                  verbatimTextOutput(outputId = "minTextBox"),
                  verbatimTextOutput(outputId = "maxTextBox"),
                ),
                column(6,
                   id = "tableCol",
                   tableOutput("table"),
                   wordcloud2Output('wordcloud'),
                ),
                HTML("<hr>"),
                column(6,
                  id = "plotCol",
                  plotOutput(outputId = "plot", width = "1000px", height = "500px"),
                ),
              )
          )
      ),
      tabPanel("About this Tool",
      
      )
  )
)

server <- function(input, output, session) {
  
  # *********************** DYNAMIC INPUTS ***************************
  
  observe({
    
    currentQuestion <- input$quest
    
    filteredSelections <- questions
    
    if(!is.null(input$uniqueTags)){
      filteredSelections <- filter(filteredSelections, Tag1 %in% input$uniqueTags | Tag2 %in% input$uniqueTags
                                   | Tag3 %in% input$uniqueTags | Tag4 %in% input$uniqueTags)
    }
    
    filteredSelections <- filter(filteredSelections, Date >= input$date[1])
    filteredSelections <- filter(filteredSelections, Date <= input$date[2])
    
    if(input$location != "All of NZ"){
      filteredSelections <- filter(filteredSelections, Location == input$location)
    }
    
    if(input$survey != "All Surveys"){
      filteredSelections <- filter(filteredSelections, Survey_Name == input$survey)
    }
    
    currentChoices <- unique(filteredSelections$Question)
    
    if(currentQuestion %in% currentChoices){
      updateSelectInput(session, "quest", choices = unique(filteredSelections$Question), selected = currentQuestion)
    }else{
      updateSelectInput(session, "quest",choices = unique(filteredSelections$Question))
    }
  })
  
  # *********************** SURVEY INFORMATION ***************************
  
  output$surveyNameText <- renderText({ paste0("Survey Name: ", questions$Survey_Name[questions$Question == input$quest]) })
  output$locationText <- renderText({ paste0("Location: ", questions$Location[questions$Question == input$quest]) })
  output$dateText <- renderText({
    date <- questions$Date[questions$Question == input$quest]
    date <- substring(date, 1, 4)
    paste0("Date: ", date)
  })
  output$sampleSizeText <- renderText({
    DFLocation <- which(sapply(surveys, function(x) any(names(x) == input$quest)))
    survey <- as.data.frame(surveys[DFLocation], check.names=FALSE)
    paste0("Sample Size: ", length(survey[, input$quest][!is.na(survey[, input$quest])])) 
    
  })
  output$dataTypeText <- renderText({ paste0("Data Type: ", questions$Data_Type[questions$Question == input$quest]) })
  output$url <- renderUI({
    url <- a("Link to the Report", href = paste0("https://", questions$Link[questions$Question == input$quest]))
    HTML(paste(url))
  })
  
  output$questionNameText <- renderText({ questions$Question[questions$Question == input$quest] })
  output$surveyNameText2 <- renderText({ questions$Survey_Name[questions$Question == input$quest] })
  
  output$qualityText <- renderText({
    dataQuality <- questions$Quality[questions$Question == input$quest]
    
    if(dataQuality == "Great"){
      return(paste("<span style=\"color:green\">Data Quality: Great</span>"))
      
    }else if(dataQuality == "Good"){
      return(paste("<span style=\"color:blue\">Data Quality: Good</span>"))
      
    }else if(dataQuality == "Bad"){
      return(paste("<span style=\"color:red\">Data Quality: Bad</span>"))
      
    }else{
      return("Data Quality: Average")
    }
    
  })
  
  # *********************** DATA TYPES ***************************
  
  observe({
    dataType <- questions$Data_Type[questions$Question == input$quest]
    DFLocation <- which(sapply(surveys, function(x) any(names(x) == input$quest)))
    survey <- as.data.frame(surveys[DFLocation], check.names=FALSE)
    
    if(dataType == "Discrete"){
      
      hide(id = "wordcloud")
      hide(id = "nTextBox")
      hide(id = "meanTextBox")
      hide(id = "medianTextBox")
      hide(id = "sdTextBox")
      hide(id = "madTextBox")
      hide(id = "minTextBox")
      hide(id = "maxTextBox")
      
      show(id = "table")
      
      tbl <- as.data.frame(table(survey[, input$quest]))
      colnames(tbl) <- c(input$quest , "Count")
      tbl <- tbl %>% mutate(Percentage = round(Count/sum(Count),2))
      tbl$Percentage <- paste0(tbl$Percentage * 100, "%")
      
      output$table <- renderTable({
        tbl
      },width = NULL)
      
      output$plot <- renderPlot({
        ggplot(tbl, aes(tbl[, input$quest], Count , fill = tbl[, input$quest])) +
          geom_bar(stat='identity') + labs(x = input$quest, fill = input$quest) + 
          theme(plot.title = element_text(family = "sans", size = 22, margin=margin(0,0,12,0)))
      })
    }
    
    if(dataType == "Continuous"){
      
      hide(id = "table")
      hide(id = "wordcloud")
      show(id = "nTextBox")
      show(id = "meanTextBox")
      show(id = "medianTextBox")
      show(id = "sdTextBox")
      show(id = "madTextBox")
      show(id = "minTextBox")
      show(id = "maxTextBox")
      
      
      tbl <- as.data.frame(describe(survey[, input$quest]))
      
      output$nTextBox  <- renderText({paste("N", round(tbl$n[1], 1), sep="\n")})
      output$meanTextBox  <- renderText({paste("Mean", round(tbl$mean[1], 1), sep="\n")})
      output$medianTextBox  <- renderText({paste("Median", round(tbl$median[1], 1), sep="\n")})
      output$sdTextBox  <- renderText({paste("SD", round(tbl$sd[1], 1), sep="\n")})
      output$madTextBox  <- renderText({paste("MAD", round(tbl$mad[1], 1), sep="\n")})
      output$minTextBox  <- renderText({paste("Min", round(tbl$min[1], 1), sep="\n")})
      output$maxTextBox  <- renderText({paste("Max", round(tbl$max[1], 1), sep="\n")})
      
      
      output$plot <- renderPlot({
        ggplot(survey, aes(x = survey[,input$quest])) +
          geom_histogram(colour="black", fill="cadetblue2", na.rm = TRUE) + labs(x = input$quest) +
          theme(plot.title = element_text(family = "sans", size = 22, margin=margin(0,0,12,0)))
      })
    }
    if(dataType == "Open"){
      
      hide(id = "table")
      show(id = "plot")
      hide(id = "nTextBox")
      hide(id = "meanTextBox")
      hide(id = "medianTextBox")
      hide(id = "sdTextBox")
      hide(id = "madTextBox")
      hide(id = "minTextBox")
      hide(id = "maxTextBox")
      show(id = "wordcloud")
      
      # openEnders
      openEnders <- gsub("'t|'s|'t|'re", "", survey[, input$quest])
      openEnders <- gsub("[(),.?:]", " ", openEnders)
      words <- strsplit(openEnders, " ", fixed = T)
      words <- unlist(words)
      words <- tolower(words)
      counts <- as.data.frame(table(words))
      counts <- counts[!(counts$words ==""),]

      cVector <- as.character(counts$words)
      sentiment <- get_nrc_sentiment(cVector)
      sentiment <- as.data.frame(colSums(sentiment))
      colnames(sentiment)[1] <- "Count"
      sentiment <- tibble::rownames_to_column(sentiment, "Emotion")
      sentiment$Emotion <- str_to_sentence(sentiment$Emotion)
      
      output$plot <- renderPlot({
        ggplot(data=sentiment, aes(x=Emotion, y=Count, fill=factor(ifelse(Emotion == "Positive" | Emotion == 'Negative' ,"Highlighted","Normal")))) +
          geom_bar(stat="identity", show.legend = FALSE)
      })
      
      output$wordcloud <- renderWordcloud2({
        wordcloud2(counts, size=1.6)
      })
      
    }
  })
}

shinyApp(ui, server)

