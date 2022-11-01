library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
require(psych)  


# *********************** LOAD & CLEAN DATA ***************************
rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Survey_Question_Catalogue")

#Sample size can be programatically obtained

questions <- read.csv("Questions.csv", header = TRUE)
questionData <- read.csv("Question_Data.csv", header = TRUE)

questionData <- questionData %>% mutate_all(na_if,"")

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Survey Questions Catalogue",
      tabPanel("Survey Questions",
          sidebarLayout(
              sidebarPanel(
                  width = "4",
                      selectInput("category","Select a category", choices = c("All", "Social", "Technological", "Economic", "Environmental", "Political"), selected = "All"),
                      selectInput("date","Select a date", choices = c("All", unique(sort(questions$Date))), selected = "All"),
                      selectInput("location", "Select a location", choices = unique(sort(questions$Location)), selected = "All"),
                      selectInput("quest","Select a question", choices = c(unique(questions$Question)), selected = "Question1")
              ),
                        
                      mainPanel(
                        width = "6",
                          # verbatimTextOutput("surveyNameText"),
                          h4(em(textOutput(outputId = "surveyNameText"))),
                          h4(em(textOutput(outputId = "locationText"))),
                          h4(em(textOutput(outputId = "dateText"))),
                          headerPanel(""),
                          DT::dataTableOutput("table")
                      )
              )
      )
  )
)

server <- function(input, output, session) {
  
  observe({
    
    filteredSelections <- questions
    if(input$category != "All"){
      filteredSelections <- filter(filteredSelections, Category == input$category)
    }
    
    if(input$date != "All"){
      filteredSelections <- filter(filteredSelections, Date == input$date)
    }
    
    if(input$location != "All of NZ"){
      filteredSelections <- filter(filteredSelections, Location == input$location)
    }
    
    updateSelectInput(session, "quest",
                      choices = unique(filteredSelections$Question)
    )
  })
  
  output$surveyNameText <- renderText({ paste0("Survey Name: ", questions$Survey_Name[questions$Question == input$quest]) })
  output$locationText <- renderText({ paste0("Location: ", questions$Location[questions$Question == input$quest]) })
  output$dateText <- renderText({ paste0("Date: ", questions$Date[questions$Question == input$quest]) })
  
  output$table = DT::renderDataTable({
    
    dataType <- questions$Data_Type[questions$Question == input$quest]
    
    theQ <- input$quest
    theQ <- str_replace_all(theQ, " ", ".")
    theQ <- str_replace_all(theQ, "\\?", ".")
    theQ <- str_replace_all(theQ, "\\,", ".")
    theQ <- str_replace_all(theQ, "\\:", ".")
    theQ <- str_replace_all(theQ, '\\"', ".")
    
    
    if(dataType == "Categorical"){
      tbl <- as.data.frame(table(questionData[, theQ]))
      colnames(tbl) <- c(input$quest , "Count")
      
    }
    
    if(dataType == "Ratio"){
      tbl <- as.data.frame(describe(questionData[, theQ]))
      # colnames(tbl) <- c(input$quest , "Count")
      tbl = subset(tbl, select = -c(skew, kurtosis, se, trimmed))
    }
    
    DT::datatable(tbl, rownames = FALSE, options = list(searching = FALSE, dom = 'f'))
  })

}

shinyApp(ui, server)

