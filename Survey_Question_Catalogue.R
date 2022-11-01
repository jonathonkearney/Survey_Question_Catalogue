library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
require(psych)  


# *********************** LOAD & CLEAN DATA ***************************
rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Survey_Question_Catalogue")

questions <- read.csv("Questions.csv", header = TRUE)
questionData <- read.csv("Question_Data.csv", header = TRUE, check.names=FALSE)

questions <- questions %>% mutate_all(na_if,"")
questionData <- questionData %>% mutate_all(na_if,"")

uniqueTags <- list(questions$Tag1, questions$Tag2, questions$Tag3, questions$Tag4)
uniqueTags <- Reduce(c,uniqueTags)
uniqueTags <- unique(uniqueTags)
uniqueTags <- uniqueTags[!is.na(uniqueTags)]

ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Survey Questions Catalogue",
      tabPanel("Survey Questions",
          sidebarLayout(
              sidebarPanel(
                  width = "3",
                      pickerInput("uniqueTags", "Select one or more uniqueTags", sort(uniqueTags), options = list(`actions-box` = TRUE), selected= uniqueTags, multiple=TRUE),
                      selectInput("date","Select a date", choices = c("All", unique(sort(questions$Date))), selected = "All"),
                      selectInput("location", "Select a location", choices = unique(sort(questions$Location)), selected = "All"),
                      selectInput("quest","Select a question", choices = c(unique(questions$Question)), selected = "After how many years do you replace your phone?")
              ),

                      mainPanel(
                        width = "6",
                          h5(em(textOutput(outputId = "surveyNameText"))),
                          h5(em(textOutput(outputId = "locationText"))),
                          h5(em(textOutput(outputId = "sampleSizeText"))),
                          h5(em(textOutput(outputId = "dateText"))),
                          h5(em(textOutput(outputId = "dataTypeText"))),
                          h6(a(textOutput(outputId = "linkText"))),
                          HTML("<hr>"),
                          # headerPanel(""),
                          DT::dataTableOutput("table", width = "1000px"),
                          HTML("<hr>"),
                          # headerPanel(""),
                          plotOutput(outputId = "plot", width = "1000px", height = "500px")
                      )
              )
      )
  )
)

server <- function(input, output, session) {
  
  observe({
    
    filteredSelections <- questions
    
    filteredSelections <- filter(filteredSelections, questions$Tag1 %in% input$uniqueTags)
    
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
  output$sampleSizeText <- renderText({ paste0("Sample Size: ", length(questionData[, input$quest][!is.na(questionData[, input$quest])])) })
  output$dataTypeText <- renderText({ paste0("Data Type: ", questions$Data_Type[questions$Question == input$quest]) })
  output$linkText <- renderText({ paste0("Survey Link: ", questions$Link[questions$Question == input$quest]) })
  
  output$table = DT::renderDataTable({
    
    dataType <- questions$Data_Type[questions$Question == input$quest]
    
    if(dataType == "Categorical"){
      tbl <- as.data.frame(table(questionData[, input$quest]))
      colnames(tbl) <- c(input$quest , "Count")
    }
    
    if(dataType == "Ratio"){
      tbl <- as.data.frame(describe(questionData[, input$quest]))
      tbl = subset(tbl, select = -c(skew, kurtosis, se, trimmed))
    }
    
    DT::datatable(tbl, rownames = FALSE, options = list(searching = FALSE, dom = 'f', pageLength = 25))
  })

  output$plot <- renderPlot({
    
    dataType <- questions$Data_Type[questions$Question == input$quest]
    
    if(dataType == "Categorical"){
      tbl <- as.data.frame(table(questionData[, input$quest]))
      p <- ggplot(tbl, aes(Var1, Freq, fill = Var1)) +
        geom_bar(stat='identity') + labs(x = input$quest)
    }
    
    if(dataType == "Ratio"){
      p <- ggplot(questionData, aes(x = questionData[,input$quest])) +
        geom_histogram(binwidth=1, colour="black", fill="cadetblue2") + labs(x = input$quest)
    }
    
    p
    
  })
}

shinyApp(ui, server)

