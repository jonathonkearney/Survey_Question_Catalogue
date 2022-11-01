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

surveys <- list.files(path = "C:/Users/OEM/OneDrive/Documents/R/Survey_Question_Catalogue/Survey_Data", pattern = '.csv$', full.names = T) %>%
  map(read_csv)

questions <- questions %>% mutate_all(na_if,"")

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
                      pickerInput("uniqueTags", "Select one or more Tags", sort(uniqueTags), options = list(`actions-box` = TRUE), multiple=TRUE),
                      selectInput("date","Select a date", choices = c("All", unique(sort(questions$Date))), selected = "All"),
                      selectInput("location", "Select a location", choices = unique(sort(questions$Location)), selected = "All"),
                      selectInput("quest","Select a question", choices = c(unique(questions$Question)), selected = "After how many years do you replace your phone?"),
                      h5(em(textOutput(outputId = "surveyNameText"))),
                      h5(em(textOutput(outputId = "locationText"))),
                      h5(em(textOutput(outputId = "sampleSizeText"))),
                      h5(em(textOutput(outputId = "dateText"))),
                      h5(em(textOutput(outputId = "dataTypeText"))),
                      h6(a(textOutput(outputId = "linkText"))),
                      headerPanel("")
              ),

                      mainPanel(
                        width = "6",
                          DT::dataTableOutput("table", width = "1000px"),
                          HTML("<hr>"),
                          plotOutput(outputId = "plot", width = "1000px", height = "500px")
                      )
              )
      )
  )
)

server <- function(input, output, session) {
  
  observe({
    
    filteredSelections <- questions
    
    if(!is.null(input$uniqueTags)){
      filteredSelections <- filter(filteredSelections, Tag1 %in% input$uniqueTags | Tag2 %in% input$uniqueTags
                                   | Tag3 %in% input$uniqueTags | Tag4 %in% input$uniqueTags)
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
  output$sampleSizeText <- renderText({
    DFLocation <- which(sapply(surveys, function(x) any(names(x) == input$quest)))
    survey <- as.data.frame(surveys[DFLocation], check.names=FALSE)
    paste0("Sample Size: ", length(survey[, input$quest][!is.na(survey[, input$quest])])) 
    
  })
  output$dataTypeText <- renderText({ paste0("Data Type: ", questions$Data_Type[questions$Question == input$quest]) })
  output$linkText <- renderText({ paste0("Survey Link: ", questions$Link[questions$Question == input$quest]) })
  
  output$table = DT::renderDataTable({
    
    dataType <- questions$Data_Type[questions$Question == input$quest]
    DFLocation <- which(sapply(surveys, function(x) any(names(x) == input$quest)))
    survey <- as.data.frame(surveys[DFLocation], check.names=FALSE)
    
    if(dataType == "Nominal"){
      tbl <- as.data.frame(table(survey[, input$quest]))
      colnames(tbl) <- c(input$quest , "Count")
    }
    
    if(dataType == "Ratio"){
      tbl <- as.data.frame(describe(survey[, input$quest]))
      tbl = subset(tbl, select = -c(skew, kurtosis, se, trimmed))
    }
    
    DT::datatable(tbl, rownames = FALSE, options = list(searching = FALSE, dom = 'p', pageLength = 10))
  })

  output$plot <- renderPlot({
    
    dataType <- questions$Data_Type[questions$Question == input$quest]
    DFLocation <- which(sapply(surveys, function(x) any(names(x) == input$quest)))
    survey <- as.data.frame(surveys[DFLocation], check.names=FALSE)
    
    if(dataType == "Nominal"){
      tbl <- as.data.frame(table(survey[, input$quest]))
      p <- ggplot(tbl, aes(Var1, Freq, fill = Var1)) +
        geom_bar(stat='identity') + labs(x = input$quest)
    }
    
    if(dataType == "Ratio"){
      p <- ggplot(survey, aes(x = survey[,input$quest])) +
        geom_histogram(binwidth=1, colour="black", fill="cadetblue2") + labs(x = input$quest)
    }
    
    p
  })
}

shinyApp(ui, server)

