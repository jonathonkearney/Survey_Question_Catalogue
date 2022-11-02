library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
require(psych)


# *********************** LOAD & CLEAN DATA ***************************
# rm(list = ls())

setwd("C:/Users/OEM/OneDrive/Documents/R/Survey_Question_Catalogue")

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
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .table, hr {
        margin-left: 24px;
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
                      selectInput("quest","Select a question", choices = c(unique(questions$Question))),
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
                          h2(textOutput(outputId = "questionNameText")),
                          HTML("<hr>"),
                          # DT::dataTableOutput("table", width = "1000px"),
                          tableOutput("table"),
                          HTML("<hr>"),
                          plotOutput(outputId = "plot", width = "1000px", height = "500px")
                      )
              )
      )
  )
)

server <- function(input, output, session) {
  
  dataType <- "Unknown"
  
  observe({
    dataType <- questions$Data_Type[questions$Question == input$quest]
  })
  
  # *********************** DYNAMIC INPUTS ***************************
  
  observe({
    
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
    
    updateSelectInput(session, "quest",
                      choices = unique(filteredSelections$Question)
    )
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
  output$linkText <- renderText({ paste0("Survey Link: ", questions$Link[questions$Question == input$quest]) })
  
  output$questionNameText <- renderText({ questions$Question[questions$Question == input$quest] })

  # *********************** DATA TYPES ***************************
  
  observe({
    dataType <- questions$Data_Type[questions$Question == input$quest]
    DFLocation <- which(sapply(surveys, function(x) any(names(x) == input$quest)))
    survey <- as.data.frame(surveys[DFLocation], check.names=FALSE)
    
    if(dataType == "Nominal"){
      tbl <- as.data.frame(table(survey[, input$quest]))
      colnames(tbl) <- c(input$quest , "Count")
      
      
      output$table <- renderTable({
        tbl
      })
      
      
      output$plot <- renderPlot({
        ggplot(tbl, aes(tbl[, input$quest], Count , fill = tbl[, input$quest])) +
          geom_bar(stat='identity') + labs(x = input$quest, fill = input$quest)
      })
    }
    
    if(dataType == "Ordinal"){
      
    }
    
    if(dataType == "Interval"){
      
    }
    
    if(dataType == "Ratio"){
      
      
      tbl <- as.data.frame(describe(survey[, input$quest]))
      tbl = subset(tbl, select = -c(vars, skew, kurtosis, se, trimmed))
      tbl <- gather(tbl, Statistic, Value, n:range)
      
      output$table <- renderTable({
        tbl
      })
      # output$plot <- renderPlot({
      #   ggplot(survey, aes(x = survey[,input$quest])) +
      #     geom_density(color = 4,
      #                  fill = 4,
      #                  alpha = 0.25,
      #                  adjust = 0.005)
      # })
      
      output$plot <- renderPlot({
        ggplot(survey, aes(x = survey[,input$quest])) +
          geom_histogram(colour="black", fill="cadetblue2", na.rm = TRUE) + labs(x = input$quest)
      })
    }
  })
}

shinyApp(ui, server)

