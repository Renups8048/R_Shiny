library(shiny)
library(shinydashboard)
library(pdfetch)
library(datasets)
library(distr)
library(dplyr)
library(MASS)

# x <-library(help ="datasets")[["info"]][[2]]
shinyUI(pageWithSidebar(
  headerPanel("Hypothesis Testing"),
  sidebarPanel(

    selectInput("ds", "Data Source :",
                c("In-Built" = "ib",
                  "File" = "file",
                  "URL" = "url"
                  
                  #,
                  # "Yahoo Finance" = "yf"
                )),
    
    
    
    conditionalPanel(
      condition = "input.ds == 'url'",
      textInput("urldat", label = "Input URL", value = "http://samplecsvs.s3.amazonaws.com/Sacramentorealestatetransactions.csv")
    ),
    
    
    #Input: Select a file
    
    conditionalPanel(
      condition = "input.ds == 'file'",
      fileInput("datafile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/coma-separated-values.text/plain",
                           ".csv"))
    ),
    
    conditionalPanel(
      condition = "input.ds == 'ib'",
      selectInput(inputId = "inbuilt", label = "Select a Dataset", choices = library(help ="datasets")[["info"]][[2]])
    ),
    
    uiOutput("columns"),
    
    selectInput("type","Enter Type of Hypothesis",
                c("One Sample t-test" =  "t",
                  "Two Sample t-test" = "tt",
                  "Goodness of fit test" = "gt",
                  "Test of Independence" = "it")),
    conditionalPanel(
      condition = "input.type == 't'",
    numericInput("mu", "Enter mu value:", 5, min = 1, max = 100)),
    
    conditionalPanel(
      condition = "input.type == 'tt'",
      fileInput("datafile1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/coma-separated-values.text/plain",
                           ".csv")),
      uiOutput("columns1")
    ),
    conditionalPanel(
      condition = "input.type == 'it'",
      
      uiOutput("columns2")
    ),
    
    radioButtons("alpha", "Significance Level :",
                 c("0.01" = 0.01,
                   "0.05" = 0.05,
                   "0.1" = 0.1)),
    
    h3("Alternate Hypothesis"),
    checkboxInput("greater","Greater", value = TRUE),
    checkboxInput("less","Less")
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Dataset",tableOutput('table')),
                tabPanel("Results",
                         # textOutput('hh'),
                         verbatimTextOutput('summ'))
    
    )
  )
))