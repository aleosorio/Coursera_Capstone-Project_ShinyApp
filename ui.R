#
# User-interface definition of the Capstone Project App.
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predictive Text App"),
  
  # Side bar with a text input for string to predict with 
  sidebarLayout(
    sidebarPanel(
       textInput(inputId = "string",
                   label = "Write down the text with which to predict the next word:",
                   value = "",
                   width = "100%")
    ),
    
    # Text output containing predicted word, from server.R
    mainPanel(
       textOutput(outputId = "wordpred")  ## continue from here!!
    )
  )
))
