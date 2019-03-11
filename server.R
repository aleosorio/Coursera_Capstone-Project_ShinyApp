#
# This is the server logic of of the Capstone Project App.
#

library(shiny)
library(tidyverse)
library(stringr)
set.seed(123)

## stringsurefinal: if unsuccessful on its first try, it conducts the next search
## after an n-gram reduction, whithin the pre-calculated n-grams dataset
## (ngramdata = wordpredgrams).  If still unsuccessful, it applies the original
## stringsure's search criteria within a sample of testdata, and so on.


stringsurefinal <- function(string, ngramdata, stringdata) {
        strlength <- str_count(string, boundary("word"))
        strsearch <- paste("(?<=(^|\\s))", string, "\\s\\w+", sep = "")
        strwhere <- str_which(stringdata, strsearch)
        strdisp <- length(strwhere)
        ngramwhere <- integer(0)
        while(strdisp == 0 & strlength > 1) {
                string <- word(string, 2, -1)
                strlength <- str_count(string, boundary("word"))
                ngramwhere <- which(string == ngramdata$string)
                if(length(ngramwhere) > 0) {
                        ngramresult <- ngramdata$modes[ngramwhere]
                        strdisp <- ngramwhere
                } else {
                        strsearch <- paste("(?<=(^|\\s))", string, "\\s\\w+", sep = "")
                        strwhere <- str_which(stringdata, strsearch)
                        strdisp <- length(strwhere)
                }
        }
        if(strdisp > 0) {
                if(length(ngramwhere) >0) {
                        result <- ngramresult
                } else {
                        result <- stringset(string , stringdata[strwhere]) %>%
                                filter(., freq == max(freq)) %>%
                                sample_n(., 1) %>%
                                select(., string) %>%
                                word(., -1)
                }
        } else {
                result <- "NOT ENOUGH DATA AVAILABLE TO PREDICT. TRY AGAIN WITH A DIFFERENT STRING"
        }
        return(result)
}

## wordpredfinal: Function that returns the predicted word, with an initial search
## within thepre-calculated popular n-grams dataset (wordpredgrams).  It starts by
## carrying on the input string's format check (converting the input string into the
## same wordpredfinal$string format) and reducing its ngram down to "ngramlength"
## (5 ended up being the number, beyond which very few succesfull searches were
## obtained) . If there's no direct match within the pre-calculated n-grams
## dataset, all additional predictions are conducted through the stringsurefinal
## function, within a testdata sample (defined by the "precision" parameter).

wordpredfinal <- function(string, precision, ngramdata, stringdata) {
        ngramlength <- 5
        strsearch <- str_to_lower(string) %>%
                str_extract_all(., boundary("word")) %>%
                unlist(., use.names = FALSE) %>%
                str_flatten(., collapse = " ") %>%
                if_else(str_count(., boundary("word")) > ngramlength, word(., str_count(., boundary("word")) - ngramlength + 1, -1), .)
        strlength <- str_count(strsearch, boundary("word"))
        ngramwhere <- which(strsearch == ngramdata$string)
        if(length(ngramwhere) > 0) {
                result <- ngramdata$modes[ngramwhere]
        } else {
                samplength <- ceiling(length(testdata) * precision)
                stringdata <- sample(stringdata, samplength)
                result <- stringsurefinal(strsearch, ngramdata, stringdata)
        }
        return(result)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$wordpred <- textOutput({
          
          wordpredfinal(string = input$string, precision = 0.075, ngramdata = wordpredgrams, stringdata = testdata)
          
  })
  
})
