#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# UI for application that
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Next-word Predictor a la SwiftKey"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # radioButtons(inputId = "sourceType", 
            #              label   = "Source type", 
            #              inline  = T, 
            #              choices = c("blogs", "news", "twitter", "all"), 
            #              selected = "all"),
            
            radioButtons(inputId = "numPreds", 
                         label   = "Prediction Output", 
                         inline  = T, 
                         choices = c("Best", "Top 3"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            HTML('Welcome to the Next-Word Predictor'),
            
            textOutput("loading"),
            
            textInput("sentence", NULL, placeholder = "Type here!"),
            
            textOutput("predictionOutputLabel"),
            
            HTML('<br>'),
            
            textOutput("nextWord"),
            
            HTML('<br><br>Analytics for top 3 word predictions:<br><br>'),
            
            DT::dataTableOutput("wordPredictionAnalytics", width = 200),
            
            actionButton("analytics", "Refresh analytics")
            
        )
    )
))
