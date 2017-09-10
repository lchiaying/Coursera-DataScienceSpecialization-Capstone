#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(magrittr)
library(data.table)
library(DT)
# library(aws.s3)

#### LOAD DATA ####
s3BucketName <- "coursera-datasciencespecialization-capstone"
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAJABVG77S7NBLH3HA",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "us-east-2")



#### SERVER ####
shinyServer(function(input, output) {
    
    #### COMMON VARIABLES ####
    
    predictor <- data.table()
    predTop <- NULL
    
    
    #### HELPER FUNCTIONS ####
    #'
    parseSentence <- function(sent) {
        sent <- tokens(sent, what = "sentence")[[1]] %>% 
            .[length(.)] %>%
            tokens(remove_twitter = T, 
                   remove_numbers = T, 
                   remove_punct = T,
                   remove_symbols = T) %>%
            .[[1]]
    }
    
    #'
    PredictNextWord <- function(inputWords) {
        if (length(inputWords) == 0)
            return(predTop$N1[[sourceType]])
        
        N <- min(length(inputWords) + 1, length(predTop)) # Ngram length
        inputWords <- inputWords[length(inputWords) - ((N - 2):0)]
        
        predictor_ <- NULL
        while(N > 0 & is.null(predictor_$Word0)) {
            Ngram_ <- paste(inputWords, collapse = "_")
            predictor_ <- predTop[[N]][[sourceType]][.(Ngram_)]
            N <- N - 1
            inputWords <- inputWords[-1]
        }
        
        predictor_
    }
    
    
    
    #### OUTPUTS ####
    
    ## Loading
    output$loading <- renderText({
        predTop_file_names <- list(N1 = list(all = "predTop_1grams_all3corpora.csv"),
                                   N2 = list(all = "predTop_2grams_all3corpora.csv"),
                                   N3 = list(all = "predTop_3grams_all3corpora.csv"))
        
        withProgress(message = 'Loading prediction data', value = 0, {
            dt <- lapply(seq_along(predTop_file_names), function(i) {
                incProgress(1/length(predTop_file_names), 
                            detail = sprintf("loading part %i of %i", 
                                             i, length(predTop_file_names)))
                
                file_names <- predTop_file_names[[i]]
                
                lapply(file_names, function(file_name) {
                    readBin(aws.s3::get_object(file_name, s3BucketName), "character") %>%
                        read.csv(text = ., stringsAsFactors = FALSE)  %>% 
                        as.data.table %>%
                        .[, prob := as.numeric(prob)] %>%
                        setkey(Ngram)
                })
            })
            names(dt) <- names(predTop_file_names)
            predTop <<- dt
        })
        
        "Start typing in an English sentence:\n
(Add a space after the last word to predict the next word)"
    })
    
    
    ## nextWord
    output$nextWord <- renderText({
        sourceType <- ifelse(is.null(input$sourceType), "all", input$sourceType)
        
        if (nchar(input$sentence) == 0) {
            predictor <<- PredictNextWord(character(0))
        } else {
            sent_tokens <- parseSentence(input$sentence)
            
            partialLastWord <- substr(input$sentence, 
                                      nchar(input$sentence), 
                                      nchar(input$sentence) + 1) != " "
            
            ## 
            nInputWords <- length(sent_tokens)
            
            ##
            lastWord <- ifelse(partialLastWord, 
                               ifelse(nInputWords == 1, "", 
                                      sent_tokens[nInputWords-1]),
                               sent_tokens[nInputWords])
            
            ##
            predictor <<- PredictNextWord(char_tolower(lastWord))
            
        }
        
        nextWord <- ifelse(input$numPreds == "Best",
                           predictor[which.max(prob), Word0],
                           predictor[, paste(Word0, collapse = ", ")])
    })
    
    ## predictionOutputLabel
    output$predictionOutputLabel <- renderText({
        sourceType <- ifelse(is.null(input$sourceType), "all", input$sourceType)
        
        ifelse(input$numPreds == "Best", 
               sprintf("Best next-word prediction %s: ", sourceType),
               sprintf("Top 3 predictions: "))
    })
    
    
    ## wordPredictionAnalytics
    output$wordPredictionAnalytics <- DT::renderDataTable({
        input$analytics
        predictor[, .(Ngram = strsplit(Ngram, "_") %>% 
                          sapply(function(s) paste(s, collapse = " ")),
                      `Next Word` = Word0,
                      Probability = round(prob, digits = 3))]
    })
    
})
