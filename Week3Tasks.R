

#'
Predict1Word.UsingNgram__Generator__ <- function(DFMs) {
    freqNgrams <- lapply(1:length(DFMs), function(N) {
        freq <- colSums(DFMs[[N]]) %>% 
            as.data.table(keep.rownames = "Ngram") %>%
            .[, `:=`(c("Ngram", paste0("Word", (N-1):0)), 
                     strsplit(Ngram, "_") %>%
                         lapply(function(s) as.list(c(paste(s[1:N][-N], collapse = "_"),
                                                      s[1:N]))) %>% 
                         rbindlist)
              ] %>%
            setnames(".", "count")
        
        for (n in 0:(N-1)) freq <- freq[get(paste0("Word", n)) != ""]
        
        freq[, `:=`(paste0("Word", (0:(N-1))[-1]), NULL)]
        
        setkey(freq, Ngram, Word0)
    })
    
    # freqNgrams.top <- lapply(freqNgrams, function(freq) {
    #     freq[, .SD[which.max(count)], by = Ngram]
    # })
    
    function(tkn, nextWords = NULL, return.counts = F) {
        Nmax <- min(length(tkn) + 1, length(freqNgrams))
        tkn <- tkn[length(tkn) - ((Nmax-2):0)]
        
        nextWord <- character(0)
        N <- Nmax
        while(length(nextWord) == 0) {
            Ngram_ <- paste(tkn, collapse = "_")
            freq <- freqNgrams[[N]][.(Ngram_, nextWords)]
            nextWord <- freq[which.max(count), Word0]
            N <- N - 1
            tkn <- tkn[-1]
        }
        
        if (return.counts) list(nextWord = nextWord, freq = freq) else nextWord
    }
}



##

DFMs <- list(DFM_noStopwords_1grams$blogs, 
             DFM_noStopwords_2grams$blogs, 
             DFM_noStopwords_3grams$blogs, 
             DFM_noStopwords_4grams$blogs)


Predict1Word.UsingNgram.noStopwords <- Predict1Word.UsingNgram__Generator__(DFMs)



#### ####
Sentences <- list(q1 = "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
                  q2 = "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
                  q3 = "I'd give anything to see arctic monkeys this",
                  q4 = "Talking to your mom has the same effect as a hug and helps reduce your",
                  q5 = "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
                  q6 = "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
                  q7 = "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
                  q8 = "Every inch of you is perfect from the bottom to the",
                  q9 = "I'm thankful my childhood was filled with imagination and bruises from playing",
                  q10 = "I like how the same people are in almost all of Adam Sandler's")

Choices <- list(q1 = c("give", "die", "eat", "sleep"),
                q2 = c("marital", "financial", "horticultural", "spiritual"),
                q3 = c("morning", "month", "weekend", "decade"),
                q4 = c("sleepiness", "stress", "happiness", "hunger"),
                q5 = c("minute", "picture", "walk", "look"),
                q6 = c("case", "account", "incident", "matter"),
                q7 = c("hand", "finger", "toe", "arm"),
                q8 = c("side", "top", "center", "middle"),
                q9 = c("inside", "daily", "weekly", "outside"),
                q10 = c("novels", "movies", "stories", "pictures"))

Correct <- c(q1="die", q2="marital", q3="weekend", q4="stress",
             q5="picture", q6 = "matter", q7="hand", q8="top",
             q9="outside", q10="movies")


#### ####
tkns <- sapply(Sentences, function(sent) {
    sent <- tokens(sent, what = "sentence")[[1]] %>% 
        .[length(.)] %>%
        tokens(remove_twitter = T, 
               remove_numbers = T, 
               remove_punct = T,
               remove_symbols = T) %>%
        .[[1]]
})


tkns_noStopwords <- sapply(Sentences, function(sent) {
    sent <- tokens(sent, what = "sentence")[[1]] %>% 
        .[length(.)] %>%
        tokens(remove_twitter = T, 
               remove_numbers = T, 
               remove_punct = T,
               remove_symbols = T) %>%
        tokens_remove(stopwords()) %>%
        .[[1]]
})

#### ####
Attempt1 <- lapply(names(tkns_noStopwords), function(q) 
    Predict1Word.UsingNgram.noStopwords(tkns_noStopwords[[q]], Choices[[q]], T))

# q1          q2          q3          q4          q5          q6 
# "die" "financial"   "morning"    "stress"   "picture"      "case" 
# q7          q8          q9         q10 
# "hand"      "side"   "outside"  "pictures" 

Score1 <- c(1,0,0, 1,1,0, 1,0,1, 0)

#### ####
Attempt2 <- lapply(names(tkns), function(q) 
    Predict1Word.UsingNgram(tkns[[q]], Choices[[q]], T))


#### ####
smoothing <- .01
Attempt3 <- mapply(function(freq1, freq2) {
    freq <- merge(freq1$freq[, .(Word0, count)][is.na(count), count := 0],
                  freq2$freq[, .(Word0, count)][is.na(count), count := 0], 
                  by = "Word0"
    )[, p.x := (count.x + smoothing) / sum(count.x + smoothing)][
        , p.y := (count.y + smoothing) / sum(count.y + smoothing)][
            , p := (p.x + p.y)/2]
    
    print(freq)
    
    freq[which.max(p), Word0]
},
                  Attempt1, Attempt2)
