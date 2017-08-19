
Sentence <- list(q1 = c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"),
                 q2 = c("You're the reason why I smile everyday. Can you follow me please? It would mean the"),
                 q3 = c("Hey sunshine, can you follow me and make me the"),
                 q4 = c("Very early observations on the Bills game: Offense still struggling but the"),
                 q5 = c("Go on a romantic date at the"),
                 q6 = c("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"),
                 q7 = c("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"),
                 q8 = c("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"),
                 q9 = c("Be grateful for the good times and keep the faith during the"),
                 q10 = c("If this isn't the cutest thing you've ever seen, then you must be"))
Sentence <- lapply(Sentence, function(sent) tokens(sent, remove_punct = T)[[1]])


Choices <- list(q1 = c("beer", "cheese", "pretzels", "soda"),
                q2 = c("best", "most", "universe", "world"),
                q3 = c("bluest", "happiest", "saddest", "smelliest"),
                q4 = c("crowd", "defense", "players", "referees"),
                q5 = c("beach", "grocery", "mall", "movies"),
                q6 = c("horse", "motorcycle", "phone", "way"),
                q7 = c("time", "thing", "weeks", "years"),
                q8 = c("ears", "eyes", "fingers", "toes"),
                q9 = c("bad", "hard", "sad", "worse"),
                q10 = c("asleep", "callous", "insane", "insensitive"))

#### Predictor Wrapper Function ####

Predict1Word <- function(sent, choices) {
    n <- length(sent)
    
    pred <- Predict1Word.Using3Word(sent[n-2], sent[n-1], sent[n], choices)
    
    if (length(pred)) return(pred)
    
    pred <- Predict1Word.Using2Word(sent[n-1], sent[n], choices)
    
    if (length(pred)) return(pred)
    
    Predict1Word.Using1Word(sent[n], choices)
}


#### ####
Attempt2 <- mapply(Predict1Word, Sentence, Choices)
Attempt2 <- c(q1 = "beer", q2 = "world", q3 = "saddest", q4 = "defense", 
              q5 = "grocery", q6 = "way", q7 = "time", q8 = "ears", q9 = "hard",
              q10 = "insane")

Score2 <- c(1,1,0, 1,0,1, 1,0,0, 1)

#### ####

## Hack: choose next best prediction, if the original prediction is wrong
Choices1 <- lapply(seq_along(Choices), function(q) {
    if (Score2[q] == 1) Choices[[q]] else setdiff(Choices[[q]], Attempt2[q])
})
names(Choices1) <- names(Choices)

Attempt3 <- mapply(Predict1Word, Sentence, Choices1)

Score3 <- c(1,1,1, 1,1,1, 1,0,1, 1)