library(magrittr)
library(data.table)

library(knitr)
library(ggplot2)

library(Matrix)
library(quanteda)

setwd("C:/Users/lchia/Coursera/DataScienceSpecialization/Course10")

## The raw corpora, loaded from ./Coursera-SwiftKey/final/en_US/ ####
load("MilestoneReport_cache/corpora_raw.Rdata", verbose = T)


## Chunk the corpora, tokenize into sentences and convert into corpus ####
nChunks <- 10

sent_corpora_chunked <- lapply(corpora_raw, function(cps) {
    breaks <- floor(seq(0, length(cps), length.out = nChunks + 1))
    lapply(1:nChunks, function(chunk) {
        # Tokenize the chunk into sentences
        sent <- tokenize(cps[(breaks[chunk]+1):breaks[chunk+1]], 
                         what = "sentence")
        docvars <- data.table(DocumentSID = rep(seq_along(sent), 
                                                times = sapply(sent, length)))
        # Each sentence is tagged with the document it originated from    
        corpus(unlist(sent), docvars = docvars)
    })
})
timestamp()

save(sent_corpora_chunked, file = "data/sent_corpora_chunked.Rdata")
rm(corpora_raw); gc()


## Convert sent_corpora_chunked into word tokens. Remove non-word characters ####
sent_tokens <- lapply(sent_corpora_chunked, function(cps) 
    lapply(cps, function(cps_chunk) tokens(cps_chunk, what = "word", 
                                           remove_twitter = T,
                                           remove_numbers = T,
                                           remove_punct = T,
                                           remove_symbols = T)))
timestamp()
save(sent_tokens, file = "data/sent_tokens.Rdata")
rm(sent_corpora_chunked); gc()


## ####
#' Helper function to compute Ngrams
#' @param InputCorpus Supply either a corpus or tokens. Cannot both be NULL.
#' @param InputTokens Supply either a corpus or tokens. Cannot both be NULL.
#' @param N Arguments for tokens_ngrams function
#' @param skip Arguments for tokens_ngrams function
#' @param remove A character vector of user-supplied features to ignore, such as
#'               "stop words", passed to the dfm function
#' @param ... Arguments to be passed to the tokens function. This could include
#'            remove_numbers, remove_punct, remove_symbols, remove_twitter, etc.
getDFM_Ngrams <- function(InputCorpus = NULL, N = 1, skip = 0L, 
                          InputTokens = NULL, remove = NULL, ...) {
    if (is.null(InputTokens)) InputTokens <- tokens(InputCorpus, ...) 
    if (!is.null(remove)) InputTokens <- tokens_remove(InputTokens, remove) 
    
    tokens_ngrams(InputTokens, N, skip) %>% 
        dfm(verbose = FALSE, ...)
}


## Ngram frequencies ####
freq_1grams <- lapply(sent_tokens, function(tkn) 
    lapply(tkn, function(tkn_chunk) {
        getDFM_Ngrams(InputTokens = tkn_chunk, N = 1) %>%
            colSums %>% 
            as.data.table(keep.rownames = "Ngram")
    }))
timestamp()
save(freq_1grams, file = "data/freq_1grams.Rdata")


freq_2grams <- lapply(sent_tokens, function(tkn) 
    lapply(tkn, function(tkn_chunk) {
        getDFM_Ngrams(InputTokens = tkn_chunk, N = 2) %>%
            colSums %>% 
            as.data.table(keep.rownames = "Ngram")
    }))
timestamp()
save(freq_2grams, file = "data/freq_2grams.Rdata")


freq_3grams <- lapply(sent_tokens, function(tkn) 
    lapply(tkn, function(tkn_chunk) {
        getDFM_Ngrams(InputTokens = tkn_chunk, N = 3) %>%
            colSums %>% 
            as.data.table(keep.rownames = "Ngram")
    }))
timestamp()
save(freq_3grams, file = "data/freq_3grams.Rdata")
rm(freq_3grams)


freq_4grams <- lapply(sent_tokens, function(tkn) 
    lapply(tkn, function(tkn_chunk) {
        getDFM_Ngrams(InputTokens = tkn_chunk, N = 4) %>%
            colSums %>% 
            as.data.table(keep.rownames = "Ngram")
    }))
timestamp()
save(freq_4grams, file = "data/freq_4grams.Rdata")
rm(freq_4grams)


freq_5grams <- lapply(sent_tokens, function(tkn) 
    lapply(tkn, function(tkn_chunk) {
        getDFM_Ngrams(InputTokens = tkn_chunk, N = 5) %>%
            colSums %>% 
            as.data.table(keep.rownames = "Ngram")
    }))
timestamp()
save(freq_5grams, file = "data/freq_5grams.Rdata")
rm(freq_5grams)


## Combine chunked frequencies ####

lapply(list.files("data", pattern = "freq_[1-5]grams\\.Rdata", full.name = T)[1:2],
       function(fname) {
           cat("\n", fname, ":")
           N <- strsplit(fname, "_|gram")[[1]][2] %>% as.integer
           cat("N =", N, "\n")
           
           assign("freq_Ngram", get(load(fname)))
           
           lapply(names(freq_Ngram), function(type) {
               cat(type)
               
               freq_Ngram[[type]] <<- 
                   lapply(freq_Ngram[[type]], function(freq) {
                       cat(".")
                       freq[, `:=`(c("Ngram", "Word0", "drop"), 
                                   strsplit(Ngram, "_") %>%
                                       lapply(function(s) 
                                           as.list(c(paste(s[1:N][-N], collapse="_"),
                                                     s[N],
                                                     any(s[1:N] == ""))) ) %>% 
                                       rbindlist)]
                       gc()
                       freq[drop == FALSE][, drop := NULL]
                   }) %>%
                   rbindlist %>%
                   setkey(Ngram, Word0) %>%
                   .[, .(frequency_count = sum(.)), by = .(Ngram, Word0)]
               
               gc()
               NULL
           })
           
           save(freq_Ngram, file = sub("grams", "grams_combined", fname))
           
           NULL
       }
) %>% invisible
timestamp()


## Parse concatenated Ngram, find top 3 predictions ####
nTopPred <- 3


lapply(list.files("data", pattern = "freq.+combine", full.names = T)[4],
       function(fname) {
           cat("\n", fname, ":")
           
           assign("freq_Ngram", get(load(fname)))
           
           N <- strsplit(fname, "_|gram")[[1]][2] %>% as.integer
           
           lapply(names(freq_Ngram), function(Type) {
               cat(".")
               
               freq <- freq_Ngram[[Type]]
               
               count <- freq[, .(frequency_count = sum(frequency_count)), 
                             by = Ngram] %>%
                   setkey(frequency_count) %>%
                   .[, cumulative_proportion := 
                         cumsum(frequency_count)/sum(frequency_count)] 
               
               cutoff <- round(1e-6 * nrow(count))
               
               freq <- freq[.(count[frequency_count >= cutoff, Ngram])] %>%
                   setkey(Ngram, Word0)
               
               predTop <- freq[, .SD[order(frequency_count, decreasing = T), 
                          .(Word0 = Word0[1:min(.N,nTopPred)], 
                            frequency_count[1:min(.N,nTopPred)],
                            prob = frequency_count[1:min(.N,nTopPred)] /
                                sum(frequency_count))],
                    by = Ngram]
               
               write.csv(predTop, 
                         file = sprintf("data/predTop_%igrams_%s.csv", N, Type),
                         row.names = F, quote = F)
               
               NULL
           })
           
           gc()
           
           NULL
       }
) %>% invisible
timestamp()







## All 3 copora ####
lapply(list.files("data", pattern = "freq.+combine", full.names = T)[c(1,3)],
       function(fname) {
           cat("\n", fname, ":")
           
           assign("freq_Ngram", get(load(fname)))
           
           N <- strsplit(fname, "_|gram")[[1]][2] %>% as.integer
           
           freq_Ngram <- rbindlist(freq_Ngram) %>%
               setkey(Ngram, Word0) %>%
               .[, .(frequency_count = sum(frequency_count)), by = .(Ngram, Word0)]
           
           count <- freq_Ngram[, .(frequency_count = sum(frequency_count)), 
                               by = Ngram] %>%
               setkey(frequency_count) %>%
               .[, cumulative_proportion := cumsum(frequency_count)/sum(frequency_count)] 
           
           if ((cutoff <- round(1e-6 * nrow(count))) > 1)
               freq_Ngram <- freq_Ngram[.(count[frequency_count >= cutoff, Ngram])] %>%
               setkey(Ngram, Word0)
           
           predTop <- freq_Ngram[, .SD[order(frequency_count, decreasing = T), 
                                       .(Word0 = Word0[1:min(.N,nTopPred)], 
                                         frequency_count[1:min(.N,nTopPred)],
                                         prob = frequency_count[1:min(.N,nTopPred)] /
                                             sum(frequency_count))],
                                 by = Ngram]
           
           write.csv(predTop, 
                     file = sprintf("data/predTop_%igrams_%s.csv", N, "all3corpora"),
                     row.names = F, quote = F)
           
       }) %>% invisible
timestamp()



##

lapply(list.files("data", pattern = "predTop_[1-3]grams_[abnt]", full.names = T)[-(1:4)],
       function(fname) {
           predTop <- read.csv(fname, stringsAsFactors = F) %>% as.data.table
           predTop[, V2 := NULL][, prob := signif(as.numeric(prob), digits = 5)]
           write.csv(predTop, file = fname, row.names = F, quote = F)
       })
