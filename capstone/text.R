library(tm)
library(ngram)
library(RWeka)
library(plyr)
library(RTextTools)
library(corpus)
library(data.table)
library(dplyr)
library(qdap)

##############
# initial load and cleaning of corpus
# 5% of lines from each dataset
docs <- Corpus(DirSource('~/code/jh-datasci-projects/capstone/final/en_US/smaller'))

# standard cleaning
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)

docs <- tm_map(docs, replace_contraction)

profanity = readLines('~/code/jh-datasci-projects/capstone/bad-words.txt')
docs <- tm_map(docs, removeWords, profanity)

# save clean corpus
writeCorpus(docs, path="~/code/jh-datasci-projects/capstone/corpus")
############

# load corpus and do basic stats/plots
docs <- Corpus(DirSource('~/code/jh-datasci-projects/capstone/corpus'))

# all word stats
most_common <- term_stats(docs)

# 144595 - full vocab, includes chinese chars, emoji, etc
all_unique_words <- dim(most_common)[1]
all_words <- sum(most_common$count)

# will build trees for 1000 most common terms
most_common_words <- most_common[nchar(most_common$term) >= 3, ]
most_common_words <- most_common_words[0:1000, ]  
##################################################

# build ngrams, only one that works
bigrams <- term_stats(docs, ngrams = 2:2)
trigrams <- term_stats(docs, ngrams = 3:3)
quadgrams <- term_stats(docs, ngrams = 4:4)

save(bigrams, file="~/code/jh-datasci-projects/capstone/bigrams.Rdata")
save(trigrams, file="~/code/jh-datasci-projects/capstone/trigrams.Rdata")
save(quadgrams, file="~/code/jh-datasci-projects/capstone/quadgrams.Rdata")
#################################

# load saved data based on percentage of total corpus
load("~/code/jh-datasci-projects/capstone/bigrams.Rdata")
load("~/code/jh-datasci-projects/capstone/trigrams.Rdata")
load("~/code/jh-datasci-projects/capstone/quadgrams.Rdata")
#################################

# create indexed data.table structures for each word/ngram
ngram_search_n2 <- as.data.table(bigrams[0:100000, ])
setkey(ngram_search_n2, term)

ngram_search_n3 <- as.data.table(trigrams[0:100000, ])
setkey(ngram_search_n3, term)

ngram_search_n4 <- as.data.table(quadgrams[0:100000, ])
setkey(ngram_search_n2, term)

save(ngram_search_n2, file="~/code/jh-datasci-projects/capstone/ngram_search_n2.Rdata")
save(ngram_search_n3, file="~/code/jh-datasci-projects/capstone/ngram_search_n3.Rdata")
save(ngram_search_n4, file="~/code/jh-datasci-projects/capstone/ngram_search_n4.Rdata")
###########################################

load("~/code/jh-datasci-projects/capstone/ngram_search_n2.Rdata")
load("~/code/jh-datasci-projects/capstone/ngram_search_n3.Rdata")
load("~/code/jh-datasci-projects/capstone/ngram_search_n4.Rdata")
############################################

# general matching strategy:
# for each chunk of up to 3 consequtive words:
# try to find quadgram match
# if false, remove first word, try to find trigram match
# if false, remove second word, try to find bigram match
# if false ?

# top 10 matches for bigrams starting with "one"
comparePhrase <- "^day "
rawMatch <- grep(comparePhrase, ngram_search_n2$term)[0:10]
top_n2 <- ngram_search_n2[rawMatch, ][order(count, decreasing=TRUE)]

# top 10 trigram matches for best match above
comparePhrase2 <- paste("^", top_n2[1:1, term], sep="")
rawMatch <- grep(comparePhrase2, ngram_search_n3$term)[0:10]
top_n3 <- ngram_search_n3[rawMatch, ][order(count, decreasing=TRUE)]

# top 10 quadgram matches for anything above
comparePhrase3 <- paste("^", top_n3[1:1, term], sep="")
rawMatch <- grep(comparePhrase3, ngram_search_n4$term)[0:10]
top_n4 <- ngram_search_n4[rawMatch, ][order(count, decreasing=TRUE)]
########################################################################

predictWord <- function(w) {
  # how big is this string, will work with max of 3 words
  n_words = length(w)
  if (n_words == 0) {
    return()
  }
  
  # use appropriate ngram dict based on number of words passed
  if (n_words > 3) {
    # use last 3 words
    return(predictWord(tail(w, 3)))
    
  } else if (n_words == 3) {
    # search quadgrams
    ngram_search <- ngram_search_n4
    
  } else if (n_words == 2) {
    # search trigrams
    ngram_search <- ngram_search_n3
    
  } else if (n_words == 1) {
    # search bigrams
    ngram_search <- ngram_search_n2
  }

  # matching is by regex of the phrase ending in these words
  comparePhrase <- paste("^", paste(w, collapse=" "), sep="")
  
  rawMatch <- grep(comparePhrase, ngram_search$term)[0:1]
  top <- ngram_search[rawMatch, ][order(count, decreasing=TRUE)]

  # if no match, then shorten the string by 1 word removing the first one, and try agan
  if (is.na(top$term[0:1])) {
    return(predictWord(tail(w, n_words - 1)))
  }
  
  # return best match
  match <- unlist(strsplit(top$term[0:1], " "))
  return(tail(match, 1))
}

testPhrase <- "what the "
w <- unlist(strsplit(testPhrase, " "))
pred <- predictWord(w)
pred




###############################
# most used words
docs_stopwords <- tm_map(docs, removeWords, stopwords('english'))
tdm <- TermDocumentMatrix(docs)
term_lst <- data.frame(word = tdm$dimnames$Terms, frequency = tdm$v)
term_freq <- plyr::arrange(term_lst, -frequency)
# plot(tdm, terms = findFreqTerms(tdm, lowfreq = 2)[1:50], corThreshold = 0.5)
