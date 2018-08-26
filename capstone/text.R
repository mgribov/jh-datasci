library(tm)
library(ngram)
library(RWeka)
library(plyr)
library(RTextTools)
library(corpus)
library(data.table)
library(dplyr)
library(qdap)
library(stringi)

##############
# initial load and cleaning of corpus
# 5% of lines from each dataset
docs <- Corpus(DirSource('~/code/jh-datasci-projects/capstone/final/en_US/smaller'))

# standard cleaning
removeUnicode <- function(x) stri_replace_all_regex(x,"[^\x20-\x7E]","")
docs <- tm_map(docs, content_transformer(removeUnicode))

docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)

docs <- tm_map(docs, content_transformer(replace_contraction))

# source: http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
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
unigrams <- term_stats(docs, ngrams = 1:1)
bigrams <- term_stats(docs, ngrams = 2:2)
trigrams <- term_stats(docs, ngrams = 3:3)
quadgrams <- term_stats(docs, ngrams = 4:4)

save(unigrams, file="~/code/jh-datasci-projects/capstone/rdata/unigrams.Rdata")
save(bigrams, file="~/code/jh-datasci-projects/capstone/rdata/bigrams.Rdata")
save(trigrams, file="~/code/jh-datasci-projects/capstone/rdata/trigrams.Rdata")
save(quadgrams, file="~/code/jh-datasci-projects/capstone/rdata/quadgrams.Rdata")
#################################

# load saved data based on percentage of total corpus
load("~/code/jh-datasci-projects/capstone/rdata/unigrams.Rdata")
load("~/code/jh-datasci-projects/capstone/rdata/bigrams.Rdata")
load("~/code/jh-datasci-projects/capstone/rdata/trigrams.Rdata")
load("~/code/jh-datasci-projects/capstone/rdata/quadgrams.Rdata")
#################################

# create indexed data.table structures for each word/ngram
ngram_search_n2 <- as.data.table(bigrams[0:100000, ])
setkey(ngram_search_n2, term)

ngram_search_n3 <- as.data.table(trigrams[0:100000, ])
setkey(ngram_search_n3, term)

ngram_search_n4 <- as.data.table(quadgrams[0:100000, ])
setkey(ngram_search_n2, term)

save(ngram_search_n2, file="~/code/jh-datasci-projects/capstone/rdata/ngram_search_n2.Rdata")
save(ngram_search_n3, file="~/code/jh-datasci-projects/capstone/rdata/ngram_search_n3.Rdata")
save(ngram_search_n4, file="~/code/jh-datasci-projects/capstone/rdata/ngram_search_n4.Rdata")
###########################################

load("~/code/jh-datasci-projects/capstone/rdata/ngram_search_n2.Rdata")
load("~/code/jh-datasci-projects/capstone/rdata/ngram_search_n3.Rdata")
load("~/code/jh-datasci-projects/capstone/rdata/ngram_search_n4.Rdata")
############################################

# general matching strategy:
# for each chunk of up to 3 consequtive words:
# try to find quadgram match
# if false, remove first word, try to find trigram match
# if false, remove second word, try to find bigram match
# if false ?

# top 10 matches for bigrams starting with "one"
phrase <- "a"
w <- unlist(strsplit(phrase, " "))
comparePhrase <- paste("^", paste(w, collapse=" "), "\\b", sep="")

rawMatch <- grep(comparePhrase, ngram_search_n2$term)
top_n2 <- ngram_search_n2[rawMatch, ]
best_n2 <- arrange(top_n2, desc(count))
########################################################################



###############################
# most used words
docs_stopwords <- tm_map(docs, removeWords, stopwords('english'))
tdm <- TermDocumentMatrix(docs)
term_lst <- data.frame(word = tdm$dimnames$Terms, frequency = tdm$v)
term_freq <- plyr::arrange(term_lst, -frequency)
# plot(tdm, terms = findFreqTerms(tdm, lowfreq = 2)[1:50], corThreshold = 0.5)
