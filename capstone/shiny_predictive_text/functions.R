load("~/code/jh-datasci-projects/capstone/shiny_predictive_text/rdata/unigrams.Rdata")
load("~/code/jh-datasci-projects/capstone/shiny_predictive_text/rdata/ngram_search_n2.Rdata")
load("~/code/jh-datasci-projects/capstone/shiny_predictive_text/rdata/ngram_search_n3.Rdata")
load("~/code/jh-datasci-projects/capstone/shiny_predictive_text/rdata/ngram_search_n4.Rdata")

make_search_query <- function(w) {
  return(paste("^", paste(w, collapse=" "), "\\b", sep=""))
}

most_common_word <- function(min_pred) {
  top <- arrange(unigrams, desc(count))
  return(top[0:min_pred, ])
}

# look for the best n-gram (n_words + 1) match for this phrase
top_match <- function(w, n_words) {
  search_query = make_search_query(w)
  
  ngram_search <- ngram_search_n2
  
  if (n_words == 3) {
    # search quadgrams
    ngram_search <- ngram_search_n4
    
  } else if (n_words == 2) {
    # search trigrams
    ngram_search <- ngram_search_n3
  }
  
  m <- grep(search_query, ngram_search$term)
  top <- ngram_search[m, ]
  top <- arrange(top, desc(count))
  
  return(top)
}

# make sure the top list is of proper length
# if not, add entries from lower order ngrams
# @todo this is the expensive function?
ensure_filled <- function(top, w, min_pred) {
  n_words = length(w)
  total_matches <- dim(top)[1]
  n_reduce <- 1

  while (total_matches < min_pred && n_reduce < n_words) {
    red <- n_words - n_reduce 
    nxt <- top_match(tail(w, red), red)
    nxt_matches <- dim(nxt)[1]
    
    if (nxt_matches > 0) {
      top <- rbind(top, nxt)
    }

    if (nxt_matches < min_pred - total_matches) {
      n_reduce <- n_reduce + 1
    }
    
    total_matches <- total_matches + nxt_matches;
  }
  
  if (total_matches < min_pred) {
    top <- rbind(top, most_common_word(min_pred - total_matches))
  }
  
  return(top)
}

# format the sorted best match list to only be the predicted words
format_results <- function(top) {
  best <- NULL
  
  # break up the phrase and return last word
  for (i in 1:length(top$term)) {
    phrase <- unlist(strsplit(as.character(top[i, 'term']), " "))
    last_word <- tail(phrase, 1)
    
    # make sure the words are unique
    if (!last_word %in% best) {
      best <- c(best, last_word)
    }
  }
  
  return(best)  
}

# take a list of top ngrams matched with regex and re-sort it according to probs
# return the list of 1-gram words as sugestion for next word
best_match <- function(top) {
  seen <- NULL
  best <- data.table(term = character(), count = double())

  # top 100 should be enough
  num_terms <- ifelse(length(top$term) > 100, 100, length(top$term))
  
  # boost suggested word based on all n-grams ending with it
  for (i in 1:num_terms) {
    phrase <- unlist(strsplit(as.character(top[i, 'term']), " "))
    last_word <- tail(phrase, 1)

    # add the counts together for words which appears in other order grams
    # @todo for now, just multiply by n in ngram as weight
    if (!last_word %in% seen) {
      seen <- c(seen, last_word)
      cnt <- top[i, 'count'] * length(phrase)
      best <- rbind(best, data.table(term=last_word, count=cnt$count), fill=TRUE) 
      
    } else {
      cnt <- best[best$term == last_word, 'count'] + (top[i, 'count'] * length(phrase))
      best[best$term == last_word, 'count'] <- cnt$count   
    }
  }
  
  best <- arrange(best, desc(count))

  # @todo just format current set for now
  return(format_results(best))
}

# return best next match given current ngrams data
predict_word <- function(phrase, min_pred = 5) {
  w <- unlist(strsplit(phrase, " "))
  
  # how big is this string, will work with max of 3 words
  n_words = length(w)
  if (n_words == 0) {
    # return most common word
    # @todo just freq count, or log prob?
    return(format_results(most_common_word(min_pred)))
  }
  
  if (n_words > 3) {
    # use last 3 words
    return(predict_word(tail(w, 3)))
  }
  
  # get n-gram matches for this phrase sorted by their freq
  top <- top_match(w, n_words)
  
  # if no match, then shorten the string by 1 word removing the first one, and try agan
  if (is.na(top$term[0:1])) {
    return(predict_word(tail(w, n_words - 1), min_pred))
  }
  
  # make sure we have the requested number of returned words
  # will fill empty slots with matches from lower order gram
  top <- ensure_filled(top, w, min_pred)
  top <- arrange(top, desc(count))
  
  # loop through top matches
  # measure the relevance of each bigram along with each of its words
  # bigrams which come from higher order grams should get additional boost
  best <- best_match(top)

  return(best[0:min_pred])
}

# @todo runtime profiling
# good test case: word "psychology" - not followed by anything?
# good test case for slow query: "psychology is a" - much slower than others
# random sentence that was built by using suggestions starting with "what":
# "what is the best thing to do in my spare time"
#p <- predict_word("the")
#print(p)
