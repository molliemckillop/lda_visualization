install.packages("LDAvis")
library("LDAvis", lib.loc="~/Library/R/3.1/library")
tweets.02.07.2016.summary <- read.csv("~/Desktop/downloads/tweets.02.07.2016.summary.csv")
library(tm)
install.packages("lda")
library("lda", lib.loc="~/Library/R/3.1/library")
install.packages("servr")
library("servr", lib.loc="~/Library/R/3.1/library")
install.packages("shiny")
library(shiny)

stop_words <- stopwords("SMART")
tweet <- tweets.02.07.2016.summary$text

tweet <- gsub("'", "", tweet)  # remove apostrophes
tweet <- gsub("https", "", tweet)  # remove url
tweet <- gsub("[[:punct:]]", " ", tweet)  # replace punctuation with space
tweet <- gsub("[[:cntrl:]]", " ", tweet)  # replace control characters with space
tweet <- gsub("^[[:space:]]+", "", tweet) # remove whitespace at beginning of documents
tweet <- gsub("[[:space:]]+$", "", tweet) # remove whitespace at end of documents
tweet <- tolower(tweet)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(tweet, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data 
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 


# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = 10, vocab = vocab, 
                                   num.iterations = 200, alpha = 0.5, eta=0.5,
                                    initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  

#LDAvis
theta <- t(apply(fit$document_sums + 0.5, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + 0.5, 2, function(x) x/sum(x)))


tweetvis <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = tweetvis$phi, 
                   theta = tweetvis$theta, 
                   doc.length = tweetvis$doc.length, 
                   vocab = tweetvis$vocab, 
                   term.frequency = tweetvis$term.frequency)

serVis(json, out.dir = tempfile(), open.browser = interactive())