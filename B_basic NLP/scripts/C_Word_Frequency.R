#' Title: Intro: Frequency Count & Dendrogram
#' Purpose: Learn about and visualize a dendrogram
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: 2020-Arp-13
#'

# Set the working directory
setwd("/cloud/project/B_basic NLP/data")


# Libs
library(qdap)
library(tm)
library(ggplot2)
library(ggthemes)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

cleanCorpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh', 'beer')

# Data 
text <- read.csv('beer.csv', header=TRUE)

# As of tm version 0.7-3 tabular was deprecated
names(text)[1] <- 'doc_id' 

# Build a volatile corpus
txtCorpus <- VCorpus(DataframeSource(text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus)

# Make TDM
beerTDM  <- TermDocumentMatrix(txtCorpus)
beerTDMm <- as.matrix(beerTDM)

# Frequency Data Frame
beerFreq <- rowSums(beerTDMm)
beerFreq <- data.frame(word=names(beerFreq),frequency=beerFreq)

# Simple barplot; values greater than 90 
topWords      <- subset(beerFreq, beerFreq$frequency >= 90) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

# qdap version, will not work if there is a java issue
plot(freq_terms(text$text, top=35, at.least=2))

# End