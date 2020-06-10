# Word Cloud
# Hansen, McMahon, and Prat, “Transparency and Deliberation,”, The Quarterly Journal of Economics, 823.

df <- read.csv("data/protected/usa-2020.csv")

library(wordcloud)
library(wordcloud2)
library(plyr)
library(tm)

text <- df$ex45a
plyr::compact(text)
docs <- Corpus(VectorSource(text))

# clean text..
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 

wdf <- data.frame(word = names(words),freq=words)

wordcloud2(data=wdf)
