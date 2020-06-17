# Word Cloud
# Hansen, McMahon, and Prat, “Transparency and Deliberation,”,
#   The Quarterly Journal of Economics, 823.

# Data: USA 2020 (not released); pride2

df <- read.csv("data/protected/usa-2020.csv")
output_dir <- "output"

library(magrittr)
library(wordcloud)
library(wordcloud2)
library(stringi)
library(tm)

text <- df$pride2
text <- stri_remove_empty(text)
text <- gsub('[^ -~]', '', text)
docs <- Corpus(VectorSource(text))

# Clean text... (Assuming English)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("SMART")) # Use American English stop words
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

# Convert to frequency table
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)

# Generate Word Cloud
wdf <- data.frame(word = names(words),freq=words)

# Show Word Cloud
wordcloud2(data=head(wdf,100),         # Most frequent 100 words
           color= 'random-dark',
           size=4,
           rotateRatio=0,
           ellipticity= 0.4,
           fontFamily = 'Baskerville'
           )
