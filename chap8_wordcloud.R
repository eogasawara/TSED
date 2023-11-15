source("header.R")
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm) #Create a vector containing only the text
library(dplyr)

plot_cloud <- function(data, stops=NULL) {
  data$text <- paste(data$title, data$abstract, data$author_keywords) 
  
  text <- data$text# Create a corpus  
  docs <- Corpus(VectorSource(text))
  
  docs <- docs |>
    tm_map(removeNumbers) |>
    tm_map(removePunctuation) |>
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  if (!is.null(stops))
    docs <- tm_map(docs, removeWords, stops)

  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  
  return(df)
}

png(filename="figures/chap8_wordcloud.png", width = 1024, height = 768)
par(mfrow=c(2,2)) # for 2 row, 2 cols

load("data/references/event_detection.RData")
load("data/references/event_prediction.RData")
#colors <- brewer.pal(9, "Set1")[-6]

df <- plot_cloud(rbind(event_detection, event_prediction), c("series" ,"time", "timeseries", "ieee", "springer"))
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, random.order=FALSE, rot.per=0.35, max.words=100, colors=colors)
title(sub = "(a)", font.sub = 1, cex.sub=2)

load("data/references/anomalies.RData")
df <- plot_cloud(anomalies, c("series" ,"time", "timeseries", "ieee", "springer"))
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, random.order=FALSE, rot.per=0.35, max.words=100, colors=colors)
title(sub = "(b)", font.sub = 1, cex.sub=2)

load("data/references/change_point.RData")
load("data/references/concept_drift.RData")
df <- plot_cloud(rbind(change_point,concept_drift), c("series" ,"time", "timeseries", "ieee", "springer"))
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, random.order=FALSE, rot.per=0.35, max.words=100, colors=colors)
title(sub = "(c)", font.sub = 1, cex.sub=2)

load("data/references/motif.RData")
df <- plot_cloud(motif, c("series" ,"time", "timeseries", "ieee", "springer"))
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, random.order=FALSE, rot.per=0.35, max.words=100, colors=colors)
title(sub = "(d)", font.sub = 1, cex.sub=2)
dev.off()

