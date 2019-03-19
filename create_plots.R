install.packages("data.table")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("reshape2")
library(data.table)
library(tidytext)
library(tidyverse)
library(dplyr)
library(stringr)
library(wordcloud)
library(reshape2)

# Location of rap data
dataDir = "/Users/zacharysnoek/Programming/r/rap-analyses/csv"
dataSet = "dataset_1.csv"

# Location of plots script
plotsScript = "/Users/zacharysnoek/Programming/r/rap-analyses/plots.R"

# Output directory of plots
baseDir = "/Users/zacharysnoek/Programming/r/rap-analyses/png/"
wordCountDir = paste(baseDir, "word-count", sep="")
netSentimentDir = paste(baseDir, "net-sentiment", sep="")
mostCommonPosNegWordsDir = paste(baseDir, "most-common-pos-neg-words", sep="")
wordCloudDir = paste(baseDir, "word-cloud", sep="")
comparisonCloudDir = paste(baseDir, "comparison-cloud", sep="")

source(plotsScript)
setwd(dataDir)

# Load dataset
initial <- fread(dataSet, sep2 = "|")
initial[, collaborators := NULL]

# String cleaning
initial <- initial %>% 
  mutate(lyrics = gsub("\\[.*?\\]", "", lyrics)) %>%
  mutate(lyrics = gsub("\\(.*?\\)", "", lyrics)) %>%
  mutate(lyrics = gsub("\\\\", "", lyrics))

# Create a list of unique artsts
artists <- initial$artist %>% unique()
# Use a small subset to test with
#artists <- artists[1:10]

for (i in 1:length(artists)) {
  # Get the lyrics for each rapper
  rapper <- toString(artists[i])
  filtered_df <- filter(initial, artist == rapper)
  text <- filtered_df %>% 
    pull(lyrics)
  
  # Tokenize lyrics
  lyrics_df <- as_tibble(text) %>%
    unnest_tokens(word, value)
  
  # Remove stop words
  data("stop_words")
  lyrics_df <- lyrics_df %>%
    anti_join(stop_words)
  
  # Include this to filter out naughty words
  #'%ni%' <- Negate('%in%')
  #nsfw <- c("bitch", "bitches", "fuck", "fuckin", "shit", "damn", 
  #          "pussy", "nigga", "niggas", "ass", "dick")
  #lyrics_df <- filter(lyrics_df, word %ni% nsfw)
  
  #===================================================================
  # The code below creates each type of plot; uncomment when necessary
  #===================================================================
  
  #======= WORD COUNT =======#
  #setwd(wordCountDir)
  #simple_wordCount(lyrics_df, rapper, 50)
  
  #======= SENTIMENT ANALYSIS =======#
  #setwd(netSentimentDir)
  #netSentiment(lyrics_df, rapper)
  
  #======= MOST COMMON POS AND NEG WORDS =======#
  #setwd(mostCommonPosNegWordsDir)
  #mostCommonPosNegWords(lyrics_df, rapper)
  
  #======= WORD CLOUD =======#
  #setwd(wordCloudDir)
  #wordCloud(lyrics_df, rapper)
  
  #======= COMPARISON CLOUD =======#
  #setwd(comparisonCloudDir)
  #comparisonCloud(lyrics_df, rapper)
}

