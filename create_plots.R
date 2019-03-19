# install.packages("data.table")
# install.packages("tidytext")
library(data.table)
library(tidytext)
library(tidyverse)

# Location of rap data
dataDir = "/Users/zacharysnoek/Programming/r/rap-analyses/csv"
dataSet = "dataset_1.csv"
# Location of plots script
plotsScript = "/Users/zacharysnoek/Programming/r/rap-analyses/plots.R"
# Output directory of plots
outputDir = "/Users/zacharysnoek/Programming/r/rap-analyses/png/word-count"

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

#simple_wordCount(artists, 50, outputDir)

setwd(outputDir)

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
  
  simple_wordCountRefactor(lyrics_df, rapper, 50)
}

