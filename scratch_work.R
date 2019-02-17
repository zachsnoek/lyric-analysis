# a bit of scratch work

install.packages("data.table")
install.packages("tidytext")
library(data.table)
library(tidytext)
library(tidyverse)
library(dplyr)

setwd("/Users/zacharysnoek/Programming/r/rap-data")

# Load dataset
initial <- fread("dataset_0.csv", sep2 = "|")
initial[, collaborators := NULL]

# String cleaning
initial <- initial %>% 
  mutate(lyrics = gsub("\\[.*?\\]", "", lyrics)) %>%
  mutate(lyrics = gsub("\\(.*?\\)", "", lyrics)) %>%
  mutate(lyrics = gsub("\\\\", "", lyrics))

# Create a list of unique artsts
artists <- initial$artist %>% unique()
# Use a small subset to test with
artists <- artists[1:10]

for (i in 1:length(artists)) {
  # Save plots in png
  setwd("/Users/zacharysnoek/Programming/r/rap-analyses/png")
  
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
  
  # Count unique words for the rapper
  word_count <- lyrics_df %>%
    count(word, sort = TRUE)
  
  # Create a bar plot of first 50 most occurring words
  head <- word_count[1:50,]
  
  # Do some stuff to sort the x axis correctly
  head$word <- as.vector(head$word) #get rid of factors
  head$word = factor(head$word,head$word) #add ordered factors back
  
  ggplot(head, aes(x = word, y = n)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 65, hjust = 1))
  
  filename <- "x.png"
  filename <- gsub("x", rapper, filename)
  ggsave(filename)
}