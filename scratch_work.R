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

text <- initial %>% 
  pull(lyrics)
#lyrics_df <- as_tibble(text)

# Break text into individual tokens
lyrics_df <- as_tibble(text) %>%
  unnest_tokens(word, value)

lyrics_df

# Remove stop words
data("stop_words")
lyrics_df <- lyrics_df %>%
  anti_join(stop_words)

lyrics_df

# Include to filter out naughty words
#'%ni%' <- Negate('%in%')
#nsfw <- c("bitch", "bitches", "fuck", "fuckin", "shit", "damn", 
#          "pussy", "nigga", "niggas", "ass", "dick")
#lyrics_df <- filter(lyrics_df, word %ni% nsfw)

# Simple word count
word_count <- lyrics_df %>%
  count(word, sort = TRUE)

# Test bar plot of first 50 most occurring words
head <- word_count[1:50,]

head$word <- as.vector(head$word) #get rid of factors
head$word = factor(head$word,head$word) #add ordered factors back

ggplot(head, aes(x = word, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

setwd("/Users/zacharysnoek/Programming/r/rap-analyses/png")
ggsave("most-occurring-head.png")