# a bit of scratch work

install.packages("data.table")
install.packages("tidytext")
<<<<<<< HEAD
=======
library(data.table)
library(tidytext)
library(dplyr)

setwd("/Users/zacharysnoek/Programming/r")

#fwrite(lyricsDB, "test_set.csv", row.names = FALSE)
foo <- fread("test_set.csv", sep2 = "|")
foo[, collaborators := NULL]

# String cleaning
foo <- foo %>% 
  mutate(lyrics = gsub("\\[.*?\\]", "", lyrics)) %>%
  mutate(lyrics = gsub("\\(.*?\\)", "", lyrics)) %>%
  mutate(lyrics = gsub("\\\\", "", lyrics))

#==================
# tidytext fun
#==================
text <- foo %>% 
  pull(lyrics)
lyrics_df <- as_tibble(text)

# Break text into individual tokens
lyrics_df <- lyrics_df %>%
  unnest_tokens(word, value)

lyrics_df

# Remove stop words
data("stop_words")
lyrics_df <- lyrics_df %>%
  anti_join(stop_words)

# Simple word count
lyrics_df <- lyrics_df %>%
  count(word, sort = TRUE)

lyrics_df
>>>>>>> 2ca0af2564eeceef330845dac9af100153b55aed
library(data.table)
library(tidytext)
library(dplyr)

setwd("/Users/zacharysnoek/Programming/r")

#fwrite(lyricsDB, "test_set.csv", row.names = FALSE)
foo <- fread("test_set.csv", sep2 = "|")
foo[, collaborators := NULL]

# String cleaning
foo <- foo %>% 
  mutate(lyrics = gsub("\\[.*?\\]", "", lyrics)) %>%
  mutate(lyrics = gsub("\\(.*?\\)", "", lyrics)) %>%
  mutate(lyrics = gsub("\\\\", "", lyrics))

#==================
# tidytext fun
#==================
text <- foo %>% 
  pull(lyrics)
lyrics_df <- as_tibble(text)

# Break text into individual tokens
lyrics_df <- lyrics_df %>%
  unnest_tokens(word, value)

lyrics_df

# Remove stop words
data("stop_words")
lyrics_df <- lyrics_df %>%
  anti_join(stop_words)

# Simple word count
lyrics_df <- lyrics_df %>%
  count(word, sort = TRUE)

lyrics_df