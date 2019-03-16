library(tidyverse)
library(textstem)
library(tidytext)
library(spacyr)


# make a tibble with a word, lemma, and stem column
# Really should get spacy working to expand contractions
lemmad <- lyricsDB %>% 
  select(artist, album, year, song, collaborators, lyrics) %>% 
  unnest_tokens(., word, lyrics) %>% 
  anti_join(stop_words) %>% 
  mutate(lemma = textstem::lemmatize_words(word)) %>% 
  mutate(stem = textstem::stem_words(word))
lemmad




lemmad %>% count(word, sort = TRUE)

