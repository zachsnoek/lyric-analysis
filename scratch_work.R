# a bit of scratch work

install.packages("data.table")
library(data.table)
library(dplyr)

setwd("/Users/zacharysnoek/Programming/r")

#fwrite(lyricsDB, "test_set.csv", row.names = FALSE)
foo <- fread("test_set.csv", sep2 = "|")
foo[, collaborators := NULL]

foo <- foo %>% 
  mutate(lyrics = gsub("\\[.*?\\]", "", lyrics)) %>%
  mutate(lyrics = gsub("\\(.*?\\)", "", lyrics)) %>%
  mutate(lyrics = gsub("\\\\", "", lyrics))

foo %>% pull(lyrics)