# install.packages("data.table")
# install.packages("tidytext")
library(data.table)
library(tidytext)
library(tidyverse)

# Location of rap data
dataDir = "/Users/zacharysnoek/Programming/r/rap-data"
dataSet = "dataset_1.csv"
# Location of plots script
plotsScript = "/Users/zacharysnoek/Programming/r/rap-analyses/plots.R"
# Output directory of plots
output = "/Users/zacharysnoek/Programming/r/rap-analyses/png/word-count"

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

simple_wordCount(artists, 50, output)
