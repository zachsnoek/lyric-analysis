#---------------------------------------------------------
# Packages
#---------------------------------------------------------
library(data.table)
library(tidytext)
library(tidyverse)
library(dplyr)
library(stringr)
library(wordcloud)
library(reshape2)
library(magrittr)









#---------------------------------------------------------
# snoek directories
#---------------------------------------------------------
# Location of rap data
dataDir <- "/Users/zacharysnoek/Programming/r/rap-analyses/csv"
dataSet <- "dataset_1.csv"

# Location of plots script
plotsScript <- "/Users/zacharysnoek/Programming/r/rap-analyses/plots.R"




#---------------------------------------------------------
# source functions for creating plots
#---------------------------------------------------------
setwd(dataDir)
source(plotsScript)





#---------------------------------------------------------
# Load dataset
#---------------------------------------------------------
initial <- fread(dataSet, sep2 = "|")





#---------------------------------------------------------
# make an artist list
#---------------------------------------------------------
# Create a list of unique artsts
# artists <- initial$artist %>% unique()

# artists for whom we have full discographies
artists <- c(
  "Tupac Shakur", 
  "2Pac",
  "The Notorious BIG", 
  "Dr. Dre",
  "Eminem", 
  "Kendrick Lamar",
  "Black Thought", 
  "Lil Wayne",
  "Nicki Minaj",
  "Andy Samberg",
  "J. Cole",
  "Drake", 
  "Common",
  "Anderson Paak",
  "J Dilla",
  "Ice Cube",
  "21 Savage",
  "Lecrae")

# Use a small subset to test with
#artists <- artists[1:10]



for (i in 1:length(artists)) {}


#------------------------------------------
# clean it, use this output to pass to create_plots
#------------------------------------------
lyrics_df <- plot_clean(initial)


# right now it's limited to only one plot type displayed at a time
# I think if you use save = TRUE, it would save multiple types? 
#     since it wouldn't be returning(plot), only doing savePlot(plot)
#     and returning nothing
# See plots.R create_plots() for all the plot type arguments
create_plots(lyrics_df, "Common", swcount = TRUE, save = FALSE)
create_plots(lyrics_df, "Common", compcloud = TRUE, save = FALSE)






