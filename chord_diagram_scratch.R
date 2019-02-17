install.packages("circlize")
library(circlize)


small_artist_list <- c(
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
  "J. Cole")

collab <- 
  lyricsDB %>% 
  filter(., collaborators != "NA") %>% 
  select(artist, collaborators) %>% 
  unnest() %>% 
  filter(., artist %in% small_artist_list) %>% 
  group_by(artist, collaborators) %>% 
  summarise(n = n()) %>% 
  filter(n > 10)

collab

unique(collab$artist)

chordDiagram(collab)
circos.clear()
