# https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html
# https://www.r-graph-gallery.com/network/

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
  "J. Cole",
  "Drake", 
  "Common",
  "Anderson Paak",
  "J Dilla",
  "Ice Cube",
  "21 Savage",
  "Lecrae")

#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Chord diagram (the circle one)
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
collab <- 
  lyricsDB %>% 
  filter(., collaborators != "NA") %>% 
  select(artist, collaborators) %>% 
  unnest() %>% 
  filter(., artist %in% small_artist_list) %>% 
  group_by(artist, collaborators) %>% 
  summarise(n = n()) %>% 
  filter(n > 5)

collab

unique(collab$artist)

chordDiagram(collab)
circos.clear()




#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Network diagrams
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
install.packages("networkD3")
library(networkD3)

foonet <- 
  lyricsDB %>% 
  filter(., collaborators != "NA") %>% 
  select(artist, collaborators) %>% 
  unnest() %>% 
  filter(., artist %in% small_artist_list) %>% 
  group_by(artist, collaborators) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2)

foonet

simpleNetwork(
  foonet,
  # linkDistance = 120,
  linkColour = "green",
  nodeColour = "blue",
  opacity = 1,
  zoom = TRUE
)
