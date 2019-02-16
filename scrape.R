source("functions.R")




#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Start a Selenium server and client; set Chrome as the client port
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
rD <- rsDriver(
  verbose = FALSE, 
  port = 4444L, 
  browser = "chrome"
)
chrome <- rD$client





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# create the list of artists and URLs to use in the scrape
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
artist_list <- get_artist_names(chrome)
artist_urls <- make_artist_urls(chrome)



chosen <- c("Tupac Shakur", 
            "The Notorious BIG", 
            "Dr Dre",
            "Eminem", 
            "Kendrick Lamar",
            "Black Thought", 
            "Lil Wayne",
            "Nicki Minaj",
            "Andy Samberg",
            "J Cole") %>% 
  match(., artist_list)

small_artist_list <- artist_list[chosen]
small_artist_urls <-artist_urls[chosen]

# *** Run these two chunks before the loop. The loop appends to these,
# *** if they're not empty you'll get duplicates.
# empty tibble to add to
# lyricsDB <-
#   tibble(
#     artist = character(0),
#     album = character(0),
#     year = character(0),
#     song = character(0),
#     collaborators = character(0),
#     lyrics = character(0),
#     artist_url = character(0),
#     album_url = character(0),
#     song_url = character(0)
#   )

# tibble to add failed items to
# failures <-
#   tibble(
#     artist = character(0),
#     album = character(0),
#     year = character(0),
#     song = character(0),
#     collaborators = character(0),
#     lyrics = character(0),
#     artist_url = character(0),
#     album_url = character(0),
#     song_url = character(0)
#   )





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# The scrape
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
for(i in 1:length(small_artist_urls)) {
  
  #start by getting an artist URL, and their albums
  artist_url <- small_artist_urls[i]
  albums <- get_albums(chrome, artist_url)
  
  # Make sure there's actually albums for an artist.
  # If there aren't, move to the next artist
  # and add artist to failures tibble (lol)
  if(identical(FALSE, albums)) {
    failures <- add_row(
      failures,
      artist_url = artist_url
    )
    #move to next artist
    next()
  }
  
  
  
  # ---------------------------------------
  # for each album for this artist
  # ---------------------------------------
  for(j in 1:length(albums$urls)) {
    
    album <- albums$info[[j]][1]
    year <- albums$info[[j]][2]
    album_url <- albums$urls[j]
    songs <- get_songs(chrome, album_url)
    
    # check to see if album was found
    # if it wasn't, move to next album
    # and add album to failures tibble
    if(identical(FALSE, songs)) {
      # uh oh, album not found
      failures <- add_row(
        failures,
        album = album,
        year = year,
        artist_url = artist_url,
        album_url = album_url
      )
      #move to next album
      next()
    }
    
    
    # ---------------------------------------
    # for each song on this album
    # ---------------------------------------
    for(k in 1:length(songs$urls)) {
      
      song_url <- songs$urls[k]
      
      # If get_song_info fails, try again
      song_info <- NULL
      attempt <- 1
      while(is.null(song_info) && attempt <= 3) {
        attempt <- attempt + 1
        try(
          song_info <- get_song_info(chrome, song_url)
        )
      }
      
      
      # check if lyrics were found. If not, move to next song
      # and add song to failures tibble
      if(identical(FALSE, song_info)) {
        # uh oh, song not found
        failures <- add_row(
          failures,
          album = album,
          year = year,
          artist_url = artist_url,
          album_url = album_url,
          song_url = song_url
        )
        #move to next song
        next
      } else {
        
        song <- song_info$title
        lyrics <- song_info$lyrics
        collaborators <- song_info$collaborators
        artist <- song_info$actual_artist
        
        # success! add everything to tibble
        lyricsDB <- add_row(
          lyricsDB,
          artist = artist,
          album = album,
          year = year,
          song = song,
          collaborators = collaborators,
          lyrics = lyrics,
          artist_url = artist_url,
          album_url = album_url,
          song_url = song_url
        )
      }
    }
  }
}



#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Always run these to close out the browser and server.
# Forgetting to stop the server will give you a 'port already in use' 
# error the next time you try to start a browser
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
chrome$close()
rD$server$stop()
