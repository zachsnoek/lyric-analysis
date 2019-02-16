#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# functions.R - Matt Sandgren and Zach Snoek
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------


# ********* All code outside of functions should be commented out before **************
# ********* saving or sourcing. (Except for the libraries())             **************


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Helpful links
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Port in use error?
# https://stackoverflow.com/questions/43991498/rselenium-server-signals-port-is-already-in-use

# RSelenium docs
# https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Libraries
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# install.packages("tidyverse")
# install.packages("RSelenium")
library(tidyverse)
library(RSelenium)





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Get Wikipedia's list of hip hop musicians
# Input: browser session
# Output: Character vector of cleaned artist names
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
get_artist_names <- function(browser) {
  
  #navigate to wikipedia page, and get the dirty artist names
  browser$navigate("https://en.wikipedia.org/wiki/List_of_hip_hop_musicians")
  message("Going to Wikipedia page...")
  # pause for 10 seconds, to make sure page is actually loaded
  Sys.sleep(10)
  
  message("Finding artist names...")
  webElems <- browser$findElements(
    using = "xpath", 
    value = "//*[@id='mw-content-text']/div/div/ul/li"
  )
  artist_names <- unlist(lapply(webElems, function(x){x$getElementText()}))
  
  # Format artist names for URLs
  message("Formatting artist names...")
  for (i in 1:length(artist_names)) {
    
    artist_names[i] <- 
      artist_names[i] %>% 
      # Remove escaped double quotes
      gsub("\"", "", .) %>% 
      # Remove Wiki reference numbers
      gsub("\\[\\d\\]", "", .) %>% 
      gsub("\\.", "", .) %>% 
      gsub("\'", "", .)
    
  }
  
  # Remove artists with parentheticals in heading
  paren <- grep("\\(|\\)", artist_names)
  artist_names <- artist_names[-paren]
  
  # Remove last 3 entries (Wiki headings)
  artist_names <- artist_names[1:(length(artist_names)-3)]
  
  message("Returned artist names")
  return(artist_names)
}

# try it
# foo <- get_artist_names(chrome)





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Construct genius.com artist page urls
# Input: Browser session
# Output: Character vector of genius artis page urls
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
make_artist_urls <- function(browser) {
  
  message("Getting artist names...")
  artist_names <- get_artist_names(browser)
  
  artist_page <- "https://genius.com/artists/" # Every Genius artist page is found in /artists
  
  # empty vector for urls
  urls <- vector("character", length = 0L)
  
  message("Constructing Artist page URLs...")
  for (i in 1:length(artist_names)) {
    
    name <- NULL
    
    name <-
      gsub(" ", "-", artist_names[i]) %>% 
      paste(artist_page, ., sep = "")
    
    urls[i] <- name
  }
  
  message("Returned URLs")
  return(urls)
  
}

#try it
# make_artist_urls(chrome)




#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Check if a genius page load failed
# Input: browser session
# Output: boolean
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
page_found <- function(browser) {

    webElems <- browser$findElements(
    using = "class",
    value = "render_404-headline"
  )
  
  if(identical(vector("list", 0L), webElems)){
    #page found
    return(TRUE)
  } else {
    #page not found
    return(FALSE)
  }
  
}

# chrome$navigate("https://genius.com/artists/Kendrick-fooooooooooooooooooooooooooooooooo")
# page_found(chrome)


# chrome$navigate("https://genius.com/artists/Kendrick-lamar")
# page_found(chrome)





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Get album names and urls for an artist
# Input: browser session, and an artist page
# Output: named list, containing urls, album names, and years
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
get_albums <- function(browser, artist_page) {
  
  #get the artist name from URL for a fancy message
  artist <- 
    artist_page %>% 
    str_extract(., "([^\\/]+$)") %>%
    gsub("-", " ", .)
  
  #go to an artist page
  message("Going to ", artist, " artist page...")
  browser$navigate(artist_page)
  Sys.sleep(15)
  
  # return false if the page wasn't found
  if(!page_found(browser)) {
    message("Artist not found; returned FALSE")
    return(FALSE)
  }
  
  #find the "show all albums button" and click it
  message("Finding album URLS and titles...")
  webElem <- browser$findElements(
    using = "xpath", 
    value = "/html/body/routable-page/ng-outlet/routable-profile-page/ng-outlet/routed-page/profile-page/div[3]/div[2]/artist-songs-and-albums/album-grid/div[2]"
  )
  
  # if there's no albums button
  if(identical(webElem, vector("list", 0L))) {
    return(FALSE)
  }
  
  # unlist(lapply(webElem, function(x){x$getElementText()}))
  webElem[[1]]$clickElement()
  Sys.sleep(15)
  
  #get album URLs
  webElems <- browser$findElements(
    using = "class",
    value = "mini_card--small"
  )
  
  # if there's no albums listed
  if(identical(webElems, vector("list", 0L))) {
    return(FALSE)
  }
  
  album_urls <- unlist(lapply(webElems, function(x){x$getElementAttribute("href")}))
  album_names <- unlist(lapply(webElems, function(x){x$getElementText()})) %>% 
    strsplit(., split = "\\n")
  
  message("Returned discography")
  return(list(urls = album_urls, info = album_names))
  
}


#---------------------------------------------------------------------------------------
# how to use the output from this thing:
#---------------------------------------------------------------------------------------
# foo <- get_albums(chrome, "https://genius.com/artists/Kendrick-lamar")

#to get urls:
# foo$urls

#to get album name and year:
# foo$info[[1]]
# foo$info[[5]]


#to get album names:
# foo$info[[1]][1]
# foo$info[[5]][1]

#to get album years:
# foo$info[[1]][2]
# foo$info[[5]][2]





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# get song URLs from an album page
# Input: browser session, album url
# Output: Named list of song URLs and song titles
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
get_songs <- function(browser, album_page) {

  # album name for a fancy message  
  album <- 
    album_page %>% 
    str_extract(., "([^\\/]+$)") %>%
    gsub("-", " ", .)
  
  #go to an artist page
  message("Going to ", album, " album page...")
  browser$navigate(album_page)
  Sys.sleep(15)
  
  # return false if the page wasn't found
  if(!page_found(browser)) {
    message("Album not found; returned FALSE")
    return(FALSE)
  }
  
  # get song URLs
  message("Getting song URLs...")
  webElems <- browser$findElements(
    using = "class",
    value = "u-display_block"
  )
  song_urls <- unlist(lapply(webElems, function(x){x$getElementAttribute("href")}))
  
  
  # get song titles
  message("Getting song titles...")
  webElems <- browser$findElements(
    using = "class",
    value = "chart_row-content-title"
  )
  song_names <- unlist(lapply(webElems, function(x){x$getElementText()}))
  
  
  #remove any URL's and title that contain booklet
  song_urls <- song_urls[!grepl("booklet", song_urls, ignore.case = TRUE)]
  song_names <- song_names[!grepl("booklet", song_names, ignore.case = TRUE)] %>% 
    gsub("Lyrics", "", .) %>% 
    gsub("\\s+$", "", .)
  
  message("Returned song information")
  return(list(urls = song_urls, titles = song_names))
}


# try it
# foo <- get_songs(chrome, "https://genius.com/albums/Kendrick-lamar/Damn")
# foo <- get_songs(chrome, "https://genius.com/albums/Kendrick-lamar/foofoofoofoo")
# foo <- get_songs(chrome, "https://genius.com/albums/03-greedo/Purple-summer-03-purple-hearted-soldier")

# foo
# foo$urls
# foo$titles





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# get song lyrics from a song page
# Input: browser session, song url
# Output: Song lyrics, in one string
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
get_song_info <- function(browser, song_page) {

  artist_song <- 
    song_page %>% 
    str_extract(., "([^\\/]+$)") %>%
    gsub("-", " ", .)
  message("Getting ", artist_song, "...")

  browser$navigate(song_page)
  Sys.sleep(15)

  # return false if the page wasn't found
  if(!page_found(browser)) {
    message("Song not found; returned FALSE")
    return(FALSE)
  }
  
  #------------------------------------------------
  # get lyrics
  #------------------------------------------------
  webElems <- browser$findElements(
    using = "class",
    value = "lyrics"
  )
    
    lyrics <- 
      unlist(lapply(webElems, function(x){x$getElementText()})) %>% 
      strsplit(., "\\n") %>% 
      unlist() %>% 
      paste(., collapse = " ")
    
    unreleased <- "Lyrics for this song have yet to be released. Please check back once the song has been released."
    if(lyrics == unreleased){
      return(FALSE)
    }
    
    #------------------------------------------------
    # get song title
    #------------------------------------------------
    webElem <- browser$findElement(
      using = "class",
      value = "header_with_cover_art-primary_info-title"
    )
    title <- 
      webElem$getElementText() %>%
      unlist()
    
    
    #------------------------------------------------
    # get actual artist
    # some albums are collabs, with multiple artists
    # can't assume they're all by the same one
    #------------------------------------------------
    webElem <- browser$findElement(
      using = "xpath",
      value = "/html/body/routable-page/ng-outlet/song-page/div/div/header-with-cover-art/div/div/div[1]/div[2]/div/h2/span/a"
    )
    actual_artist <- 
      webElem$getElementText() %>%
      unlist()
    
    
    
    #------------------------------------------------
    # get collaborators, if any
    #------------------------------------------------
    #check to see if there are any collaborators
    webElems <- browser$findElements(
      using = "class",
      value = "metadata_unit-label"
    )
    available_info <- unlist(lapply(webElems, function(x){x$getElementText()}))
    
    if("Featuring" %in% available_info) {
      
      # check to see if there's a show more button 
      # if so, click it
      webElem <- tryCatch({ #AAAAAAAAAAAAAAAARRRRRRRRRGGGGGHHHHHHHHHHHHHHHH
        browser$findElements(
          using = "xpath",
          value = "/html/body/routable-page/ng-outlet/song-page/div/div/header-with-cover-art/div/div/div[1]/div[2]/div/ng-transclude/metadata/h3[1]/expandable-list/div/span[2]/span[4]/a"
          )
      }, error = function(e) {
        webElem <- NA
      }, warning = function(w) {
        webElem <- NA
      })
      
      if(!identical(webElem, vector("list", 0L))) {
        webElem[[1]]$clickElement()
        Sys.sleep(15)
      }      
      
      # find the collaborator names
      webElems <- browser$findElements(
        using = "xpath",
        value = "/html/body/routable-page/ng-outlet/song-page/div/div/header-with-cover-art/div/div/div[1]/div[2]/div/ng-transclude/metadata/h3[1]/expandable-list/div/span[2]/span"
      )
      collaborators <- 
        unlist(lapply(webElems, function(x){x$getElementText()})) %>% 
        gsub(",", "", .) %>% 
        gsub(" &", "", .)
      
    } else {
      collaborators <- NA
    }
    
    return(list(lyrics = lyrics,
                title = title, 
                actual_artist = actual_artist,
                collaborators = list(collaborators)))
}


# foo <- get_song_info(chrome, "https://genius.com/Kendrick-lamar-blood-lyrics")
# foo <- get_song_info(chrome, "https://genius.com/Kendrick-lamar-BLOODONTHEDANCEFLOOR-lyrics")
# foo <- get_song_info(chrome, "https://genius.com/Havoc-and-the-alchemist-buck-50s-and-bullet-wounds-lyrics")

# foo <- get_song_info(chrome, "https://genius.com/03-greedo-roadrunning-lyrics")
# foo <- get_song_info(chrome, "https://genius.com/03-greedo-run-for-yo-life-lyrics")
# foo <- get_song_info(chrome, "https://genius.com/03-greedo-buckhead-lyrics")
# foo
# foo$lyrics
# foo$title
# foo$collaborators
