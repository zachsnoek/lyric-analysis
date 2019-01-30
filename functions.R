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
# Get album names and urls for an artist
# Input: browser session, and an artist page
# Output: named list, containing urls, album names, and years
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
get_albums <- function(browser, artist_page) {
  
  #go to an artist page
  message("Going to Artist page...")
  browser$navigate(artist_page)
  Sys.sleep(10)
  
  
  #find the "show all albums button" and click it
  message("Finding album URLS and titles...")
  webElem <- chrome$findElements(
    using = "xpath", 
    value = "/html/body/routable-page/ng-outlet/routable-profile-page/ng-outlet/routed-page/profile-page/div[3]/div[2]/artist-songs-and-albums/album-grid/div[2]"
  )
  
  unlist(lapply(webElem, function(x){x$getElementText()}))
  webElem[[1]]$clickElement()
  
  
  #get album URLs
  webElems <- chrome$findElements(
    using = "class",
    value = "mini_card--small"
  )
  
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
  
  message("Going to album page...")
  browser$navigate(album_page)
  Sys.sleep(10)
  
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
# foo
# foo$urls
# foo$titles
