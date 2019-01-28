# functions.R - Matt Sandgren and Zach Snoek

# Port in use error?
# https://stackoverflow.com/questions/43991498/rselenium-server-signals-port-is-already-in-use

# RSelenium docs
# https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf

install.packages("tidyverse")
install.packages("RSelenium")
library(tidyverse)
library(RSelenium)

# Start a Selenium server and client; set Chrome as the client port
rD <- rsDriver(verbose = FALSE, port = 4444L, browser = "chrome")
chrome <- rD$client

# Get Wikipedia's list of hip hop musicians
chrome$navigate("https://en.wikipedia.org/wiki/List_of_hip_hop_musicians")
webElems <- chrome$findElements(using = "xpath", value = "//*[@id='mw-content-text']/div/div/ul/li")
artist_names <- unlist(lapply(webElems, function(x){x$getElementText()}))

# Format artist names for URLs
for (i in 1:length(artist_names)) {
  # Remove escaped double quotes
  artist_names[i] <- gsub("\"", "", artist_names[i])
  
  # Remove Wiki reference numbers
  artist_names[i] <- gsub("\\[\\d\\]", "", artist_names[i])
  
  artist_names[i] <- gsub("\\.", "", artist_names[i])
  artist_names[i] <- gsub("\'", "", artist_names[i])
}

# Remove artists with parentheticals in heading
paren <- grep("\\(|\\)", artist_names)
artist_names = artist_names[-paren]

# Remove last 3 entries (Wiki headings)
artist_names <- artist_names[1:(length(artist_names)-3)]

# Construct genius.com artist page urls
artist_page <- "https://genius.com/artists/" # Every Genius artist page is found in /artists

urls = c()

for (i in 1:length(artist_names)) {
  # Probably not the right way to do this in R
  name <- gsub(" ", "-", artist_names[i])
  name <- paste(artist_page, name, sep = "")
  urls <- c(urls, name)
}

urls

# Try/catch for URLs
failed = c()

# Always run these to close out the browser and server.
# Forgetting to stop the server will give you a 'port already in use' 
# error the next time you try to start a browser
chrome$close()
rD$server$stop()