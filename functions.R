# port in use error?
# https://stackoverflow.com/questions/43991498/rselenium-server-signals-port-is-already-in-use

# RSelenium docs
# https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf

install.packages("tidyverse")
install.packages("RSelenium")
library(tidyverse)
library(RSelenium)



# start a selnium server and client
# set chrome as the client part. chrome is what we'll use to navigate around
rD <- rsDriver(verbose = FALSE, port = 4444L, browser = "chrome")
chrome <- rD$client


# go to a URL this way
chrome$navigate("http://www.google.com")
chrome$navigate("http://genius.com/verified-artists")


# this read the raw html page source. If we want to do a bunch of regex to extract
# what we need we could do this
chrome$getPageSource()



#one way to pick out element(s) of a page. the webElems vector is objects
# that can be used to click links and navigate around. 
# The unlist command on the second line is just retrieving the element text part of those
# objects, useful for putting artist name, URLs, album title, or whatever in a dataframe
webElems <- chrome$findElements(using = "class", value = "user_details")
unlist(lapply(webElems, function(x){x$getElementText()}))


#go to lin's page
webElems[[14]]$clickElement()

# bet you can't tell what this does
chrome$goBack()


#getting the artists using xpath instead of class
webElems <- chrome$findElements(using = "xpath", value = "//*[@id='main']/div")
unlist(lapply(webElems, function(x){x$getElementText()}))


#gets artist page URLs
# well, it worked yesterday but it broke
# unlist(lapply(webElems, function(x){x$getElementAttribute('href')}))


#get wikipedias list of hiphop musicians
chrome$navigate("https://en.wikipedia.org/wiki/List_of_hip_hop_musicians")
webElems <- chrome$findElements(using = "xpath", value = "//*[@id='mw-content-text']/div/div/ul/li")

artist_names <- unlist(lapply(webElems, function(x){x$getElementText()}))


# important to always run these to close out the browser and server
# forgetting to stop the server will give you a 'port already in use' 
# error the next time you try to start a browser
chrome$close()
rD$server$stop()





url <- "https://en.wikipedia.org/wiki/List_of_hip_hop_musicians"
p <- readLines(url)
