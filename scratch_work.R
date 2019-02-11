# a bit of scratch work

install.packages("data.table")
library(data.table)


# setwd("C://Users/msand/Documents/genius lyrics")
fwrite(lyricsDB, "test_set.csv", row.names = FALSE)

# set your working directory before reading
foo <- fread("test_set.csv", sep2 = "|")
