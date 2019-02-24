# Create a barplot of a n most occuring words in data; output in outputDir
simple_wordCount <- function(data, n, outputDir) {
  setwd(outputDir)
  
  for (i in 1:length(data)) {
    # Get the lyrics for each rapper
    rapper <- toString(data[i])
    filtered_df <- filter(initial, artist == rapper)
    text <- filtered_df %>% 
      pull(lyrics)
    
    # Tokenize lyrics
    lyrics_df <- as_tibble(text) %>%
      unnest_tokens(word, value)
    
    # Remove stop words
    data("stop_words")
    lyrics_df <- lyrics_df %>%
      anti_join(stop_words)
    
    # Include this to filter out naughty words
    #'%ni%' <- Negate('%in%')
    #nsfw <- c("bitch", "bitches", "fuck", "fuckin", "shit", "damn", 
    #          "pussy", "nigga", "niggas", "ass", "dick")
    #lyrics_df <- filter(lyrics_df, word %ni% nsfw)
    
    # Count unique words for the rapper
    word_count <- lyrics_df %>%
      count(word, sort = TRUE)
    
    # Create a bar plot of first 50 most occurring words
    head <- word_count[1:n,]
    
    # Do some stuff to sort the x axis correctly
    head$word <- as.vector(head$word) #get rid of factors
    head$word = factor(head$word,head$word) #add ordered factors back
    
    ggplot(head, aes(x = word, y = n)) +
      geom_bar(stat = "identity") +
      coord_flip()
    
    filename <- "x.png"
    filename <- gsub("x", rapper, filename)
    ggsave(filename)
  }
}