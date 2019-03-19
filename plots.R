# Create a barplot of a n most occuring words in data; output in outputDir
simple_wordCount <- function(data, rapper, n) {
  # Count unique words for the rapper
  word_count <- data %>%
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

simple_netSentiment <- function(data) {
  #nrc_sad <- get_sentiments("nrc") %>%
    #filter(sentiment == "bing")
  
  #sadData <- data %>%
    #inner_join(nrc_sad) %>%
    #count(word, sort = TRUE)
  
  #print(sadData)
  
  # Added tryCatch as temporary fix because Jewell was throwing 
  # "Evaluation error: object 'positive' not found.[1] NA" message
  
  tryCatch(
    {
      netSentiment <- data %>%
        inner_join(get_sentiments("bing")) %>% # Use "bing" to get sentiment score of each word"
        count(word, sentiment) %>%
        spread(sentiment, n, fill = 0 ) %>%
        mutate(sentiment = positive - negative)
      #print(netSentiment)
      #TODO CREATE PLOT
    },
    error = function(error_message) {
      message(error_message)
      return(NA)
    }
  ) 
}

mostCommonPosNegWords <- function(data, rapper) {
  tryCatch(
    {
      bing_word_counts <- data %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
      
      bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contibution to sentiment", x = NULL) +
        coord_flip()
      
      filename <- "x.png"
      filename <- gsub("x", rapper, filename)
      ggsave(filename)
    }, 
    error = function(error_message) {
      message(error_message)
      return(NA)
    }
  )
}