# -----------------------------------------------------------------
# Plot clean does the following:
# - Strip lyrics of punctuation and symbols
# - Unnests tokens, and creates a word column
# - removes stop words from the tidytext::stop_words dataset
# - (Optionally) removes naughty words
#
# It does **not** filter to an artist
# -----------------------------------------------------------------
plot_clean <- function(df, filter_naughty = FALSE) {
  df %<>% 
    as_tibble() %>% 
    mutate(lyrics = gsub("\\[.*?\\]", "", lyrics)) %>%
    mutate(lyrics = gsub("\\(.*?\\)", "", lyrics)) %>%
    mutate(lyrics = gsub("\\\\", "", lyrics)) %>% 
    tidytext::unnest_tokens(input = lyrics, output = word) %>% 
    anti_join(tidytext::stop_words)
  
  if (filter_naughty == TRUE) {
    '%ni%' <- Negate('%in%')
    nsfw <- c("bitch", "bitches", "fuck", "fuckin", "shit", "damn",
              "pussy", "nigga", "niggas", "ass", "dick")
    df %<>% filter(word %ni% nsfw)
  }
  
  return(df)
}





#---------------------------------------------------------
# create plots creates chosen plots for a chosen rapper
# the default is to not save anything, and only display in R
#---------------------------------------------------------
create_plots <- function(
  df, 
  rapper,
  n = 10,
  save = FALSE,
  swcount = FALSE, 
  netsent = FALSE, 
  mcposneg = FALSE, 
  wcloud = FALSE, 
  compcloud = FALSE) {
  
  #======= WORD COUNT =======#
  if (swcount == TRUE) {
    return(simple_wordCount(df, rapper, n, save))
  }
  
  
  #======= SENTIMENT ANALYSIS =======#
  if (netsent == TRUE) {
    return(netSentiment(df, rapper, save))
  }
  
  
  #======= MOST COMMON POS AND NEG WORDS =======#
  if (mcposneg == TRUE) {
    return(mostCommonPosNegWords(df, rapper, save))
  }
  
  
  #======= WORD CLOUD =======#
  if (wcloud == TRUE) {
    return(wordCloud(df, rapper, save))
  }
  
  
  #======= COMPARISON CLOUD =======#
  if (compcloud == TRUE) {
    return(comparisonCloud(df, rapper, save))
  }
} # end function definition




# -------------------------------------------------------------------------
# Create a barplot of a n most occuring words in data; output in outputDir
# -------------------------------------------------------------------------
simple_wordCount <- function(data, rapper, n, save = FALSE) {
  
  data %<>% filter(artist == rapper)
  
  # Count unique words for the rapper
  word_count <- data %>%
    count(word, sort = TRUE)
  
  # Create a bar plot of first 50 most occurring words
  head <- word_count[1:n,]
  
  # Do some stuff to sort the x axis correctly
  head$word <- as.vector(head$word) #get rid of factors
  head$word = factor(head$word,head$word) #add ordered factors back
  
  
  plot <- 
    ggplot(head, aes(x = word, y = n)) +
    geom_bar(stat = "identity") +
    coord_flip()
  
  if (save == TRUE) {
    baseDir <- "/Users/zacharysnoek/Programming/r/rap-analyses/png/"
    wordCountDir <- paste0(baseDir, "word-count")
    savePlot(rapper, wordCountDir, plot)
  }
  
  return(plot)
}





# -------------------------------------------------------------------------
# Net sentiment of an artist's words
# -------------------------------------------------------------------------
netSentiment <- function(data, rapper) {
  
  data %<>% filter(artist == rapper)
  
  
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
      # baseDir <- "/Users/zacharysnoek/Programming/r/rap-analyses/png/"
      # netSentimentDir <- paste0(baseDir, "net-sentiment")
      
    },
    error = function(error_message) {
      message(error_message)
      return(NA)
    }
  ) 
}





# -------------------------------------------------------------------------
# Top 10 most common positive and negative words
# -------------------------------------------------------------------------
mostCommonPosNegWords <- function(data, rapper, save = FALSE) {
  
  data %<>% filter(artist == rapper)
  
  
  tryCatch({
    bing_word_counts <- data %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    plot <- 
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
    
    if (save == TRUE) {
      
      baseDir <- "/Users/zacharysnoek/Programming/r/rap-analyses/png/"
      mostCommonPosNegWordsDir <- paste0(baseDir, "most-common-pos-neg-words")
      savePlot(rapper, mostCommonPosNegWordsDir, plot)
    }
    return(plot)
  }, error = function(error_message) {
    message(error_message)
    return(NA)
  })
}





# -------------------------------------------------------------------------
# Word cloud of top 100 most used words
# -------------------------------------------------------------------------
wordCloud <- function(data, rapper, save = FALSE) {
  
  data %<>% filter(artist == rapper)
  
  
  plot <- 
    data %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))
  
  if (save == TRUE) {
    baseDir <- "/Users/zacharysnoek/Programming/r/rap-analyses/png/"
    wordCloudDir <- paste0(baseDir, "word-cloud")
    savePlot(rapper, wordCloudDir, plot)  
  }
  return(plot)
}





# -------------------------------------------------------------------------
# Comparison cloud of most positive and negative words
# -------------------------------------------------------------------------
comparisonCloud <- function(data, rapper, save = FALSE) {
  
  data %<>% filter(artist == rapper)
  
  
  tryCatch(
    {
      plot <- 
        data %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"),
                         max.words = 100)
      
      if (save == TRUE) {
        baseDir <- "/Users/zacharysnoek/Programming/r/rap-analyses/png/"
        comparisonCloudDir <- paste0(baseDir, "comparison-cloud")
        savePlot(rapper, comparisonCloudDir, plot)
      }
      return(plot)
    },
    error = function(error_message) {
      message(error_message)
      return(NA)
    }
  ) 
}





# -------------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------------
savePlot <- function(rapper, wd, plot) {
  filename < paste0(rapper, ".png")
  setwd(wd)
  ggsave(filename, plot = plot)
}

saveAsPDF <- function(rapper) {
  #TODO Figure out how to save as PNG
  filename < paste0(rapper, ".png")
  dev.print(pdf, filename)
  dev.off()
}