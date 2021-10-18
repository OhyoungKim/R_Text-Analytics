# Load packages
library(pdftools)
library(stringr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(ggplot2)


# Import all PDF files from the same folder
setwd("C:/wd/HULT/Text Analytics and NLP/individual")
nm <- list.files(path="C:/wd/HULT/Text Analytics and NLP/individual")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) paste(pdf_text(x), collapse = " "))) #1 variable
colnames(my_pdf_text) <- c("text")
mydf <- data.frame(line=1:6, text = my_pdf_text[,1])

# Tokenize the mydf dataframe
token_list <- mydf %>% unnest_tokens(word, text)
# Check the result
#print(token_list)

# Check the token frequencies
frequencies_tokens <- mydf %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort=TRUE)
# Check the result
print(frequencies_tokens)

# Remove meaningless words: numbers, units, auxiliary verbs and etc.
mystopwords <- data_frame(word = c("19","cent","months","percent","50","pre","2020","we’re","1", "2", "25", "60",
                                   "70","a3","10","100","30","5","85","11","20","2019","3","36","40","90","a1",
                                   "what's","1.1","180","35","4","400","66","750","three","month","was","during",
                                   "you","now","those"))
clean_frequencies_tokens <- anti_join(frequencies_tokens, mystopwords, by = "word")

#############
# Look at the graphical approach:
freq_hist <- clean_frequencies_tokens %>%
    filter(n > 8) %>% # we need this to eliminate all the low count words
    mutate(word=reorder(word, n)) %>%
    ggplot(aes(word, n))+
    geom_col()+
    labs(x = NULL, y = "frequencies")+
    coord_flip()
print(freq_hist)
#word: home, economy, stay, airbnb, safety, time, asia, weekend




#######################################
#####      TF-IDF framework       #####
#######################################
each_words <- mydf %>%
    unnest_tokens(word, text) %>%
    count(line, word, sort = TRUE) %>%
    ungroup()

each_words <- each_words %>%
    bind_tf_idf(word, line, n)

# Remove meaningless words: numbers, units, auxiliary verbs and etc.
each_words <- anti_join(each_words, mystopwords, by = "word")

# Check the result
each_words %>%
    arrange(desc(tf_idf))

#############
# Look at the graphical approach:
each_words %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    group_by(line) %>%
    top_n(9) %>%
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = line)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~line, ncol = 2, scales = "free") +
    coord_flip()




#######################################
#####     Sentiment analysis      #####
#######################################

token_words <- mydf %>%
    unnest_tokens(word, text) %>%
    count(line, word, sort = TRUE) %>%
    ungroup()

# Remove meaningless words: numbers, units, auxiliary verbs and etc.
token_words <- anti_join(token_words, mystopwords, by = "word")

# Get sentiments based on NRC lexicon
token_words_sentiments <- token_words %>%
    inner_join(get_sentiments("nrc"), by = "word")

#############
# Look at the graphical approach:
token_words_sentiments %>%
    rename(
        document = line,
        term = word,
        count = n,
        sentiment = sentiment
    ) %>%
    count(sentiment, term, wt = count) %>%
    ungroup() %>%
    filter(n >= 5) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(n, term, fill = sentiment)) +
    geom_col() +
    labs(x = "Contribution to sentiment", y = NULL)
