# Nietzsche Text Mining Project

# To begin I create a character vector of partial urls for each of Nietzsche's books. 
# They have been made available by the Gutenberg project (http://www.gutenberg.org/), 
# which is an online resource for free e-books.

nietz_url <- c("files/51356/51356-0", "cache/epub/5652/pg5652", "cache/epub/38226/pg38226", 
               "files/51935/51935-0", "files/37841/37841-0", "files/39955/39955-0", 
               "files/52881/52881-0", "files/1998/1998-0", "cache/epub/4363/pg4363", 
               "files/52319/52319-0", "files/52263/52263-0", "files/52166/52166-0", 
               "files/25012/25012-0", "files/52263/52263-0", "files/52190/52190-0", 
               "files/52914/52914-0", "files/52915/52915-0")


# An empty character vector is first defined that will hold the full urls for each book. 
# The 'make_urls' function makes the full url from the partial urls in 'nietz_url'. The 
# full urls are then then saved into 'nietz_book_urls'. 

urls <- vector()
make_urls <- function(url_temp){
  for (i in 1:length(url_temp)){
    temp_title <- paste("http://www.gutenberg.org/", url_temp[i], ".txt", sep="")
    urls <- rbind(urls, temp_title)
  }
  return(urls)
}
nietz_book_urls <- make_urls(nietz_url)

# The 'dplyr' package is imported to use the 'data_frame' function, as well as for other 
# features like piping (%>%), etc. 

# Each line of text for each book is contained in 'full_texts' within a single column, along 
# with a second column that contains the book title. The function 'read_books' scrapes each 
# text from the Gutenberg site, which is provided in .csv format. In some of the books the text 
# for a given line was spread across 2 columns, instead of 1. The function corrects for this by 
# gathering these columns into a single column through the if/else statement. Finally, the list 
# is converted into a data frame and saved into 'full_texts'. The 'read_books' function is then 
# called, and the output is saved into 'nietz_full_texts'. 

library(dplyr)
full_texts <- data.frame(text=character(), title=character())
read_books <- function(text_urls){
  for (i in 1:length(text_urls)){
    #print(i)
    temp_data <- read.csv(url(text_urls[i]), quote = "", row.names=NULL)
    
    if (ncol(temp_data)==1) {
      colnames(temp_data) <- "text"
    } else {
      colnames(temp_data) <- c("text1", "text2")
      temp_data$text <- paste(temp_data$text1, temp_data$text2, sep=" ")
    }
    temp_data <- data_frame(text = temp_data$text)
    full_texts <- rbind(full_texts, temp_data)
  }
  return(full_texts)
}
nietz_full_texts <- read_books(nietz_book_urls)

# Next we ensure that all non-Nietzsche text (i.e. translator introductions and forewords) 
# is excluded from our data set.  Given the inconsistency in text structure across the 
# individual books, the relevant text is selected with row numbers rather regular expressions, 
# with the book names assigned to a new column 'title'. Excluded text is labeled as 'Supplementary Texts'. 

nietz_rows <- as.numeric(rownames(nietz_full_texts))
nietz_full_texts$title <- 
  ifelse(nietz_rows >= 652 & nietz_rows <= 5817, "The Birth of Tragedy", 
  ifelse(nietz_rows >= 7695 & nietz_rows <= 15957, "Thoughts Out of Season: Part 1",
  ifelse(nietz_rows >= 16482 & nietz_rows <= 21295, "Thoughts Out of Season: Part 2", 
  ifelse(nietz_rows >= 21770 & nietz_rows <= 31013, "Human, All Too Human: Part 1",
  ifelse(nietz_rows >= 31426 & nietz_rows <= 39569, "Human, All Too Human: Part 2",
  ifelse(nietz_rows >= 40186 & nietz_rows <= 50115, "The Dawn of Day", 
  ifelse(nietz_rows >= 50577 & nietz_rows <= 60293, "The Joyful Wisdom",
  ifelse(nietz_rows >= 61266 & nietz_rows <= 72131, "Thus Spoke Zarathustra",
  ifelse(nietz_rows >= 74236 & nietz_rows <= 81292, "Beyond Good and Evil",
  ifelse(nietz_rows >= 81674 & nietz_rows <= 87977, "The Genealogy of Morals",
  ifelse(nietz_rows >= 88610 & nietz_rows <= 91934, "The Twilight of the Idols",
  ifelse(nietz_rows >= 91939 & nietz_rows <= 96155, "The Antichrist",
  ifelse(nietz_rows >= 97027 & nietz_rows <= 98372, "The Case of Wagner",
  ifelse(nietz_rows >= 98420 & nietz_rows <= 99601, "Nietzsche Contra Wagner",
  ifelse(nietz_rows >= 99680 & nietz_rows <= 101793, "We Philologists",
  ifelse(nietz_rows >= 114027 & nietz_rows <= 119452, "Ecce Homo",
  ifelse(nietz_rows >= 120049 & nietz_rows <= 128916, "The Will to Power: Books 1 & 2",
  ifelse(nietz_rows >= 129623 & nietz_rows <= 139729, "The Will to Power: Books 3 & 4", 
                                                      "Supplementary Texts"))))))))))))))))))

# Stop words are highly common words, such as 'the' and 'if', that are in many ways 
# uninformative for topic modeling and need to be removed. This is done below with a 
# list called 'stop_words' provided by the 'tidytext' library. I also define a list of 
# custom stop words for archaic English that is used in some of the translations of 
# Nietzsche's books. After that any remaining underscores that are still present due to 
# abnormal punctuation and spacing are removed. 

library(tidytext)
data(stop_words)
custom_stop_words <- data_frame(word = c("ye", "thou", "thee", "hast", "dost", "shalt", 
                                         "didst", "hath", "thy", "ni"))
nietz_full_texts$text <- gsub("_", "", nietz_full_texts$text)

# Now it is time to *tokenize* the words (i.e. make one word per row) with 'unnest_tokens' 
# while also filtering out stop words. The 'group_by' function (from dplyr) allows the grouping 
# of variables, such that operations are performed separately for given levels within a variable. 
# The 'mutate' function (also from dplyr) is then used to create a new variable (i.e. new column) 
# that contains the row number for each line of text. The text is then ungrouped, and the 'unnest_tokens' 
# function (from tidytext) places each word into a single row. And then we ensure that the supplementary 
# texts are excluded, as well as the stop words from 'stop_words' and 'custom_stop_words'. The data are 
# now in tidy format. 

nietz_tidy <- nietz_full_texts %>% group_by(title) %>% 
  mutate(line_number=row_number()) %>% ungroup(title) %>%
  unnest_tokens(word, text) %>%
  filter(title != "Supplementary Texts") %>%
  filter(!word %in% stop_words$word) %>%
  anti_join(custom_stop_words, by = "word")

# The 'wordcloud' library is loaded and the background and color scheme of the word cloud are defined. 
# The word frequency is tallied with the function 'count' (from dplyr). The top 100 most frequent words 
# are included in the plot. It reflects the most utilized words, apart from stop words, throughout all 
# of his books, and is a marker for the topics Nietzsche discusses in general. 

library(wordcloud)
par(bg="white")
pal2 <- brewer.pal(9,"Set1")

nietz_tidy %>%
  count(word) %>% 
  with(wordcloud(word, n, scale=c(5,.2),min.freq=2,max.words=100, 
                 random.order=FALSE, rot.per=.25, colors=pal2))

# Here the tf-idf is calculated for each word. This is a way of analyzing a term's uniqueness within each 
# individual book. 

# First a list 'nietz_book_words' is created that contains all of Nietzsche's words counted separately for 
# each book. Another variable 'nietz_total_words' is used to count the total number of words used within each 
# book. The 'total words' column from 'nietz_total_words' is then attached to 'nietz_book_words' via 'left_join'.

nietz_book_words <- nietz_tidy %>%
  count(title, word, sort = TRUE)

nietz_total_words <- nietz_book_words %>% 
  group_by(title) %>% summarize(total = sum(n)) %>%
  ungroup()

nietz_book_words <- left_join(nietz_book_words, nietz_total_words)

# The tf-idf is calculated separately for each word within each book using 'bind_tf_idf' from the 'tidytext' 
# library. After that the data are rearranged in 'nietz_book_words', such that the words with the highest 
# tf-idf appear at the top of the column. The 'word' column is also converted into a factor, with the levels 
# being equal to the reverse number of times a word appears (see 'rev'). After that the 6 books that I am interested 
# in are selected, with each book being filtered for the 15 words with the highest tf-idf.

nietz_book_words <- nietz_book_words %>%
  bind_tf_idf(word, title, n)

plot_nietz <- nietz_book_words %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_nietz_bar <- plot_nietz %>%
  filter(title=="Thoughts Out of Season: Part 1" | 
           title=="Thoughts Out of Season: Part 2" | 
           title=="The Dawn of Day" | 
           title=="We Philologists" | 
           title=="The Will to Power: Books 1 & 2" | 
           title=="The Will to Power: Books 3 & 4") %>%
  group_by(title) %>% top_n(15) %>% ungroup()

# Before plotting the data saved in 'plot_nietz_bar', 'ggplot2' is loaded for the upcoming visual graphics. 
# The 'fill' option makes the bars a different color for each book, and 'geon_bar' tells 'ggplot' to make 
# a bar graph (without a legend). The x and y variables are then labeled and 'facet_wrap' organizes the 6 
# plots into a single graphic. The x and y coordinates are then flipped so that the bars appear horizontal. 

library(ggplot2)

plot_nietz_bar %>% 
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Top 15 Most Unique Words", y = "Term Frequency-Inverse Document Frequency") +
  facet_wrap(~title, ncol = 4, scales = "free") +
  coord_flip()

# Next the lists 'wephil' and 'seaout' are created to hold text from the books 'We Philologists' 
# and 'Thoughts Out of Season: Part 2', respectively.

wephil <- nietz_full_texts %>% filter(title=="We Philologists")
seaout <- nietz_full_texts %>% filter(title=="Thoughts Out of Season: Part 2")

# A function 'make_book_sections' is then defined, which divides each book into 10 separate 
# (but equally sized) sections. The column 'section' is also created to indicate the 'section' 
# number for each line of text. 

make_book_sections <- function(input_txt){
  var1 <- input_txt %>% group_by(title) %>%
    mutate(line_number = row_number(), total_lines = n(), 
           line_proportion = line_number/total_lines) %>% ungroup()
  
  var2 <- ifelse(var1$line_proportion <= 0.1, "section_1", 
          ifelse(var1$line_proportion > 0.1 & var1$line_proportion <= 0.2, "section_2",
          ifelse(var1$line_proportion > 0.2 & var1$line_proportion <= 0.3, "section_3",
          ifelse(var1$line_proportion > 0.3 & var1$line_proportion <= 0.4, "section_4", 
          ifelse(var1$line_proportion > 0.4 & var1$line_proportion <= 0.5, "section_5",
          ifelse(var1$line_proportion > 0.5 & var1$line_proportion <= 0.6, "section_6", 
          ifelse(var1$line_proportion > 0.6 & var1$line_proportion <= 0.7, "section_7", 
          ifelse(var1$line_proportion > 0.7 & var1$line_proportion <= 0.8, "section_8", 
          ifelse(var1$line_proportion > 0.8 & var1$line_proportion <= 0.9, "section_9", 
          ifelse(var1$line_proportion > 0.9 & var1$line_proportion <= 1.0, "section_10","NA"))))))))))
  
  var1$section <- var2
  return(var1)
}

# The 'tidyr' library is loaded in order to use the 'unite' function. The 'make_book_sections' 
# function divides each book into 10 sections. The 'section' and 'title' variables are also 
# concatenated into a single variable called 'document'. 

library(tidyr)

by_section_wephil <- make_book_sections(wephil) %>%
  unite(document, title, section) %>% select(text, document)

by_section_seaout <- make_book_sections(seaout) %>%
  unite(document, title, section) %>% select(text, document)

# Here the text is tokenized and tidied, and then the stop words are removed. The words are 
# then counted and sorted from highest to lowest.

by_section_tidy_wephil <- by_section_wephil %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>% anti_join(custom_stop_words) %>%
  count(document, word, sort = TRUE) 

by_section_tidy_seaout <- by_section_seaout %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>% anti_join(custom_stop_words) %>%
  count(document, word, sort = TRUE) 

# Now a Document Term Matrix for both books must be created in order to conduct the LDA analysis.

sections_dtm_wephil <- by_section_tidy_wephil %>%
  cast_dtm(document, word, n)

sections_dtm_seaout <- by_section_tidy_seaout %>%
  cast_dtm(document, word, n)

# The 'Latent Dirichlet Allocation' (LDA) model is run with 'LDA' that comes from the library 
# 'topicmodels'. Ten (k) topics are specified for each book, and is seeded with 1234, rather 
# than a random number, for reproducibility purposes. 

library(topicmodels)

sections_lda_wephil <- LDA(sections_dtm_wephil, k = 10, control = list(seed = 1234))

sections_lda_seaout <- LDA(sections_dtm_seaout, k = 10, control = list(seed = 1234))

# The output of the LDA function, which was saved in the variables 'sections_lda_wephil' and 
# 'sections_lda_seaout', contains beta values for each word showing the degree to which each 
# word matches the given topic in which it is grouped. Now that we have beta values for each word, 
# we re-tidy the data, and then we find the words with the highest beta values within each of the 
# 10 topics that were generated by 'LDA'.  

top_terms_wephil <- tidy(sections_lda_wephil, matrix = "beta") %>%
  group_by(topic) %>% top_n(5, beta) %>% ungroup() %>% arrange(topic, -beta)

top_terms_seaout <- tidy(sections_lda_seaout, matrix = "beta") %>%
  group_by(topic) %>% top_n(5, beta) %>% ungroup() %>% arrange(topic, -beta)

# Finally, we plot the 5 largest beta values for each topic, which is done separately for both books. 
# Each book has 10 categories, and the 5 words that best reflect those categories are displayed together. 

top_terms_wephil %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) +
  labs(x = "Word", title = "We Philologists\n") +
  facet_wrap(~ topic, ncol=5, scales = "free") + coord_flip() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))

top_terms_seaout %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) +
  labs(x = "Word", title = "Thoughts Out of Season Part 2\n") + 
  facet_wrap(~ topic, ncol=5, scales = "free") + coord_flip() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))


