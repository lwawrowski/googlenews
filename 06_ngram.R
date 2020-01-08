library(tidyverse)
library(stringi)
library(tidytext)
library(wordcloud2)

load("data/googlenews.RData")

table(googlenews$category)

tech <- googlenews %>% 
  filter(category == "technology")

stopwords <- read_lines("data/polish_stopwords.txt")

titles <- tech %>% 
  select(title) %>% 
  mutate(title_str = title %>% 
           str_replace_all("[^[:alnum:] ]", " ") %>% 
           str_squish())

words <- titles %>% 
  unnest_tokens(words, title_str) %>% 
  # count(title, words) %>% 
  filter(!(words %in% stopwords),
         is.na(as.numeric(words)),
         str_length(words)>2)

words_cnt <- words %>% 
  count(words)

words_cnt_50 <- words_cnt %>% 
  filter(n > 20)

wordcloud2(words_cnt_50, rotateRatio = 0)

desc <- tech %>% 
  select(description) %>% 
  mutate(desc_str = description %>% 
           str_replace_all("[^[:alnum:] ]", " ") %>% 
           str_squish())

words <- desc %>% 
  unnest_tokens(words, desc_str) %>% 
  count(description, words) %>% 
  filter(!(words %in% stopwords),
         is.na(as.numeric(words)),
         str_length(words)>2)

words_cnt <- words %>% 
  count(words)

words_cnt_n <- words_cnt %>% 
  filter(n > 30) %>% 
  mutate(words = fct_reorder(words, n))

wordcloud2(words_cnt_n, rotateRatio = 0)

ggplot(words_cnt_n, aes(x=words, y=n)) + 
  geom_col() +
  coord_flip()

# bigramy

titles_2gram <- titles %>% 
  select(title_str) %>% 
  unnest_tokens(word, title_str, token = "ngrams", n = 2, drop = FALSE) %>% 
  select(-title_str) %>% 
  separate(word, c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(!word1 %in% stopwords) %>%
  filter(is.na(as.numeric(word1))) %>%
  filter(str_length(word1) > 2) %>%
  filter(!word2 %in% stopwords) %>%
  filter(is.na(as.numeric(word2))) %>%
  filter(str_length(word2) > 2) %>%
  select(-word1, -word2)

titles_2gram_cnt <- titles_2gram %>% 
  count(word)

# desc

desc_2gram <- desc %>% 
  select(desc_str) %>% 
  filter(!is.na(desc_str)) %>% 
  unnest_tokens(word, desc_str, token = "ngrams", n = 2, drop = FALSE) %>% 
  select(-desc_str) %>% 
  separate(word, c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(!word1 %in% stopwords) %>%
  filter(is.na(as.numeric(word1))) %>%
  filter(str_length(word1) > 2) %>%
  filter(!word2 %in% stopwords) %>%
  filter(is.na(as.numeric(word2))) %>%
  filter(str_length(word2) > 2) %>%
  select(-word1, -word2)

desc_2gram_cnt <- desc_2gram %>% 
  count(word)
