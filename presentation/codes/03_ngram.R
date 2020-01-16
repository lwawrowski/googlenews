library(tidyverse)
library(tidytext)

load("data/googlenews.RData")

news_general <- googlenews %>% 
  filter(category == "general")

pl_stop <- read_lines("data/polish_stopwords.txt")

titles <- news_general %>% 
  select(title) %>% 
  mutate(title_str = title %>% 
           str_replace_all("[^[:alnum:] ]", " ") %>% 
           str_squish()) %>% 
  unnest_tokens(output = words, input = title_str) %>% 
  filter(!words %in% pl_stop,
         is.na(as.numeric(words)),
         str_length(words) > 2)

titles_count <- titles %>% 
  count(words)

titles_count %>% 
  top_n(30, n) %>% 
  mutate(words = fct_reorder(words, n)) %>% 
  ggplot(aes(x=words, y=n)) + 
  geom_col() +
  coord_flip()

titles_count %>% 
  top_n(50, n) %>% 
  wordcloud2::wordcloud2(rotateRatio = 0)

# bigramy

titles2 <- news_general %>% 
  select(title) %>% 
  mutate(title_str = title %>% 
           str_replace_all("[^[:alnum:] ]", " ") %>% 
           str_squish()) %>% 
  unnest_tokens(output = words, input = title_str,
                token = "ngrams", n = 2) %>% 
  separate(col = words, into = c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% pl_stop,
         !word2 %in% pl_stop,
         is.na(as.numeric(word1)),
         is.na(as.numeric(word2)),
         str_length(word1) > 2,
         str_length(word2) > 2) %>% 
  mutate(words=str_c(word1, word2, sep = " "))

titles2_count <- titles2 %>% 
  count(words)

titles2_count %>% 
  top_n(30, n) %>% 
  mutate(words = fct_reorder(words, n)) %>% 
  ggplot(aes(x=words, y=n)) + 
  geom_col() +
  coord_flip()

titles2_count %>% 
  top_n(50, n) %>% 
  wordcloud2::wordcloud2(rotateRatio = 0)














