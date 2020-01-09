load("data/googlenews.RData")

summary(googlenews)

library(tidyverse)

news_general <- googlenews %>% 
  filter(category == "general")

cat_n <- googlenews %>% 
  count(category)

cat_n

googlenews %>% 
  count(name) %>% 
  arrange(desc(n))

googlenews %>% 
  filter(!is.na(author)) %>% 
  count(author) %>% 
  arrange(desc(n))

googlenews %>% 
  count(category, name) %>% 
  arrange(desc(n)) %>% 
  group_by(category) %>% 
  top_n(3, n)
