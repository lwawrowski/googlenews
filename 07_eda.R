library(tidyverse)

load("data/googlenews.RData")

googlenews %>% 
  count(name) %>% 
  top_n(10, n) %>% 
  mutate(name=fct_reorder(name, n)) %>% 
  ggplot(aes(x=name, y=n)) +
    geom_col() +
    coord_flip()

author_count <- googlenews %>% 
  count(author)

library(lubridate)

googlenews <- googlenews %>% 
  mutate(published=as_datetime(publishedAt),
         date=date(published),
         weekday=wday(published))

googlenews %>% 
  count(date) %>% 
  ggplot(aes(x=date, y=n)) + 
  geom_line()

googlenews %>% 
  count(weekday) %>% 
  ggplot(aes(x=as.factor(weekday), y=n)) + 
  geom_col()
  





