library(tidyverse)

load("data/googlenews.RData")

name_count <- googlenews %>% 
  count(name)

author_count <- googlenews %>% 
  count(author)

library(lubridate)

googlenews <- googlenews %>% 
  mutate(published=as_datetime(publishedAt),
         date=date(published),
         weekday=wday(published))

googlenews %>% 
  count(category, date) %>% 
  ggplot(aes(x=date, y=n, color=category)) + 
  geom_smooth(se=F) +
  ylim(0,400)

googlenews %>% 
  count(category, weekday) %>% 
  ggplot(aes(x=weekday, y=n, fill=category)) + 
  geom_col(position = "dodge")
  





