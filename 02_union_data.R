library(tidyverse)

pliki <- list.files("data")

news <- data.frame()

for(i in 1:length(pliki)){
  
  load(paste0("data/",pliki[i]))
  
  news <- union_all(news, news_all)
  
}

googlenews <- news %>%
  distinct()

# news 137557 obs.
# googlenews 19805 obs.
# 14% unique

save(googlenews, file="data/googlenews.RData")
