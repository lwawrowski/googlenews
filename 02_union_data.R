library(tidyverse)

pliki <- list.files("data")

pliki <- pliki[str_detect(pliki, "2020")]

news <- data.frame()

for(i in 1:length(pliki)){
  
  load(paste0("data/",pliki[i]))
  
  news <- union_all(news, news_all)
  
}

googlenews2020 <- news %>%
  distinct()

# news 137557 obs.
# googlenews 19805 obs.
# 14% unique

save(googlenews2020, file="data/googlenews2020.RData")
