library(tidyverse)
library(stringi)

load("data/googlenews.RData")

table(googlenews$category)

tech <- googlenews %>% 
  filter(category == "technology")

stri_stats_general(tech$title[1])

stri_extract_all_words(tech$title[1])

microbenchmark::microbenchmark(
  str_dup("a", 1:1000),
  stri_dup("a", 1:1000),
  unit="relative",
  times=10
  )
