library(tidyverse)
library(udpipe)

load("data/googlenews.RData")

news_general <- googlenews %>% 
  filter(category == "general")

udpipe_download_model(language = "polish-lfg")

model <- udpipe_load_model(file = "polish-lfg-ud-2.4-190531.udpipe")

news_general$doc_id <- 1:nrow(news_general)

news_model <- udpipe_annotate(object = model, 
                              x = news_general$title, 
                              doc_id = news_general$doc_id)

news_model <- as.data.frame(news_model)

rake <- keywords_rake(news_model, "lemma", group = "doc_id",
                      relevant = news_model$upos %in% c("NOUN", "ADJ", "VERB"))


