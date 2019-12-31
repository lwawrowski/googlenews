library(tidyverse)
library(udpipe)

# mod <- udpipe_download_model(language = "polish-pdb")
mod <- udpipe_download_model(language = "polish-lfg")

load("data/googlenews.RData")

news_general <- googlenews %>%
  filter(category == "general") %>% 
  mutate(unique_id = 1:nrow(.))

model <- udpipe_load_model(mod$file_model)

x <- udpipe_annotate(model, x = news_general$title, doc_id = news_general$unique_id)

x <- as.data.frame(x)

# części mowy

x %>%
  mutate(upos = fct_infreq(upos)) %>% 
  ggplot(aes(x=upos)) + 
  geom_bar() +
  coord_flip()

# rzeczownik

x %>%
  filter(upos == "NOUN") %>%
  count(token) %>% 
  filter(n > 40) %>%
  mutate(token = fct_reorder(token, n)) %>% 
  ggplot(aes(x=token, y=n)) + 
  geom_col() +
  coord_flip()

x %>%
  filter(upos == "NOUN") %>%
  count(lemma) %>% 
  filter(n > 50) %>%
  mutate(lemma = fct_reorder(lemma, n)) %>% 
  ggplot(aes(x=lemma, y=n)) + 
  geom_col() +
  coord_flip()

# przymiotnik

x %>%
  filter(upos == "ADJ") %>%
  count(token) %>% 
  filter(n > 20) %>%
  mutate(token = fct_reorder(token, n)) %>% 
  ggplot(aes(x=token, y=n)) + 
  geom_col() +
  coord_flip()

x_rok <- x %>% 
  filter(lemma == "rok")

# RAKE

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", relevant = x$upos %in% c("NOUN", "ADJ"))
stats <- keywords_rake(x = x, term = "token", group = "doc_id", relevant = x$upos %in% c("NOUN", "ADJ"))

stats %>%
  filter(ngram == 2) %>% 
  arrange(desc(freq))

# PMI

x$word <- tolower(x$token)

x_adj_noun <- x %>% 
  filter(upos %in% c("NOUN", "ADJ"))

stats <- keywords_collocation(x = x_adj_noun, term = "word", group = "doc_id", ngram_max = 4)

stats %>%
  arrange(desc(freq))

# graf

news_sub <- x %>% 
  filter(upos %in% c("NOUN", "PROPN", "VERB"))

cooc <- cooccurrence(x = news_sub, 
                     term = "token", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc, 50)

library(igraph)
library(ggraph)

wordnetwork <- head(cooc, 50)
wordnetwork <- graph_from_data_frame(wordnetwork)

ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4)


cooc <- cooccurrence(news_sub$token, skipgram = 1)

wordnetwork <- head(cooc, 50)
wordnetwork <- graph_from_data_frame(wordnetwork)

ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4)