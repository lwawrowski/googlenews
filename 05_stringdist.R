library(tidyverse)
library(stringdist)

kody <- readxl::read_xlsx("data/kody.xlsx") %>% 
  mutate(id=stri_join(id,"_",grupa)) %>% 
  select(-grupa)

save(kody, file = "data/kody.RData")

macierz <- stringdistmatrix(a = kody$odp, b = kody$odp, method = "jw")

colnames(macierz) <- row.names(macierz) <- kody$id

macierz[lower.tri(macierz)] <- Inf

mdf <- reshape2::melt(macierz)

mdf_unique <- mdf %>% 
  rename(from=Var1, to=Var2) %>% 
  filter(value != Inf) %>% 
  filter(from != to)

mdf_unique_graph <- mdf_unique %>% 
  top_n(50, -value)

wordnetwork <- graph_from_data_frame(mdf_unique_graph)

ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = value, edge_alpha = value), edge_colour = "lightblue") +
  geom_node_text(aes(label = name), col = "black", size = 4) +
  theme_graph()

library(ggalluvial)

mdf_unique %>% 
  top_n(20, -value) %>% 
  ggplot(aes(y = value, axis1 = from, axis2 = to)) +
  geom_alluvium(aes(fill = value)) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE)

library(circlize)

mdf_unique %>% 
  top_n(20, -value) %>% 
  chordDiagram(.)

# nazwiska

nazwiska <- readxl::read_xlsx("data/nazwiska_slask.xlsx")

save(nazwiska, file = "data/nazwiska.RData")

nazwiska <- nazwiska %>% 
  filter(liczba > 200)

macierz_nazw <- stringdistmatrix(a = nazwiska$nazwisko, b = nazwiska$nazwisko, method = "jw")

colnames(macierz_nazw) <- row.names(macierz_nazw) <- nazwiska$nazwisko

macierz_nazw[lower.tri(macierz_nazw)] <- Inf

mdf <- reshape2::melt(macierz_nazw)

mdf_unique <- mdf %>% 
  rename(from=Var1, to=Var2) %>% 
  filter(value != Inf) %>% 
  filter(from != to)

mdf_unique %>% 
  top_n(20, -value) %>% 
  chordDiagram(.)

mdf_unique %>% 
  top_n(20, value) %>%
  mutate(from=fct_reorder(from, value)) %>% 
  ggplot(aes(y = value, axis1 = from, axis2 = to)) +
  geom_alluvium(aes(fill = value), color = "black") +
  geom_stratum(width = 1/8, fill = "black", color = "white") +
  geom_label(stat = "stratum", infer.label = TRUE)

kowalski <- mdf_unique %>% 
  filter(from == "KUBICA")

mdf_unique_graph <- mdf_unique %>% 
  top_n(50, -value)

wordnetwork <- graph_from_data_frame(mdf_unique_graph)

ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = value, edge_alpha = value), edge_colour = "lightblue") +
  geom_node_text(aes(label = name), col = "black", size = 4) +
  theme_graph()


