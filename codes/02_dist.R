library(tidyverse)
library(stringdist)

load("data/kody.RData")

macierz <- stringdistmatrix(a = kody$odp, b = kody$odp, method = "jw")

colnames(macierz) <- kody$id
rownames(macierz) <- kody$id

macierz[!lower.tri(macierz)] <- 1000

mdf <- reshape2::melt(macierz) %>% 
  filter(value != 1000) %>% 
  rename(from=Var1, to=Var2)

library(circlize)

chordDiagram(mdf)

mdf_n <- mdf %>% 
  top_n(50, -value)

chordDiagram(mdf_n)

library(igraph)
library(ggraph)

network <- graph_from_data_frame(mdf_n)

ggraph(network, layout = "fr") +
  geom_edge_link(aes(width = value), edge_colour = "lightblue") +
  geom_node_text(aes(label = name)) +
  theme_graph()

load("data/nazwiska.RData")

nazwiska_200 <- nazwiska %>% 
  filter(liczba > 200)

macierz <- stringdistmatrix(a = nazwiska_200$nazwisko, 
                            b = nazwiska_200$nazwisko, 
                            method = "lv")

colnames(macierz) <- nazwiska_200$nazwisko
rownames(macierz) <- nazwiska_200$nazwisko

macierz[!lower.tri(macierz)] <- 1000

mdf <- reshape2::melt(macierz) %>% 
  filter(value != 1000) %>% 
  rename(from=Var1, to=Var2)

mdf_n <- mdf %>% 
  top_n(30, -value)

network <- graph_from_data_frame(mdf_n)

ggraph(network, layout = "fr") +
  geom_edge_link(aes(width = value), edge_colour = "lightblue") +
  geom_node_text(aes(label = name)) +
  theme_graph()

kowalski <- mdf %>% 
  filter(from == "KOWALSKI" | to == "KOWALSKI") %>% 
  top_n(10, -value)

mod1 <- mdf %>% 
  filter(value == 1)















