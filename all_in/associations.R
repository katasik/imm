library(tidyverse)
library(tidytext)
library(janitor)
library(widyr)
library(readxl)

assoc_raw <- read_xlsx("/Users/katasik/Desktop/suli/szakdoga/assoc_raw.xlsx")

assoc <- 
  assoc_raw %>% 
  rename(id = 1) %>% 
  janitor::clean_names() 

associations_by_id_en <- read_xlsx("/Users/katasik/Desktop/suli/szakdoga/associations_by_id_en.xlsx")

associations_by_id <-
  assoc %>% 
  select(id, matches("^kudarc\\d+|^kritika\\d+|^kihivas\\d+")) %>% 
  gather(assoc_order, word, -id) %>% 
  extract(assoc_order, 
          into = c("assoc", "order"), 
          regex = "(\\w+)(\\d+)",
          convert = TRUE)

rank_order <-    
  associations_by_id_en %>% 
  count(assoc, order, word) %>% 
  arrange(assoc, order, -n) %>% 
  
  
  word_attitudes <-
  bind_cols(
    assoc %>% 
      select(kudarc1:att_kihivas5, -starts_with("att_")) %>% 
      gather(variable, word) %>% 
      select(word),
    assoc %>% 
      select(starts_with("att_")) %>% 
      gather(variable, attitude) %>% 
      select(attitude)
  ) %>% 
  group_by(word) %>% 
  summarise(attitude_mean = mean(attitude, na.rm = TRUE))


ranking <- left_join(rank_order, word_attitudes, by = "word")

write.xlsx(ranking, file="ranking1.xlsx")

pair<- associations_by_id_en %>% 
  group_by(assoc) %>% 
  pairwise_count(
    item = word,
    feature = id,
    sort = TRUE,
    upper = FALSE
  ) %>% 
  ungroup() 

library(ggplot2)
install.packages("ggraph")
library(igraph)
library(ggraph)


library(dplyr)
install.packages("dplyr")

nested_pairs <- 
  pair %>% 
  group_nest(assoc) %>% 
  mutate(graph = map(data, ~graph_from_data_frame(.x)))

library(tidygraph)
nested_pairs <-
  pair %>% 
  filter(n >=4) %>% 
  group_nest(assoc) %>% 
  mutate(graph = map(data, ~.x %>%
                       as_tbl_graph()),
         plot = map(graph, 
                    ~.x %>% 
                      ggraph(layout="linear",circular = TRUE) +
                      geom_edge_link(aes(edge_alpha = n, 
                                         edge_width = n), 
                                     edge_colour = "olivedrab3") +
                      geom_node_point(size = 4) +
                      geom_node_text(aes(label = name),
                                     repel=TRUE,
                                     point.padding = unit(0.2, "lines")) +
                      theme_void()
         )
  )
nested_pairs[[1,"plot"]] 
nested_pairs[[2,"plot"]] 
nested_pairs[[3,"plot"]]

library(qdap)
library(xlsx)

freq<- read_xlsx("/Users/katasik/Documents/Kata/suli/szakdoga/associations_by_id_1.xlsx")
term_count_kihivas <- freq_terms(freq$kihivas, 20)
term_count_kritika <- freq_terms(freq$kritika, 20)
term_count_kudarc <- freq_terms(freq$kudarc, 20)

