library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(viridis)

source("scripts/load_data.R")
theme_set(theme_bw())

df %>%
  select(id, od_factors) %>% 
  filter(!is.na(od_factors)) -> df_text

df_text %>% 
  unnest_tokens(word, od_factors, token = stringr::str_split, pattern = ", ") -> df_text

df_text

df_text %>% 
  pairwise_count(word, id, sort = TRUE) -> df_word_pairs

df_word_pairs %>% 
  filter(item1 == "heroin")

df_text %>% 
  #group_by(word) %>%
  filter(n() >= 100) %>%
  pairwise_cor(word, id, sort = TRUE) -> df_word_cors

df_word_cors %>% 
  filter(item1 == "heroin")

top_od_factors

df_word_cors %>%
  filter(item1 %in% c("heroin", "cocaine", "fentanyl", "alcohol")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)

df_word_cors %>%
  filter(abs(correlation) > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = abs(correlation), color = correlation), show.legend = TRUE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_label(aes(label = name), repel = FALSE) +
  scale_edge_alpha_continuous("Correlation", range = c(.5, 1)) +
  scale_edge_color_continuous(low = "blue", high = "red") +
  theme_graph()

