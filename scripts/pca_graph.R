library(broom)
library(ggfortify)

source("scripts/pca_munging.R")
options(scipen = 999)

df_long_pca <- df_long %>% 
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% select(-incident_zip), 
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

df_long_pca

var_exp <- df_long_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

var_exp

var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  mutate(pc = factor(pc, levels = unique(.$pc))) %>% 
  ggplot(aes(pc, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance",
       title = "Variance explained by each principal component")

df_long_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, label = TRUE,
                 label.label = "incident_zip",
                 label.repel = TRUE) +
        theme_bw() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA on 311 dataset")
    )
  ) %>%
  pull(pca_graph)
ggsave("images/od_factor_pca_graph.png")
