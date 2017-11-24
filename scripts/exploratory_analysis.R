source("scripts/load_data.R")

library(scales)
library(viridis)

theme_set(theme_bw())

df %>% 
  count(date) %>% 
  ggplot(aes(date, n)) +
  geom_col()

df %>% 
  mutate(year = as.factor(year)) %>% 
  count(year, yday) %>%
  ggplot(aes(yday, n, color = year, fill =  year)) +
  geom_smooth()

df %>% 
  count(date) %>% 
  mutate(n_cumsum = cumsum(n)) %>% 
  ggplot(aes(date, n_cumsum)) +
  geom_line()


df %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year, yday) %>% 
  summarize(n = n()) %>% 
  mutate(n_cumsum = cumsum(n)) -> df_cumsum

df %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year) %>% 
  summarize(yday = last(yday),
            date = last(date)) %>% 
  left_join(df_cumsum) -> df_tag

df_cumsum %>% 
  ggplot(aes(x = yday, y = n_cumsum, color = year)) +
    geom_line() +
    geom_label(data = df_tag, aes(x = yday, y = n_cumsum, label = year))
                   
df %>% 
  mutate(od_heroin = str_detect(overdose_factors, "Heroin"),
         od_cocaine = str_detect(overdose_factors, "Cocaine"),
         od_fentanyl = str_detect(overdose_factors, "Fentanyl"),
         od_alcohol = str_detect(overdose_factors, "Alcohol"),
         od_alprazolam = str_detect(overdose_factors, "Alprazolam"),
         od_oxycodone = str_detect(overdose_factors, "Oxycodone"),
         od_morphine = str_detect(overdose_factors, "Morphine"),
         od_methadone = str_detect(overdose_factors, "Methadone"),
         od_hydrocodone = str_detect(overdose_factors, "Hydrocodone")) -> df_factors


df_factors %>% 
  gather(od_factor, od_flag, starts_with("od_")) %>% 
  #gather(od_factor, od_flag, c(od_heroin, od_cocaine, od_fentanyl, od_alcohol)) %>% 
  filter(od_flag) -> df_factors

df_factors %>% 
  group_by(od_factor, date) %>% 
  summarize(n = n()) %>% 
  group_by(od_factor) %>% 
  mutate(od_cumsum = cumsum(n)) -> df_factors_cumsum

df_factors_cumsum %>% 
  ggplot(aes(date, od_cumsum, color = od_factor)) +
  geom_line()

df %>% 
  count(year, month, mday) %>%
  complete(year, month, mday = 1:31) %>% 
  replace_na(list(n = 0)) %>% 
  ggplot(aes(mday, month, fill = n)) +
  geom_tile() +
  facet_grid(year ~.) +
  scale_fill_viridis() +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))

#over time, what % of heroin ODs contained fentanyl
#CJ - res