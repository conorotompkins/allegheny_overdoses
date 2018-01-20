source("scripts/load_data.R")

library(scales)
library(viridis)

theme_set(theme_bw())

df %>% 
  mutate(year = as.factor(year)) -> df
  
df %>% 
  count(date) %>% 
  ggplot(aes(date, n)) +
  geom_col()

#zip code
#df %>% 
#  count(incident_zip, sort = TRUE) %>% 
#  ggplot(aes(incident_zip, n)) + 
#  geom_col()

df %>% 
  count(year, yday) %>%
  ggplot(aes(yday, n, color = year, fill =  year)) +
  geom_smooth(se = FALSE) +
  labs(y = "Fatal Overdoses",
       x = "Day of year")

df %>% 
  count(date) %>% 
  mutate(n_cumsum = cumsum(n)) %>% 
  ggplot(aes(date, n_cumsum)) +
  geom_line(size = 2) +
  labs(y = "Cumulative Sum of Fatal Overdoses")


df %>% 
  group_by(year, yday) %>% 
  summarize(n = n()) %>% 
  mutate(n_cumsum = cumsum(n)) -> df_cumsum

df %>% 
  group_by(year) %>% 
  summarize(yday = last(yday),
            date = last(date)) %>% 
  left_join(df_cumsum) -> df_tag

df_cumsum %>% 
  ggplot(aes(x = yday, y = n_cumsum, color = year)) +
    geom_line(size = 2) +
    geom_label(data = df_tag, aes(x = yday, y = n_cumsum, label = year, size = 3)) +
  labs(x = "Day of year",
       y = "Cumulative sum of fatal overdoses")
                   
df %>% 
  mutate(od_heroin = str_detect(od_factors, "Heroin"),
         od_cocaine = str_detect(od_factors, "Cocaine"),
         od_fentanyl = str_detect(od_factors, "Fentanyl"),
         od_alcohol = str_detect(od_factors, "Alcohol"),
         od_alprazolam = str_detect(od_factors, "Alprazolam"),
         od_oxycodone = str_detect(od_factors, "Oxycodone"),
         od_morphine = str_detect(od_factors, "Morphine"),
         od_methadone = str_detect(od_factors, "Methadone"),
         od_hydrocodone = str_detect(od_factors, "Hydrocodone")) -> df_factors


df_factors %>% 
  gather(od_factor, od_flag, starts_with("od_")) %>% 
  #gather(od_factor, od_flag, c(od_heroin, od_cocaine, od_fentanyl, od_alcohol)) %>% 
  filter(od_flag == TRUE) -> df_factors_long

df_factors_long %>% 
  group_by(od_factor, date) %>% 
  summarize(n = n()) %>% 
  group_by(od_factor) %>% 
  mutate(od_cumsum = cumsum(n)) -> df_factors_cumsum

df_factors_cumsum %>% 
  ggplot(aes(date, od_cumsum, color = od_factor)) +
  geom_line(size = 1.5) +
  labs(y = "Cumulative sum of fatal overdoses by factor")

#2017ncumsum by drug

df_factors_long %>% 
  filter(date >= "2017-01-01") %>% 
  group_by(od_factor, date) %>% 
  summarize(n = n()) %>% 
  group_by(od_factor) %>% 
  mutate(od_cumsum = cumsum(n)) -> df_factors_cumsum_2017

df_factors_cumsum_2017 %>% 
  ggplot(aes(date, od_cumsum, color = od_factor)) +
  geom_line(size = 2) +
  labs(title = "2017",
       y = "Cumulative sum of fatal overdoses by factor")

df %>% 
  #filter(date >= "2016-01-01") %>% 
  count(year, month, mday) %>%
  complete(year, month, mday = 1:31) %>% 
  replace_na(list(n = 0)) %>% 
  #filter(year %in% c("2016", "2017")) %>% 
  ggplot(aes(mday, month, fill = n)) +
  geom_tile() +
  facet_grid(year ~.) +
  scale_fill_viridis("Number of fatal overdoses") +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0),
                   limits = rev(levels(df$month)))


#over time, what % of heroin ODs contained fentanyl
#CJ - res

df_factors %>% 
  select(date, od_heroin) %>% 
  filter(od_heroin) %>% 
  count(date) %>% 
  mutate(n_cumsum = cumsum(n)) %>% 
  ggplot(aes(date, n_cumsum)) +
  geom_line() +
  labs(title = "Fatal overdoses involving heroin",
       x = "",
       y = "Cumulative sum of fatal overdoses")

df_factors %>% 
  select(date, od_heroin, od_fentanyl) %>% 
  filter(od_heroin) %>% 
  group_by(date) %>% 
  summarize(percent_fentanyl = mean(od_fentanyl)) -> df_factors_heroin_fent 

df_factors_heroin_fent %>% 
  #filter(date >= "2017-01-01") %>% 
  ggplot(aes(date, percent_fentanyl)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  labs(title = "Fatal heroin overdoses involving fentanyl",
       x = "",
       y = "% involving fentantyl")
