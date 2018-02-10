source("scripts/load_data.R")

library(scales)
library(viridis)
library(ggrepel)
library(scales)

theme_set(theme_bw(base_size = 18))

df %>% 
  select(id, date, year, age, sex, race) %>% 
  filter(date <= "2017-09-01")-> df_demo
df_demo

df_demo %>% 
  count(race, sort = TRUE)

df_demo %>% 
  filter(race %in% c("White", "Black")) %>% 
  ggplot(aes(age, fill = race)) +
  geom_density(alpha = .75)

df_demo %>% 
  ggplot(aes(age, fill = sex)) +
  geom_density(alpha = .75)

df_demo %>% 
  filter(race %in% c("White", "Black")) %>% 
  ggplot(aes(age)) +
  geom_density(alpha = .75) +
  facet_wrap(sex~race)

df_demo %>% 
  filter(race %in% c("White", "Black")) %>% 
  mutate(category = str_c(sex, race, sep = ", ")) %>% 
  arrange(category, date) %>% 
  count(category, date) %>% 
  group_by(category) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  ggplot(aes(date, cum_sum, color = category)) +
  geom_line(size = 2) +
  scale_y_continuous(label = comma)



#drug factor by category (sex, race)

df %>% 
  filter(race %in% c("White", "Black")) %>% 
  mutate(od_heroin = str_detect(od_factors, "Heroin"),
         od_cocaine = str_detect(od_factors, "Cocaine"),
         od_fentanyl = str_detect(od_factors, "Fentanyl"),
         od_alcohol = str_detect(od_factors, "Alcohol"),
         od_alprazolam = str_detect(od_factors, "Alprazolam"),
         od_oxycodone = str_detect(od_factors, "Oxycodone"),
         od_morphine = str_detect(od_factors, "Morphine"),
         od_methadone = str_detect(od_factors, "Methadone"),
         od_hydrocodone = str_detect(od_factors, "Hydrocodone"),
         od_diazepam = str_detect(od_factors, "Diazepam")) -> df_factors


df_factors %>%
  mutate(category = str_c(sex, race, sep = ", ")) %>% 
  gather(od_factor, od_flag, starts_with("od_"), -category) %>% 
  #gather(od_factor, od_flag, c(od_heroin, od_cocaine, od_fentanyl, od_alcohol)) %>% 
  filter(od_flag == TRUE) -> df_factors_long

#create od_factor df
df_factors_long %>% 
  group_by(category, od_factor, date) %>% 
  summarize(n = n()) %>% 
  group_by(category, od_factor) %>% 
  mutate(od_cumsum = cumsum(n)) -> df_factors_cumsum

#create label df for od_factors
df_factors_long %>% 
  group_by(category, od_factor) %>% 
  summarize(last_date = last(date),
            total = n()) -> df_factors_cumsum_label

#plot cumulative od_factor
ggplot(data = df_factors_cumsum, aes(x = date, y = od_cumsum, color = od_factor)) +
  geom_line(size = 1.5) +
  geom_label_repel(data = df_factors_cumsum_label, aes(x = last_date, y = total, label = str_replace(od_factor, "od_", ""), group = od_factor)) +
  geom_point(data = df_factors_cumsum_label, aes(x = last_date, y = total, group = od_factor, color = od_factor), size = 3) +
  facet_wrap(~category) +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(expand = c(.01, .1), label = comma) +
  labs(y = "Cumulative sum of fatal overdoses by overdose factor",
       caption = "Not exclusive")
