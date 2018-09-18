#Need to adjust for per capita (per 100,000)

source("scripts/load_data.R")

library(scales)
library(viridis)
library(ggrepel)
library(scales)
library(knitr)
library(kableExtra)

theme_set(theme_bw(base_size = 18))

ct_caption = "Conor Tompkins, @conor_tompkins"
ct_subtitle = "Data from the WPRDC"
ct_title = "Demographics of overdoses in Allegheny County"

df %>% 
  select(id, date, year, age, sex, race) %>%
  filter(date <= "2017-09-01") %>% 
  mutate(category = str_c(sex, race, sep = ", ")) -> df_demo

df_demo %>% 
  head() %>% 
  kable("html") %>% 
  kable_styling()

df_demo %>% 
  count(race, sort = TRUE) %>% 
  head() %>% 
  kable("html") %>% 
  kable_styling()

df_demo %>% 
  filter(race %in% c("White", "Black")) %>% 
  ggplot(aes(age, fill = race)) +
  geom_density(alpha = .75) +
  labs(title = ct_title,
       subtitle = ct_subtitle,
       x = "Age",
       y = "",
       caption = ct_caption) +
  scale_fill_discrete("Race")

df_demo %>% 
  ggplot(aes(age, fill = sex)) +
  geom_density(alpha = .75) +
  labs(title = ct_title,
       subtitle = ct_subtitle,
       x = "Age",
       y = "",
       caption = ct_caption) +
  scale_fill_discrete("Sex")

df_demo %>% 
  count(category, sort = TRUE) %>% 
  head() %>% 
  kable("html") %>% 
  kable_styling()

df_demo %>% 
  filter(race %in% c("White", "Black")) %>% 
  ggplot(aes(age, fill = category)) +
  geom_density(alpha = .5) +
  facet_wrap(~category, nrow = 1) +
  labs(title = ct_title,
       subtitle = ct_subtitle,
       x = "Age",
       y = "",
       caption = ct_caption) +
  scale_fill_discrete("")

df_demo %>% 
  filter(race %in% c("White", "Black")) %>% 
  arrange(category, date) %>% 
  count(category, date) %>% 
  group_by(category) %>% 
  mutate(cum_sum = cumsum(n)) -> df_demo_cumulative

df_demo %>% 
  filter(race %in% c("White", "Black")) %>% 
  group_by(category) %>% 
  summarize(total = n(),
            last_date = last(date)) -> df_demo_cumulative_tag

ggplot(data = df_demo_cumulative, aes(x = date, y = cum_sum, color = category)) +
  geom_line(size = 2) +
  geom_point(data = df_demo_cumulative_tag, aes(x = last_date, y = total), size = 3) +
  geom_label_repel(data = df_demo_cumulative_tag, aes(x = last_date, y = total, color = category, label = category),
                   nudge_x = 3) +
  scale_y_continuous(label = comma) +
  scale_color_discrete(guide = FALSE) +
  labs(title = ct_title,
       subtitle = ct_subtitle,
       x = "",
       y = "Cumulative sum of overdoses",
       caption = ct_caption)

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
  labs(title = ct_title,
       subtitle = ct_subtitle,
       y = "Cumulative sum of fatal overdoses",
       caption = "Conor Tompkins, @conor_tompkins \nNot exclusive")
