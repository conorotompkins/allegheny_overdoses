source("scripts/load_data.R")

library(scales)
library(viridis)
library(ggrepel)

theme_set(theme_bw())

df %>% 
  mutate(year = as.factor(year)) -> df
  
#zip code
#df %>% 
#  count(incident_zip, sort = TRUE) %>% 
#  ggplot(aes(incident_zip, n)) + 
#  geom_col()


#identify cutoff date for 2017
#cumulative sum
df %>% 
  filter(year == 2017) %>% 
  count(date) %>% 
  mutate(n_cumsum = cumsum(n)) %>% 
  ggplot(aes(date, n_cumsum)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = ymd("2017-09-01")) +
  geom_line(size = 2) +
  labs(y = "Cumulative Sum of Fatal Overdoses")

df %>% 
  filter(date <= "2017-09-01") %>% 
  count(date) %>% 
  mutate(n_cumsum = cumsum(n)) %>% 
  ggplot(aes(date, n_cumsum)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 2) +
  labs(y = "Cumulative Sum of Fatal Overdoses")


df %>%
  count(incident_zip, sort = TRUE)

#old, delete
#df_year %>%
#  ggplot(aes(yday, n, color = year, fill =  year)) +
#  geom_smooth(se = FALSE) +
#  #scale_y_continuous(limits = c(0, 3)) +
#  labs(y = "Fatal Overdoses",
#       x = "Day of year")

#time series EDA

df %>% 
  filter(date <= "2017-09-01") -> df

#year graph with labels
df %>% 
  count(year, yday) -> df_year

df %>% 
  count(year, yday) %>% 
  group_by(year) %>% 
  mutate(smooth = predict(loess(n ~ yday))) %>% 
  summarize(last_yday = last(yday),
            last_smooth = last(smooth),
            tag = unique(year)) -> df_year_label

ggplot(data = df_year, aes(x = yday, y = n, color = year)) +
  geom_smooth(se = FALSE) +
  geom_label_repel(data = df_year_label, aes(x = last_yday, y = last_smooth, label = year)) +
  geom_point(data = df_year_label, aes(x = last_yday, y = last_smooth), size = 3) +
  scale_color_discrete(guide = FALSE) +
  labs(title = "Fatal overdoses in Allegheny County",
       x = "Day of the year",
       y = "Number of overdoses")


#cumulative sum
df %>% 
  count(date) %>% 
  mutate(n_cumsum = cumsum(n)) %>% 
  ggplot(aes(date, n_cumsum)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 2) +
  labs(y = "Cumulative Sum of Fatal Overdoses")


#fatal overdoses by zip code
top_zips <- df %>% 
  count(incident_zip, sort = TRUE) %>% 
  top_n(5) %>% 
  select(incident_zip) %>% 
  unlist()
top_zips

zips_df <- df %>% 
  arrange(incident_zip, date) %>% 
  count(incident_zip, date) %>% 
  #mutate(is_top_zip = incident_zip %in% top_zips) %>% 
  group_by(incident_zip) %>% 
  mutate(n_cumsum = cumsum(n))

top_zips_df <- zips_df %>% 
  filter(incident_zip %in% top_zips)

top_zips_df_labels <- df %>% 
  filter(incident_zip %in% top_zips) %>% 
  group_by(incident_zip) %>% 
  summarize(last_date = last(date),
            tag = unique(incident_zip),
            total = n())

ggplot(zips_df, aes(x = date, y = n_cumsum, group = incident_zip)) +
  geom_line(size = 1, alpha = .1) +
  geom_line(data = top_zips_df, aes(x = date, y = n_cumsum, color = incident_zip), size = 2) +
  geom_label_repel(data = top_zips_df_labels, aes(x = last_date, y = total, label = tag, color = incident_zip)) +
  geom_point(data = top_zips_df_labels, aes(x = last_date, y = total, color = incident_zip), size = 3) +
  labs(y = "Cumulative Sum of Fatal Overdoses") +
  scale_alpha_manual(values = c(.1, 1), guide = FALSE) +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(expand = c(.01, .1)) +
  labs(title = "Fatal overdoses per zip code, cumulative",
       x = NULL)

#fatal overdoses by year
df %>% 
  group_by(year, yday) %>% 
  summarize(n = n()) %>% 
  mutate(n_cumsum = cumsum(n)) -> df_year_cumsum

df %>% 
  group_by(year) %>% 
  summarize(last_yday = last(yday),
            total = n()) -> df_year_tag

ggplot(data = df_year_cumsum, aes(x = yday, y = n_cumsum, color = year)) +
  geom_line(size = 2) +
  geom_label_repel(data = df_year_tag, aes(x = last_yday, y = total, label = year, color = year, group = year)) +
  geom_point(data = df_year_tag, aes(x = last_yday, y = total, color = year, group = year), size = 3) +
  scale_size_continuous(guide = FALSE) +
  scale_color_discrete(guide = FALSE) +
  labs(x = "Day of year",
       y = "Cumulative sum of fatal overdoses")

#identify top 10 OD factors
df %>%
  select(id, date, starts_with("od_")) %>% 
  gather(od_factor, od_flag, starts_with("od_"))


df_factor_total <- tibble(factor = c(df$combined_od1,
                 df$combined_od2,
                 df$combined_od3,
                 df$combined_od4,
                 df$combined_od5,
                 df$combined_od6,
                 df$combined_od7))

df_factor_total <- df_factor_total %>% 
  filter(!is.na(factor)) %>% 
  count(factor, sort = TRUE)


df %>% 
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
  gather(od_factor, od_flag, starts_with("od_")) %>% 
  #gather(od_factor, od_flag, c(od_heroin, od_cocaine, od_fentanyl, od_alcohol)) %>% 
  filter(od_flag == TRUE) -> df_factors_long

#create od_factor df
df_factors_long %>% 
  group_by(od_factor, date) %>% 
  summarize(n = n()) %>% 
  group_by(od_factor) %>% 
  mutate(od_cumsum = cumsum(n)) -> df_factors_cumsum

#create label df for od_factors
df_factors_long %>% 
  group_by(od_factor) %>% 
  summarize(last_date = last(date),
            total = n()) -> df_factors_cumsum_label

#plot cumulative od_factor
ggplot(data = df_factors_cumsum, aes(x = date, y = od_cumsum, color = od_factor)) +
  geom_line(size = 1.5) +
  geom_label_repel(data = df_factors_cumsum_label, aes(x = last_date, y = total, label = str_replace(od_factor, "od_", ""), group = od_factor)) +
  geom_point(data = df_factors_cumsum_label, aes(x = last_date, y = total, group = od_factor, color = od_factor), size = 3) +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(expand = c(.01, .1)) +
  labs(y = "Cumulative sum of fatal overdoses by overdose factor",
       caption = "Not exclusive")

#2017ncumsum by drug

df_factors_long %>% 
  filter(date >= "2017-01-01") %>% 
  group_by(od_factor, date) %>% 
  summarize(n = n()) %>% 
  group_by(od_factor) %>% 
  mutate(od_cumsum = cumsum(n)) -> df_factors_cumsum_2017

df_factors_long %>% 
  filter(date >= "2017-01-01") %>% 
  group_by(od_factor) %>% 
  summarize(last_date = last(date),
            total = n()) -> df_factors_cumsum_label_2017

df_factors_cumsum_2017 %>% 
  ggplot(aes(date, od_cumsum, color = od_factor)) +
  geom_line(size = 2) +
  geom_label_repel(data = df_factors_cumsum_label_2017, aes(x = last_date, y = total, label = str_replace(od_factor, "od_", ""), group = od_factor)) +
  geom_point(data = df_factors_cumsum_label_2017, aes(x = last_date, y = total, group = od_factor, color = od_factor), size = 3) +
  scale_color_discrete(guide = FALSE) +
  scale_y_continuous(expand = c(.01, .1)) +
  labs(y = "Cumulative sum of fatal overdoses by overdose factor")

#create tile df
df %>% 
  #filter(date >= "2016-01-01") %>% 
  group_by(year, month, mday) %>%
  summarize(n = n()) %>% 
  ungroup() %>% 
  complete(year, month, mday = 1:31) %>% 
  replace_na(list(n = 0)) -> df_tile

#hack the data so NA and 0s aren't combined
df_tile$n[df_tile$year == 2017 & df_tile$month == "Sep" & df_tile$mday >= 1] <- NA
df_tile$n[df_tile$year == 2017 & df_tile$month %in% c("Oct", "Nov", "Dec")] <- NA
#September, April, June, and November have 30 days
months_30 <- c("Sep", "Apr", "Jun", "Nov")
df_tile$n[df_tile$month %in% months_30 & df_tile$mday == 31] <- NA

df_tile %>% 
  ggplot(aes(mday, month, fill = n)) +
  geom_tile() +
  facet_grid(year ~.) +
  scale_fill_viridis("Number of fatal overdoses") +
  coord_equal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0),
                   limits = rev(levels(df$month))) +
  labs(y = NULL,
       x = "Day of month")

#over time, what % of heroin ODs contained fentanyl
#CJ - res

df_factors %>% 
  select(date, od_heroin) %>% 
  filter(od_heroin) %>% 
  count(date) %>% 
  mutate(n_cumsum = cumsum(n),
         tag = "Heroin") %>% 
  ggplot(aes(x = date, y = n_cumsum)) +
  geom_line(size = 2) +
  #geom_label_repel(aes(x = last(date), label = tag)) + not working
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
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Fatal heroin overdoses involving fentanyl",
       x = "",
       y = "% involving fentantyl")