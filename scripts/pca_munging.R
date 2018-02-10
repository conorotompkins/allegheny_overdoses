source("scripts/load_data.R")

df

df %>%
  select(id, incident_zip, starts_with("od_")) %>%
  filter(!is.na(incident_zip)) %>% 
  gather(key = od_factor, value = od_flag, starts_with("od_")) %>%
  separate(od_flag, sep = ", ", into = str_c("od_", 1:8)) %>% 
  select(-od_factor) %>% 
  gather(key = od_factor_number, value = od_factor, -c(id, incident_zip)) %>% 
  arrange(id, od_factor_number) %>% 
  filter(!is.na(od_factor)) -> df_long

df_long %>% 
  count(od_factor, sort = TRUE) %>% 
  top_n(10) -> top_od_factors

df_long %>% 
  semi_join(top_od_factors) %>% 
  group_by(incident_zip, od_factor) %>% 
  summarize(n = n()) %>% 
  complete(incident_zip, od_factor = unlist(top_od_factors$od_factor)) %>% 
  replace_na(replace = list(n = 0)) -> df_long
  
df_long %>% 
  group_by(incident_zip) %>% 
  mutate(incident_zip_total = sum(n)) -> df_long

df_long %>% 
  ungroup() %>% 
  mutate(od_factor_percentage = n / incident_zip_total) -> df_long

df_long %>% 
  filter(is.na(od_factor_percentage))

df_long %>% 
  filter(is.nan(od_factor))

df_long %>% 
  select(incident_zip, od_factor, od_factor_percentage) %>% 
  spread(od_factor, od_factor_percentage) %>% 
  ungroup() -> df_long

