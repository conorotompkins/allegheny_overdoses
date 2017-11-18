library(tidyverse)
library(lubridate)

rm(list = ls())
data_path <- "./data/overdoses"
data_list <- list.files(path = data_path, pattern = ".csv")
data_list <- paste0(data_path, "/", data_list)
data_list <- lapply(data_list, read_csv)

for(i in seq_along(data_list)){
  data_list[[i]]$`Incident Zip` <- as.character(data_list[[i]]$`Incident Zip`)
  data_list[[i]]$`Decedent Zip` <- as.character(data_list[[i]]$`Decedent Zip`)
}

data <- bind_rows(data_list)

colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub(" ", "_", colnames(data))

data %>%
  mutate(id = row_number(),
         date = mdy(death_date),
         year = year(date),
         yday = yday(date),
         month = month(date),
         mday = mday(date),
         wday = wday(date)) %>% 
  select(-c(case_year, death_date)) %>% 
  gather(overdose_number, overdose_factor, 
         -c(manner_of_death, age, sex, race, case_dispo, 
            id, incident_zip, decedent_zip, 
            date, year, yday, month, mday, wday, death_time)) %>% 
  arrange(date, death_time, overdose_number) %>% 
  select(id, date, death_time, year, yday, month, mday, wday, everything()) %>% 
  replace_na(list(overdose_factor = "Unknown"))  -> df_long
  
rm("data")

df_long %>% 
  count(overdose_factor, sort = TRUE) -> od_factors
od_factors


df_wide 