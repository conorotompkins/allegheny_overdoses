library(tidyverse)
library(lubridate)
library(stringr)

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
  filter(overdose_factor != "Unknown") %>% 
  count(overdose_factor, sort = TRUE) %>% 
  top_n(10) -> od_factors
od_factors

df_long %>% 
  count(overdose_number, sort = TRUE)


df_long %>% 
  spread(overdose_number, overdose_factor) %>% 
  #group_by(id, date, death_time, year, yday, month, mday, wday, ) %>% 
  mutate(overdose_factors = str_c(combined_od1, 
                            combined_od2, 
                            combined_od3, 
                            combined_od4, 
                            combined_od5, 
                            combined_od6, 
                            combined_od7, sep = ", ")) %>% 
  select(-c(contains("combined_od"))) %>% 
  mutate(overdose_factors = str_replace_all(overdose_factors, "Unknown, ", ""),
         overdose_factors = str_replace_all(overdose_factors, ", Unknown", ""),
         od_heroin = str_detect(overdose_factors, "Heroin"),
         od_cocaine = str_detect(overdose_factors, "Cocaine"),
         od_fentanyl = str_detect(overdose_factors, "Fentanyl"),
         od_alcohol = str_detect(overdose_factors, "Alcohol"))  %>% 
  mutate(od_factor_count = rowSums(select(., contains("od_")))) -> df_wide
 

df_wide %>%
  select(starts_with("od_"))
  
?sum
%>% 
   -> df_wide
        
?str_replace
?rowwise
df_wide %>% 
  filter(od_heroin == TRUE)
?str_detect
