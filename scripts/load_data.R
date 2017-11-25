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
         month = month(date, label = TRUE),
         mday = mday(date),
         wday = wday(date)) %>% 
  select(-c(case_year, death_date)) %>% 
  arrange(date, death_time) %>% 
  select(id, date, death_time, year, yday, month, mday, wday, everything()) -> df

df %>% 
  gather(od_number, od_factor, combined_od1, combined_od2, combined_od3, combined_od4, combined_od5, combined_od6, combined_od7) %>% 
  select(od_number, od_factor) %>% 
  count(od_factor, sort = TRUE) -> top_od_factors


#function from https://stackoverflow.com/questions/13673894/suppress-nas-in-paste/31508774#31508774
paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}


df %>% 
  mutate(od_factors = paste5(combined_od1,
                             combined_od2,
                             combined_od3,
                             combined_od4,
                             combined_od5,
                             combined_od6,
                             combined_od7, sep = ", ", na.rm = TRUE)) -> df

rm("data")

df %>% 
  select(id, date, death_time, year, 
         yday, month, mday, wday, 
         manner_of_death, age, sex, race, 
         case_dispo, incident_zip, decedent_zip, od_factors, 
         combined_od1, combined_od2, combined_od3, combined_od4, 
         combined_od5, combined_od6, combined_od7) -> df