library(tidyverse)
library(janitor)
library(lubridate)

url <- "https://data.wprdc.org/datastore/dump/1c59b26a-1684-4bfb-92f7-205b947530cf"
data <- read_csv(url, progress = FALSE)

glimpse(data)

df <- data %>%
  clean_names() %>% 
  mutate_at(vars(contains("combined")), as.character) %>% 
  mutate(id = row_number(),
         date = ymd(str_sub(death_date_and_time, 1, 10)),
         hour = as.numeric(str_sub(death_date_and_time, 11, 12)),
         #year = year(date),
         yday = yday(date),
         month = month(date, label = TRUE),
         mday = mday(date),
         week = week(date),
         wday = wday(date),
         incident_zip = as.character(incident_zip),
         decedent_zip = as.character(decedent_zip)) %>% 
  select(-death_date_and_time, year = case_year) %>% 
  arrange(date) %>% 
  select(id, date, year, yday, month, mday, week, wday, hour, everything()) %>% 
  drop_na(date)

glimpse(df)

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


df <- df %>% 
  mutate(od_factors = paste5(combined_od1,
                             combined_od2,
                             combined_od3,
                             combined_od4,
                             combined_od5,
                             combined_od6,
                             combined_od7, sep = ", ", na.rm = TRUE))

rm("data")

df <- df %>% 
  select(id, date, year, 
         yday, month, mday, wday, 
         manner_of_death, age, sex, race, 
         case_dispo, incident_zip, decedent_zip, od_factors, 
         combined_od1, combined_od2, combined_od3, combined_od4, 
         combined_od5, combined_od6, combined_od7)
