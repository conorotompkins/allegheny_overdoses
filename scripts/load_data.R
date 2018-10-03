library(tidyverse)
library(janitor)
library(lubridate)

#rm(list = ls())
#data_path <- "./data/overdoses"
#data_list <- list.files(path = data_path, pattern = ".csv")
#data_list <- paste0(data_path, "/", data_list)
#data_list <- lapply(data_list, read_csv)

#for(i in seq_along(data_list)){
#  data_list[[i]]$`Incident Zip` <- as.character(data_list[[i]]$`Incident Zip`)
#  data_list[[i]]$`Decedent Zip` <- as.character(data_list[[i]]$`Decedent Zip`)
#}

#data <- bind_rows(data_list)

#colnames(data) <- tolower(colnames(data))
#colnames(data) <- gsub(" ", "_", colnames(data))

URLs <- c("2017" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/2d963e35-4f69-495e-985e-55acd72c87ca/download/crimelabaccidentaldrugdeathsextract2017.csv",
          "2016" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/386ca0ed-717d-46d3-8ed2-da37553ec6d1/download/crimelabaccidentaldrugdeathsextract2016.csv",
          "2015" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/502136a8-649b-44c5-9b08-ec2733201e5f/download/crimelabaccidentaldrugdeathsextract2015.csv",
          "2014" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/7146df41-9531-4327-961a-362ad6f3d2c3/download/crimelabaccidentaldrugdeathsextract2014.csv",
          "2013" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/ee648fab-3048-48b6-9757-f8dfbae378c8/download/crimelabaccidentaldrugdeathsextract2013.csv",
          "2012" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/d8db2bcb-58cf-4de7-91f5-82d122c0f1f3/download/crimelabaccidentaldrugdeathsextract2012.csv",
          "2011" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/1556690f-47e5-4ce5-bd5f-25eff4955c32/download/crimelabaccidentaldrugdeathsextract2011.csv",
          "2010" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/8f8d068c-9c4b-4563-bc3b-1eee72bbf533/download/crimelabaccidentaldrugdeathsextract2010.csv",
          "2009" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/395f38ac-d021-4b33-959d-b06bd7b9efe1/download/crimelabaccidentaldrugdeathsextract2009.csv",
          "2008" = "https://data.wprdc.org/dataset/7fb0505e-8e2c-4825-b22c-4fbee8fc8010/resource/233c321d-ff80-4d06-85b7-d5c3f227f2a8/download/crimelabaccidentaldrugdeathsextract2008.csv")
URLs
map_df(URLs, read_csv, col_names = TRUE, cols(.default = "c")) -> data

#map_df(list.files("data/overdoses", full.names = TRUE), 
#       read_csv, col_names = TRUE, cols(.default = "c")) -> data

data %>%
  clean_names() %>% 
  mutate(id = row_number(),
         date = mdy(death_date),
         year = year(date),
         yday = yday(date),
         month = month(date, label = TRUE),
         mday = mday(date),
         wday = wday(date),
         incident_zip = as.character(incident_zip)) %>% 
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
