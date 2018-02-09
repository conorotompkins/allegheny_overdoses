#load packages
library(tidyverse)
library(tidycensus)
library(sf)
library(viridis)
library(ggmap)
library(zipcode)

#load od data
source("scripts/load_data.R")

#set cache and api key
options(tigris_use_cache = TRUE)
Sys.getenv("CENSUS_API_KEY")

#write_csv(df_zips, "data/zip_codes.csv")

#load census variables
v15 <- load_variables(2016, "acs5", cache = TRUE)

#View(v15)

#load zip codes from OD data
df_zips <- read_csv("data/zip_codes.csv")

#load PA zip codes
data("zipcode")
pa_zips <- zipcode %>% 
  filter(state == "PA")

#get allegheny county zip codes
allegheny_zip_codes <- df %>% 
  select(incident_zip) %>% 
  distinct() %>% 
  filter(!is.na(incident_zip), incident_zip != "1`513")

#query census api
usa <- get_acs(geography = "zcta", 
                     variables = c(total_population = "B01003_001E"),
                     geometry = TRUE) %>% 
  mutate(NAME = str_replace(NAME, "ZCTA5 ", ""))

usa %>% 
  st_centroid() %>% 
  st_geometry() -> usa_centroids
usa_centroids[[1]]

#join PA zips with census df
pennsylvania <- pa_zips %>%
    left_join(usa, by = c("zip" = "NAME"))

#plot
pennsylvania %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")


#join allegheny zips with census df
allegheny <- allegheny_zip_codes %>%
  left_join(usa, by = c("incident_zip" = "NAME"))

allegheny %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")



#does not work
allegheny_map <- get_map(location = "Allegheny County")
ggmap(allegheny_map) +
  geom_sf(data = allegheny, aes(fill = estimate, color = estimate), inherit.aes = FALSE) + 
  #coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

