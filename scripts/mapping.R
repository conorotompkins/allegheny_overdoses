library(tidyverse)
library(tidycensus)
library(sf)
library(viridis)
library(ggmap)

source("scripts/load_data.R")
options(tigris_use_cache = TRUE)
Sys.getenv("CENSUS_API_KEY")

df_zips <- df %>%
  select(incident_zip) %>% 
  distinct()

write_csv(df_zips, "data/zip_codes.csv")

df_zips <- read_csv("data/zip_codes.csv")

usa <- get_acs(geography = "zcta", 
                     variables = "B01003_001",
                     geometry = TRUE) %>% 
  mutate(NAME = str_replace(NAME, "ZCTA5 ", "")) %>% 
  st_transform(crs = 3857)

allegheny <- df_zips %>%
    left_join(usa, by = c("incident_zip" = "NAME"))


allegheny %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")


allegheny_map <- get_map(location = "Allegheny County")
ggmap(allegheny_map) +
  geom_polygon(data = allegheny, aes(fill = estimate, color = estimate)) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

