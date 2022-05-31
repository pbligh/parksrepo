library(tidyverse)
library(sf)
library(tmap)
library(shinyjs)
library(osmdata)
library(RColorBrewer)
install.packages("raster")
library(raster)
install.packages("vtable")
library(vtable)
library(dplyr)
install.packages("basemaps")
library(basemaps)
install.packages("r5r")
library(r5r)
# grand parks dataset
url <- "https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d79baa27d/resource/35796624-15df-4503-a569-797665f8768e/download/espace_vert.json"

temp <-  tempfile()
download.file(url, temp)

parks <- read_sf(temp)

#limits of montreal dataset
url <- "https://data.montreal.ca/dataset/b628f1da-9dc3-4bb1-9875-1470f891afb1/resource/674c2a18-e013-42ca-bfa6-3bb7358e820d/download/limites-terrestres.zip"

download.file(url, temp)
unzip(temp)
mtl_boundary <- read_sf("limites-terrestres.shp")
mtl_boundary
plot(mtl_boundary)

#importing agglomeration of mtl dataset
url <- "https://data.montreal.ca/dataset/00bd85eb-23aa-4669-8f1b-ba9a000e3dd8/resource/e9b0f927-8f75-458c-8fda-b5da65cc8b73/download/limadmin.geojson"

temp <-  tempfile()
download.file(url, temp)


mtl <- read_sf(temp)
mtl
plot(mtl)

# need to intersect the montreal boundary dataset with the agglomeration of mtl data
# they don't share the same borders (limits data is more specific, so that is why we had to intersect)
mtl_boundary <- 
  mtl_boundary %>% 
  st_union()
mtl_boundary

mtl <- 
  mtl %>% 
  st_intersection(mtl_boundary)
plot(mtl)

#setting the map mode to an interactive map
tmap_mode("view")



#plotting all parks on a map
parks <- 
  parks %>% 
  st_intersection(mtl_boundary)
plot(parks["Nom"])

#calculates area of all parks in square meters
st_area(parks)

#adding a column with the areas in square meters of parks
parks <- 
mutate(parks) %>% 
  add_column(st_area(parks))

glimpse(parks)
parks

#statistics table for parks
st(parks)

#mapping the parks using brewer colors and mapping by park area
my_map <- 
  tm_shape(mtl) +
  tm_polygons() + 
  tm_shape(parks) + 
  tm_polygons("st_area(parks)", 
              palette = "BuGn")

my_map

#however, the data includes some green spaces that aren't really parks
#so I wanted to break the green spaces down by how the data classified them under "TYPO1"

#all parks classified as "Grand parc"
large_parks <- 
 filter(parks, TYPO1 == "Grand parc")

#all parks classified as "Parc d'arrondissement"
Borough_parks <- 
  filter(parks, TYPO1 == "Parc d'arrondissement")

#all parks classified as other green spaces
other_green <- 
  filter(parks, TYPO1 =="Autre espace vert")

road_space <- 
  filter(parks, TYPO1 == "Espace voirie")

in_process <- 
  filter(parks, TYPO1 == "En cours de validation")

plot(in_process["Nom"])

plot(road_space["Nom"])

plot(other_green["Nom"])

plot(Borough_parks["Nom"])

plot(large_parks["Nom"])

#all park layers mapped
my_map2 <- 
  tm_shape(large_parks) + 
  tm_fill("green") + 
  tm_shape(Borough_parks) + 
  tm_fill("blue") +
  tm_shape(other_green) + 
  tm_fill("red") + 
  tm_shape(road_space) + 
  tm_fill("orange") + 
  tm_shape(in_process)+
  tm_fill("purple")

#map of all park layers by their different classifications
my_map2


#selecting just the large and borough parks to be mapped
good_parks <- 
  filter(parks, TYPO1 == "Grand parc" | TYPO1 == "Parc d'arrondissement")

good_parks


glimpse(good_parks)

good_parks_map   

good_parks <- rename(good_parks, park_area = `st_area(parks)`)

good_parks_map <- 
  tm_shape(good_parks) +
  tm_polygons("park_area", 
              title = "Park Area (in m^2)", 
              id = "Nom",
              palette = "BuGn", 
              legend.hist = T)

tmap_mode(mode="view")
good_parks_map

#interactive map of all good parks, make sure you run the interactive map code line
good_parks_map

#statistics table for the good parks
st(good_parks)

#parks above 5,000 square meters

parks_above_5000 <-
  good_parks %>% 
  mutate(park_area = units::drop_units(park_area)) |>
  filter(park_area >= 5000)

parks_above_5000_map <- 
  tm_shape(parks_above_5000) +
  tm_polygons("park_area", 
              title = "Park Area (in m^2)", 
              id = "Nom",
              palette = "BuGn")

parks_above_5000_map

#filtering parks over 10,000 square meters (1 hectare)

parks_above_10000 <-
  good_parks %>% 
  mutate(park_area = units::drop_units(park_area)) |>
  filter(park_area >= 10000)

parks_above_10000_map <- 
  tm_shape(parks_above_10000) +
  tm_polygons("park_area", 
              title = "Park Area (in m^2)", 
              id = "Nom",
              palette = "BuGn")

parks_above_10000_map


#parks above 30,000 meters squared (3 hectares)
parks_above_30000 <-
  good_parks %>% 
  mutate(park_area = units::drop_units(park_area)) |>
  filter(park_area >= 30000)
parks_above_30000

parks_above_30000_map <- 
  tm_shape(parks_above_30000) +
  tm_polygons("park_area", 
              title = "Park Area (in m^2)", 
              id = "Nom",
              palette = "BuGn")
parks_above_30000_map


#parks over 50,000 meters squared (5 hectares)

parks_above_50000 <-
  good_parks %>% 
  mutate(park_area = units::drop_units(park_area)) |>
  filter(park_area >= 50000)

parks_above_50000_map <- 
  tm_shape(parks_above_50000) +
  tm_polygons("park_area", 
              title = "Park Area (in m^2)", 
              id = "Nom",
              palette = "BuGn")

parks_above_50000_map

#parks above 100,000 meters squared (10 hectares)
parks_above_100000 <-
  good_parks %>% 
  mutate(park_area = units::drop_units(park_area)) |>
  filter(park_area >= 100000)


parks_above_100000_map <- 
  tm_shape(parks_above_100000) +
  tm_polygons("park_area", 
              title = "Park Area (in m^2)", 
              id = "Nom",
              palette = "BuGn")
parks_above_100000_map

#parks above 200,000 meters squared (20 hectares)

parks_above_200000 <-
  good_parks %>% 
  mutate(park_area = units::drop_units(park_area)) |>
  filter(park_area >= 200000)


parks_above_200000_map <- 
  tm_shape(parks_above_200000) +
  tm_polygons("park_area", 
              title = "Park Area (in m^2)", 
              id = "Nom",
              palette = "BuGn", 
              legend.hist = T)


parks_above_200000_map


good_parks            #1473
parks_above_5000      #690
parks_above_10000     #474
parks_above_30000     #215
parks_above_50000     #115
parks_above_100000    #60
parks_above_200000    #20



# MAPS ------------------------------------------------------------------
tmap_mode("view")

good_parks_map
parks_above_5000_map 
parks_above_10000_map
parks_above_30000_map
parks_above_50000_map
parks_above_100000_map
parks_above_200000_map

combined_map

# Census -------------------------------------------------------------
library(cancensus)
options(cancensus.cache_path = getwd())
options(cancensus.api_key = "CensusMapper_f7efc25c289687165f04e88b96a673a0")

list_census_datasets()
%>% 
  filter(str_extract(description, "\\d+") =="2016")

r <- list_census_regions("CA16") %>% 
  filter(level =="CSD" & name == "Montréal") %>% 
  as_census_region_list()


# ecology data ----------------------------
#import forest dataset
url <- "https://data.montreal.ca/dataset/29791562-f050-401e-b405-5c1fbf427f65/resource/c21c28e6-d9a2-41f1-aebf-c5e00ef3de32/download/bois.geojson"

temp <-  tempfile()
download.file(url, temp)

mtl_forest <- read_sf(temp)

mtl_forest

mtl_forest %>% 
  st_is_valid()

mtl_forest %>% 
  (is.na(Shape_Leng))

tmap_options(check.and.fix = TRUE)


tm_shape(mtl_forest) + 
  tm_borders(mtl_forest) + 
  tm_fill("Shape_Area")
 
     
plot(mtl_forest["Shape_Area"])


mtl_forest <- st_make_valid(mtl_forest)
st_area(mtl_forest)
mtl_forest <- 
  mutate(mtl_forest) %>% 
  add_column(st_area(mtl_forest))

mtl_forest

mtl_forest <- rename(mtl_forest, forest_area = `st_area(mtl_forest)`)


glimpse(mtl_forest)


mtl_forest <- mtl_forest %>% 
  mutate(`st_area(mtl_forest)` = units::drop_units(`st_area(mtl_forest)`)

mtl_forest


mtl_forest_map <- 
  tm_shape(mtl_forest) + 
  tm_polygons("Shape_Area", 
    palette = "YlOrBr")
mtl_forest_map


combined_map <- 
  good_parks_map + 
  mtl_forest_map +
  mtl_wetlands_map

combined_map


# wetlands dataset ------------------------

url <- "https://data.montreal.ca/dataset/6aad129f-175a-4c95-aa9e-a8e8f6531bd7/resource/1ba5cb02-9374-453f-9c1f-f3c7bf4cbd9d/download/milieuhumide.geojson"
temp <-  tempfile()
download.file(url, temp)

mtl_wetlands <- read_sf(temp)

glimpse(mtl_wetlands)

tmap_mode("view")
tm_shape(mtl_wetlands) + 
  tm_polygons("Shape_Area")


st_area(mtl_wetlands)

mtl_wetlands_map <- 
  tm_shape(mtl_wetlands) + 
  tm_polygons("Shape_Area", 
              palette = "BuPu")
mtl_wetlands_map


install.packages("usethis")
library(usethis)
usethis::use_github()
use_git()
