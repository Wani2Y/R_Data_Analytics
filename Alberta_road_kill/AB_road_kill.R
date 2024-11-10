library(sf)
library(leaflet)
library(dplyr)

#get the map of Alberta
library(ggmap)
library(maps)
library(ggthemes)

ab_map <- get_stamenmap(
  bbox = c(left = -121, bottom = 49, right = -109, top = 61),
  maptype = "toner",
  zoom = 7
)

#examine the map, particularly on the accuracy of the bbox
ggmap(ab_map)

#read in and organize roadkill data
rk_2020 <- read.csv("D:/Projects/Teaching/Data_visualization/Alberta_road_kill/raw_data/open-data_aww-2020.csv", header = T) %>% as_tibble()
head(rk_2020)
str(rk_2020)

#convert GPR point as spatial data
rk_2020 <- st_as_sf(rk_2020, coords = c("Longitude", "Latitude"))
str(rk_2020)
head(rk_2020)

#plotting the basic version
ab_rk_map <- ggmap(ab_map) +
  geom_point(data = rk_2020,
             aes(x = lon, y = lat, col = Family),
             size = 1.5) + 
  scale_color_manual(values = c("#ffd358", "#ee6a6a", "#ffff00", "#1d032e", "#48036d", "#9b6feb", "#c53333", "#cb92c2", "#89215e", "#932544", 
                                "#008bff", "#00ffff", "#000b77", "#6cbcff", "#4f3606", "#6b4509", "#f6f4ec", "#b5654e", "#c53333", "#00c6ee",
                                "#316b67", "#000000", "#ff0090", "#f34524", "#2b4e6d", "#092408", "#ce9595")) +
  theme_map()
   
ab_rk_map


#draw the interactive with tmap
library(tmap)
library(tmaptools)

ab_shp <- st_read("D:/Projects/Teaching/Data_visualization/Alberta_road_kill/shapefile/NRN_AB_14_0_ROADSEG.shp", stringsAsFactors = FALSE)
str(ab_shp)
head(ab_shp)

tmap_mode("plot")

ab_rk_map_in <- tm_shape(ab_shp) + 
  tm_lines(col = "gray") + 
  tm_shape(rk_2020, projection = "longlat") +
  tm_layout(legend.outside = TRUE) + 
  tm_bubbles(size = 0.5, jitter = 0.2, col = "Family")
  
tmap_save(ab_rk_map_in, "ab_rk_2021map.pdf")
tm_polygons(id = "L_PLACENAM", col = "L_PLACENAM", palette = "Pastel1")

#Redo
library(sf)
library(dplyr)
library(plotly)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(ggformula)
library(ggplot2)
library(ggstance)
ab_shp <- ab_shp <- st_read("D:/Projects/Teaching/Data_visualization/Alberta_road_kill/shapefile/NRN_AB_14_0_ROADSEG.shp", stringsAsFactors = FALSE)
str(ab_shp)
head(ab_shp)

#read in the roadkill data
#read in and organize roadkill data
rk_2020 <- read.csv("D:/Projects/Teaching/Data_visualization/Alberta_road_kill/raw_data/open-data_aww-2020.csv", header = T) %>% as_tibble()
head(rk_2020)
str(rk_2020)

#drop the usesless column from the roadkill data
rk_2020 <- rk_2020 %>% select("Month.Observed", "Species", "Family", "lon", "lat")
head(rk_2020)

#convert GPR point as spatial data
rk_2020 <- st_as_sf(rk_2020, coords = c("lon", "lat"))
str(rk_2020)
head(rk_2020)

#plotting
tmap_mode("plot")
ab_rk_map <- tm_shape(ab_map) + 
  tm_lines(col = "gray") + 
  tm_shape(rk_2020, projection = "longlat") +
  tm_layout(legend.outside = TRUE) + 
  tm_symbols(size = 0.5, jitter = 0.2, col = "Family", shape = "Month.Observed")

ab_rk_map
