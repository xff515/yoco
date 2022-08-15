### Spatial mapping: Map merchant locations to regions (Block IDs) in the City of Cape Town shape files
### Rowan Clarke and Yifei Wu

## References:
## https://cengel.github.io/R-spatial/intro.html
## https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile

library(dplyr)
library(readr)
library(sf)
library(tmap)
library(tmaptools)

## 1. import Yoco's location data (coordinates) 
df_location <- read_csv("Z:/data/loadshedding/dim_merchants_202208121120.csv")
df_location <- df_location[!is.na(df_location$latitude),] #remove NAs

df_location$long <- df_location$latitude # correct lat and long values (data in raw data were flipped)
df_location$lat <- df_location$longitude

## 2. import the City of Cape Town shapefile
CPTshape <- st_read("Z:/data/loadshedding/CPTshape_Loadshedding_Blocks/Loadshedding_Blocks.shp")

# print the CPT shapes
plot(st_geometry(CPTshape))

qtm(CPTshape) +
  tm_legend(show = FALSE)

# 3. convert Yoco's location data into an sf object 
df_location_sf <- st_as_sf(df_location, 
                      coords = c(x = "long", y = "lat"),
                      crs=st_crs(CPTshape)) # the same coordinate system as the shape file
colnames(df_location_sf)[2:3] <- c("latitude","longitude")

# 4. Identify intersections: for some reason, no intersection is identified using either method

# method 1: 
points <- df_location_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, CPTshape))
  , area = if_else(is.na(intersection), '', CPTshape$SUA_NAME16[intersection])
) 
points

# method 2: 
joined <- st_join(df_location_sf, CPTshape, joing=st_within)

# if there are intersected values, we should see red dots on the map 
tm_shape(CPTshape) +
  tm_fill() +
  tm_shape(joined) +
  tm_bubbles(col = "red", size = 0.25)

     