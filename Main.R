library(terra)
library(spData)
library(raster)

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(sf)
library(parallel)
library(raster)
library(stringr)
library(lubridate)


## Crawl data from website
source(paste(getwd(), "/DataScraping.R", sep = ""))


link <- "https://agriculture.canada.ca/atlas/data_donnees/psssm/data_donnees/tif/absolute/"

extract_all_datafiles(c(2021:2022), link)

## Add and store data from DataLoading

## Data loading:
source(paste(getwd(), "/DataLoading.R", sep = ""))
path = c("E:/SpacialStatistics_MoistureData/data")

## Shapefiles
if (!file.exists(paste(getwd(),"/data/shapefiles/SAD2017/SADRegions2017_shape.shp", sep = ""))) {
  SADRegions2017 <- get_SAD_layers(path, "SAD2017")
  st_write(SADRegions2017, paste(getwd(),"/data/shapefiles/SAD2017/SADRegions2017_shape.shp", sep = ""))
} else {
  SADRegions2017 <- st_read(paste(getwd(),"/data/shapefiles/SAD2017/SADRegions2017_shape.shp", sep = ""))
}

if (!file.exists(paste(getwd(),"/data/shapefiles/SAD2008_2015/SADRegions2008_2015_shape.shp", sep = ""))) {
  SADRegions2008_2015 <- get_SAD_layers(path, "SAD2008_2015")
  st_write(SADRegions2017, paste(getwd(),"/data/shapefiles/SAD2008_2015/SADRegions2008_2015_shape.shp", sep = ""))
} else {
  SADRegions2008_2015 <- st_read(paste(getwd(),"/data/shapefiles/SAD2008_2015/SADRegions2008_2015_shape.shp", sep = ""))
}

## Crop yields
cropyield_exist <- c(!file.exists(paste(getwd(),"/data/shapefiles/cropyield/canola_cropyields.csv", sep = "")), !file.exists(paste(getwd(),"/data/shapefiles/cropyield/wheat_cropyields.csv", sep = "")))

if (any(cropyield_exist == T)) {
  cropyield_data <- load_cropyield_data(path)
  write.csv(cropyield_data$canola_cropyields, paste(getwd(),"/data/cropyield/canola_cropyields.csv", sep = ""))
  write.csv(cropyield_data$wheat_cropyields, paste(getwd(),"/data/cropyield/wheat_cropyields.csv", sep = ""))
} else {
  cropyield_data <- list(
    canola_cropyields = read.csv(paste(getwd(),"/data/cropyield/canola_cropyields.csv", sep = "")),
    wheat_cropyields = read.csv(paste(getwd(),"/data/cropyield/wheat_cropyields.csv", sep = ""))
  )
}


moisture_objects <- load_tif_pictures(path, ".tif", c(2011))

