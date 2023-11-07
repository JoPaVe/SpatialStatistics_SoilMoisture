library(dplyr)
library(plyr)
library(stringr)
library(spData)
library(sf)
library(tidyr)
library(terra)
library(raster)

############## Preprocessing crop yield data ###############

Saskatchewan_cropyield <- lapply(cropyield_data, FUN = function(df) {
  df <- df |>
    filter(str_detect(df$GEO,"Saskatchewan"))
})

############## Filter SADRegions to only get Saskatchewan ###############

SADRegions2008_2015 <- SADRegions2008_2015 |>
  filter(PRuid == 47) 

SADRegions2017 <- SADRegions2017 |>
  filter(PRuid == 47) 

############## Intersects Polygons of SADRegions with moisture rasters  ###############

moisture_data <- moisture_objects[[1]]
years_integer <- as.integer(names(moisture_data))

crs_tif <- crs(moisture_objects[[2]][[1]]) #Extract crs of first item of .tif pictures (stored as SpatRaster objects)

sf_use_s2(FALSE) # Disable usage of spherical geometry package for geographical coordinate operations as SADRegions2008_2015 has duplicates therwise
SARRegions2017_crs_transformed <- st_transform(SADRegions2017, crs = crs_tif)
SARRegions2008_2015_crs_transformed <- st_transform(SADRegions2008_2015, crs = crs_tif)

intersected_years_matrices <- list()
crs_SAR <- crs(SARRegions2017_crs_transformed$Shape)
for (year in 1:length(moisture_data)) {
    year_sf_obj <- st_as_sf(moisture_data[[year]], coords = c("x","y"), crs = crs_SAR)
  if (years_integer[year] <= 2016) {
    intersected_year_values <- st_intersects(year_sf_obj, st_geometry(SARRegions2008_2015_crs_transformed))
  }  
  else {
    intersected_year_values <- st_intersects(year_sf_obj, st_geometry(cSARRegions2017_crs_transformed))
  }
  intersected_years_matrices[[toString(years_integer[year])]] <- as.matrix(intersected_year_values)
}



