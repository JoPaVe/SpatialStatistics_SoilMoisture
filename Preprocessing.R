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
    filter(str_detect(df$GEO,"Small.*Saskatchewan")) |>
    filter(!is.na(VALUE))
})

############## Filter SADRegions to only get Saskatchewan ###############

SADRegions2008_2015 <- SADRegions2008_2015 |>
  filter(PRuid == 47) 
  

SADRegions2017 <- SADRegions2017 |>
  filter(PRuid == 47) 

############## Intersects Polygons of SADRegions with moisture rasters  ###############

year_of_SAD_change <- 2016 # Define when SADs were changed

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
  if (years_integer[year] <= year_of_SAD_change) {
    intersected_year_values <- st_intersects(year_sf_obj, st_geometry(SARRegions2008_2015_crs_transformed))
  }  
  else {
    intersected_year_values <- st_intersects(year_sf_obj, st_geometry(SARRegions2017_crs_transformed))
  }
  intersected_years_matrices[[toString(years_integer[year])]] <- as.matrix(intersected_year_values)
}

############## Create list per SAD to get mean moisture per entry ###############

# Create storage dataframe for each year, for each SAD

SAD_moisture <- list()

SARRegions_number_2008_2015 <- dim(SARRegions2008_2015_crs_transformed)[1]
SARRegions_number_2017 <- dim(SARRegions2017_crs_transformed)[1]


for (year in names(moisture_data)) {
  if (as.integer(year) <= year_of_SAD_change) {
    number_rows <- SARRegions_number_2008_2015
  }
  else {
    number_rows <- SARRegions_number_2017
  }
  SAD_moisture[[year]] <- data.frame(matrix(nrow = number_rows, ncol = dim(moisture_data[[year]])[2]-2))
  colnames(SAD_moisture[[year]]) <- names(moisture_data[[year]][3:dim(moisture_data[[year]])[2]])
}


for (year in names(moisture_data)) {
  year_integer <- as.integer(year)
  if (year_integer <= year_of_SAD_change) {
    number_rows <- SARRegions_number_2008_2015
  }
  else {
    number_rows <- SARRegions_number_2017
  }
  for (SAD_id in 1:number_rows) {
    mean_vec <- colMeans(subset(moisture_data[[year]], intersected_years_matrices[[year]][,SAD_id]), na.rm = T)
    SAD_moisture[[year]][SAD_id,] <- mean_vec[3:dim(moisture_data[[year]])[2]]
    
  }
}

############## Combine crop_yield data with yearly Dataframes of SAD_moisture ###############

# Add column of SAD_id
Saskatchewan_cropyield <- lapply(Saskatchewan_cropyield, FUN = function(df) {
  df <- df |>
    mutate(SAD_ID = as.integer(sub(".*?(\\d+).*", "\\1",df$GEO)))
})

Saskatchewan_cropyield_to_merge <- lapply(Saskatchewan_cropyield, FUN = function(df) {
  df <- df |>
  dplyr::select(c(REF_DATE, VALUE, SAD_ID))
})

Saskatchewan_cropyield_to_merge <- merge(Saskatchewan_cropyield_to_merge$canola_cropyields, Saskatchewan_cropyield_to_merge$wheat_cropyields, by = c("REF_DATE", "SAD_ID"))

colnames(Saskatchewan_cropyield_to_merge) <- c("Year", "SAD_ID", "Canola_yield", "Wheat_yield")

SAD_moisture_crop <- SAD_moisture

for (year in names(moisture_data)) {
  merge_df_specific_year <- Saskatchewan_cropyield_to_merge |> 
    dplyr::filter(Year == as.integer(year)) |>
    dplyr::select(c(SAD_ID, Canola_yield, Wheat_yield))
  SAD_moisture_crop[[year]] <- merge(SAD_moisture[[year]], merge_df_specific_year, by.x = 0, by.y = c("SAD_ID"))
  SAD_moisture_crop[[year]] <- base::subset(SAD_moisture_crop[[year]], select = -c(Row.names))
}

