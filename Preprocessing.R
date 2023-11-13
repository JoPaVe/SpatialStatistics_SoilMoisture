library(dplyr)
library(plyr)
library(stringr)
library(spData)
library(sf)
library(tidyr)
library(terra)
library(raster)
library(lubridate)

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


############## Gap-filled NaNs ###############

# Rewrite this monster
fill_missing <- function(year) {
  missing_indices <- which(is.na(year), arr.ind = T)
  if (all(missing_indices == 0)) {
    return(year)
  } 
  for (i in 1:nrow(missing_indices)){
    if ((missing_indices[i,2] > 1) & (missing_indices[i,2] < dim(year)[2])) {
      if (is.na(year[i[1],missing_indices[i,2]+1]) | is.na(year[missing_indices[i,1],missing_indices[i,2]-1])) {
        year[missing_indices[i,1],missing_indices[i,2]] <- NA
      }
      year[missing_indices[i,1],missing_indices[i,2]] <- (year[missing_indices[i,1],missing_indices[i,2]-1] + year[missing_indices[i,1],missing_indices[i,2]+1]) / 2
    } else if (missing_indices[i,2] == 1) {
      if (is.na(year[missing_indices[i,1],missing_indices[i,2]]+1)) {
        year[missing_indices[i,1],missing_indices[i,2]] <- NA
      }
      year[missing_indices[i,1],missing_indices[i,2]] <- year[missing_indices[i,1],missing_indices[i,2] + 1]
    } else if (i == dim(year)[2]) {
      if (is.na(year[missing_indices[i,1],missing_indices[i,2]]-1)) {
        year[missing_indices[i,1],missing_indices[i,2]] <- NA
      }
      year[missing_indices[i,1],missing_indices[i,2]] <- year[missing_indices[i,1],missing_indices[i,2] - 1]
    }
  }
  return(year)
}

SAD_moisture_filled <- lapply(SAD_moisture, FUN = fill_missing)


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

SAD_moisture_crop <- SAD_moisture_filled

for (year in names(moisture_data)) {
  merge_df_specific_year <- Saskatchewan_cropyield_to_merge |> 
    dplyr::filter(Year == as.integer(year)) |>
    dplyr::select(c(SAD_ID, Canola_yield, Wheat_yield))
  SAD_moisture_crop[[year]] <- merge(SAD_moisture_filled[[year]], merge_df_specific_year, by.x = 0, by.y = c("SAD_ID"))
  SAD_moisture_crop[[year]] <- base::subset(SAD_moisture_crop[[year]], select = -c(Row.names))
}

############## Create dataframe for Iterative Chisquared ###############

create_iterativeCS_df <- function(crop = c("Canola_yield", "Wheat_yield"), sequence = c("high_to_low", "low_to_high"), cropyield_df, soil_moisture_df, years, grid_spec, quantiles) {
  # Grid_spec: c(from, to, in)
  # quantiles: c(lower, higher)
  
  grouped_crops <- group_into_classes(cropyield_df, crop, quantiles, years)
  moisture_counts <- get_moisture_counts(soil_moisture_df, grid_spec)
  moisture_counts_prepared <- prepare_for_merge(moisture_counts, years)
  SAD_moisture_crop_class <- merge_SAD_moisture_crop(grouped_crops, moisture_counts_prepared)
  return(SAD_moisture_crop_class)
}

group_into_classes <- function(cropyield_df, crop, quantiles, years) {
  quantiles_cropyield <- quantile(cropyield_df[[crop]], c(quantiles[1], quantiles[2]))
  
  if (crop == "Canola_yield") {
    cropyield_df_class <- cropyield_df |> 
      dplyr::filter(Year %in% years) |>
      dplyr::select(Year, SAD_ID, Canola_yield) |>
      dplyr::mutate(class=cut(Canola_yield, breaks = c(-Inf, quantiles_cropyield[1], quantiles_cropyield[2], Inf), labels = c("low", "normal", "high")))
    
  }
  else {
    quantiles_cropyield <- cropyield_df |> 
      dplyr::filter(Year %in% years) |>
      dplyr::select(Year, SAD_ID, Wheat_yield) |>
      dplyr::mutate(class=cut(Wheat_yield, breaks = c(-Inf, quantiles_cropyield[1], quantiles_cropyield[2], Inf), labels = c("low", "normal", "high")))
  }
  
  cropyield_df_class$class <- as.character(cropyield_df_class$class)

  return(cropyield_df_class)
}

get_moisture_counts <- function(soil_moisture_df, grid_spec) {
  moisture_counts <- lapply(SAD_moisture_filled, FUN = function(year) {
    apply(year, 1, FUN = get_SAD_moisture_counts, grid_spec = grid_spec, column_names = colnames(year), year_name = deparse(substitute(year)))
  }) 
}

get_SAD_moisture_counts <- function(SAD, grid_spec, column_names, year_name) {
  
  used_dates <- array()
  sequence <- seq(1,length(SAD),7) 
  grid <- seq(grid_spec[1], grid_spec[2], grid_spec[3])
  
  SAD <- unlist(SAD)
  cuts <- cut(SAD[sequence[1]:sequence[4]], grid, include.lowest = T, right = F)
  tab <- table(cuts)
  output <- cumsum(tab) 
  used_dates <- lubridate::week(ymd(column_names[sequence[1]]))
  
  if (length(SAD) %% 7 != 0) {
    sequence <- c(sequence,dim(SAD))
  }
  
  for (i in 2:(length(sequence)-2)) {
    cuts <- cut(SAD[sequence[i-1]:sequence[i+2]], seq(grid_spec[1], grid_spec[2], grid_spec[3]), include.lowest = T, right = F)
    tab <- table(cuts)
    output <- cbind(output,cumsum(tab))
    used_dates <- append(used_dates,lubridate::week(ymd(column_names[sequence[i]])))
  }
  
  
  
  
  ## Extract in different function
  output <- data.frame(output)
  colnames(output) <- used_dates
  output[["Interval"]] <- 1:(length(grid)-1)
  
  output <- gather(output, key = "Week", value = "Moisture_Count", -Interval)

  return(output)
}

prepare_for_merge <- function(moisture_counts, years_count) {
  return_list_year <- list()
  
  for (year in years_count) {
    return_data_frame_year <- moisture_counts[[toString(year)]]
    for (SAD_count in 1:length(return_data_frame_year)) {
      return_data_frame_year[[SAD_count]][["Year"]] <- year
      return_data_frame_year[[SAD_count]][["SAD_ID"]] <- SAD_count
    }
    return_list_year[[toString(year)]] <- Reduce(function(df1,df2) rbind(df1,df2), return_data_frame_year)
    
  }
  return_data_frame <- Reduce(function(df1,df2) rbind(df1,df2), return_list_year)
  
  return(return_data_frame)
}


merge_SAD_moisture_crop <- function(grouped_crops, moisture_counts_prepared) {
  SAD_moisture_crop_class <- merge(moisture_counts_prepared, grouped_crops, by = c("Year", "SAD_ID"), all.y = T)
  
  SAD_moisture_crop_class_return <- SAD_moisture_crop_class |>
    group_by(Week, Interval, class) |>
    pivot_wider(id_cols = c("Week", "Interval"), names_from = class, values_from = Moisture_Count, values_fn = sum)
  
  return(SAD_moisture_crop_class_return)
}


df <- create_iterativeCS_df("Canola_yield",
                      sequence = "high_to_low",
                      Saskatchewan_cropyield_to_merge,
                      SAD_moisture_filled,
                      c(2010,2011,2012),
                      c(0,50,0.2),
                      c(0.25,0.75))


