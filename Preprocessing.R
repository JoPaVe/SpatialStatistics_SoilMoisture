

############## Filter Crop yield data to only get Saskatchewan ###############

Saskatchewan_cropyield <- lapply(cropyield_data, FUN = function(df) {
  # Filter given crop yield data from Canada and extract only observations for Saskatchewan
  df <- df |>
    filter(str_detect(df$GEO,"Small.*Saskatchewan")) |>
    filter(!is.na(VALUE))
})

rm(list = c("cropyield_data"))

############## Filter SADRegions to only get Saskatchewan ###############

SADRegions2008_2015 <- SADRegions2008_2015 |>
  filter(PRuid == 47) 

SADRegions2017 <- SADRegions2017 |>
  filter(PRuid == 47) 

############## Combine Polygons of SADRegions with moisture rasters  ###############

kYearSADChange <- 2016  # SADs were changed in 2016

moisture_data <- moisture_objects[[1]]
years_integer <- as.integer(names(moisture_data))  # Extract years as integer values

crs_tif <- crs(moisture_objects[[2]][[1]])  # Extract crs of first item of .tif pictures (stored as SpatRaster objects)

sf_use_s2(FALSE)  # Disable usage of spherical geometry package for geographical coordinate operations as SADRegions2008_2015 has duplicates otherwise
SADRegions2017_crs_transformed <- st_transform(SADRegions2017, crs = crs_tif)  # Transform SADRegions to crs of Raster object (Long,Lat)
SADRegions2008_2015_crs_transformed <- st_transform(SADRegions2008_2015, crs = crs_tif)

intersected_years_matrices <- list()
crs_SAD <- crs(SADRegions2017_crs_transformed$Shape)


for (year in 1:length(moisture_data)) {
    year_sf_obj <- st_as_sf(moisture_data[[year]], coords = c("x","y"), crs = crs_SAD)  # Change x,y coordinates of the moisture data to long, lat
  if (years_integer[year] <= kYearSADChange) {
    intersected_year_values <- st_intersects(year_sf_obj, st_geometry(SADRegions2008_2015_crs_transformed))  # Find all intersections for years before and after kYearSADChange
  }  
  else {
    intersected_year_values <- st_intersects(year_sf_obj, st_geometry(SADRegions2017_crs_transformed))
  }
  intersected_years_matrices[[toString(years_integer[year])]] <- as.matrix(intersected_year_values)
}

rm(list = c("intersected_year_values", "crs_SAD", "year_sf_obj", "crs_tif"))


############## Create mean of soil moisture data for each SAD ###############

# Create storage dataframe for each year and for each SAD

SAD_moisture <- list()

kSADRegions20082015 <- dim(SADRegions2008_2015_crs_transformed)[1]  # Number of SAD regions in 2008_2015 and 2017
kSADRegions2017 <- dim(SADRegions2017_crs_transformed)[1]

# First create yearly dataframes 
for (year in names(moisture_data)) {
  if (as.integer(year) <= kYearSADChange) {
    number_rows <- kSADRegions20082015
  }
  else {
    number_rows <- kSADRegions2017
  }
  SAD_moisture[[year]] <- data.frame(matrix(nrow = number_rows, ncol = dim(moisture_data[[year]])[2]-2))
  colnames(SAD_moisture[[year]]) <- names(moisture_data[[year]][3:dim(moisture_data[[year]])[2]])
}

# Secondly create means of SADs not considering missing values
for (year in names(moisture_data)) {
  year_integer <- as.integer(year)
  if (year_integer <= kYearSADChange) {
    number_rows <- kSADRegions20082015
  }
  else {
    number_rows <- kSADRegions2017
  }
  for (SAD_id in 1:number_rows) {
    mean_vec <- colMeans(subset(moisture_data[[year]], intersected_years_matrices[[year]][,SAD_id]), na.rm = T)
    SAD_moisture[[year]][SAD_id,] <- mean_vec[3:dim(moisture_data[[year]])[2]]
    
  }
}

rm(list = c("kSADRegions20082015", "kSADRegions2017", "mean_vec", "moisture_data"))

############## Fill missing data acoording to White et al. (2019) - Canola yield sensitivity to climate indicators and passive microwave-derived soil moisture estimates in Saskatchewan, Canada###############
# "Dates with missing SMOS retrievals for a given CAR were gap-filled as the average of the two surrounding days"
# Additionally, as it is not specified in the paper, if the error occurs at the beginning of the year or at the end of the year, the subsequent (precedent) daily observation is taken

fill_missing <- function(year) {
  missing_indices <- which(is.na(year), arr.ind = TRUE)
  if (all(missing_indices == 0)) {
    return(year)
  } 
  for (i in 1:nrow(missing_indices)){
    missing_row <- missing_indices[i,1]
    missing_column <- missing_indices[i,2]
    if ((missing_column > 1) & (missing_column < dim(year)[2])) {  # Imputation at inner data column

      year[missing_row,missing_column] <- (year[missing_row,missing_column-1] + year[missing_row,missing_column+1]) / 2  # Calculate mean according to calculation above

    } else if (missing_column == 1) {  # Imputation at first data column

      year[missing_row,missing_column] <- year[missing_row,missing_column + 1]

    } else if (missing_column == dim(year)[2]) {  # Imputation at last data column

      year[missing_row,missing_column] <- year[missing_row,missing_column - 1]

    }
  }
  return(year)
}


SAD_moisture_filled <- lapply(SAD_moisture, FUN = fill_missing)

# Manually inspect remaining missing values
#SAD_moisture_remaining_errors <- lapply(SAD_moisture_filled, FUN = function(year) which(is.na(year), arr.ind = TRUE))

# Take the closest observations if missing data is in same SAD
SAD_moisture_filled$`2010`[10,105] <- SAD_moisture_filled$`2010`[10,106] <- 1/2 * (SAD_moisture_filled$`2010`[10,104] + SAD_moisture_filled$`2010`[10,107])
# If observations are at beginning but in same SAD, take mean of next four observations 
SAD_moisture_filled$`2011`[2,1] <- SAD_moisture_filled$`2011`[2,2] <- mean(as.numeric(SAD_moisture_filled$`2011`[2,3:6]))
SAD_moisture_filled$`2019`[3,1] <- SAD_moisture_filled$`2019`[3,2] <- mean(as.numeric(SAD_moisture_filled$`2019`[3,3:6]))

SAD_moisture_filled$`2013`[4,1] <- SAD_moisture_filled$`2013`[4,2] <- mean(as.numeric(SAD_moisture_filled$`2013`[4,3:6]))
SAD_moisture_filled$`2013`[18,1] <- SAD_moisture_filled$`2013`[18,2] <- mean(as.numeric(SAD_moisture_filled$`2013`[18,3:6]))
SAD_moisture_filled$`2013`[19,1] <- SAD_moisture_filled$`2013`[19,2] <- mean(as.numeric(SAD_moisture_filled$`2013`[19,3:6]))
SAD_moisture_filled$`2013`[20,1] <- SAD_moisture_filled$`2013`[20,2] <- mean(as.numeric(SAD_moisture_filled$`2013`[20,3:6]))


############## Prepare cropyield data for merge (Add SAD_ID) ###############

# Add column of SAD_ID - ID of SAD
Saskatchewan_cropyield <- lapply(Saskatchewan_cropyield, FUN = function(df) {
  df <- df |>
    mutate(SAD_ID = as.integer(sub(".*?(\\d+).*", "\\1",df$GEO)))  # Extract number of SAD from GEO name and store as own column
})

Saskatchewan_cropyield_to_merge <- lapply(Saskatchewan_cropyield, FUN = function(df) {
  df <- df |>
  dplyr::select(c(REF_DATE, VALUE, SAD_ID))  # Select only Year (REF_DATE), Cropyield (VALUE) and SAD_ID 
})

# Merge crop yield data from wheat and canola
Saskatchewan_cropyield_to_merge <- merge(Saskatchewan_cropyield_to_merge$canola_cropyields, Saskatchewan_cropyield_to_merge$wheat_cropyields, by = c("REF_DATE", "SAD_ID"))

colnames(Saskatchewan_cropyield_to_merge) <- c("Year", "SAD_ID", "Canola_yield", "Wheat_yield")  # Change column names accordingly

rm(list = "Saskatchewan_cropyield")


############## Create summary statistics for every SAD ###############

# Add SADRegionUID for merge (differentiate single and double digits)
Saskatchewan_cropyield_to_merge_UID <- Saskatchewan_cropyield_to_merge |>
  dplyr::mutate(SADRegionUID = ifelse(SAD_ID < 10,paste0("470",SAD_ID),paste0("47",SAD_ID)))  # Region UID are different between Shapefiles and cropyield data - have to be aligned

# Merge Cropyield data with Shapefiles for 2008_2015 and >2017
cropyield_summary_map_2008_2015 <- merge(Saskatchewan_cropyield_to_merge_UID |>
                                           dplyr::filter(Year <= kYearSADChange), 
                                         SADRegions2008_2015_crs_transformed,
                                         by = c("SADRegionUID"))

cropyield_summary_map_2017 <- merge(Saskatchewan_cropyield_to_merge_UID |>
                                      dplyr::filter(Year > kYearSADChange), 
                                    SADRegions2017_crs_transformed,
                                    by = c("SADRegionUID"))

rm(list = c("Saskatchewan_cropyield_to_merge_UID"))
# Get summary statistics for 2008_2015
Saskatchewan_cropyield_summary_to_merge_2008_2015 <- Saskatchewan_cropyield_to_merge |>
  dplyr::filter(Year <= kYearSADChange) |>
  dplyr::group_by(SAD_ID) |>
  dplyr::summarize(mean_canola = mean(Canola_yield),
                   mean_wheat = mean(Wheat_yield),
                   sd_canola = sd(Canola_yield),
                   sd_wheat = sd(Wheat_yield),
                   sum_canola = sum(Canola_yield),
                   sum_wheat = sum(Wheat_yield)) |>
  dplyr::mutate(share_canola = sum_canola / sum(sum_canola), 
                share_wheat = sum_wheat / sum(sum_wheat))

# Get summary statistics for >2017
Saskatchewan_cropyield_summary_to_merge_2017 <- Saskatchewan_cropyield_to_merge |>
  dplyr::filter(Year > kYearSADChange) |>
  
  dplyr::group_by(SAD_ID) |>
  dplyr::summarize(mean_canola = mean(Canola_yield),
                   mean_wheat = mean(Wheat_yield),
                   sd_canola = sd(Canola_yield),
                   sd_wheat = sd(Wheat_yield),
                   sum_canola = sum(Canola_yield),
                   sum_wheat = sum(Wheat_yield)) |>
  dplyr::mutate(share_canola = sum_canola / sum(sum_canola), 
                share_wheat = sum_wheat / sum(sum_wheat))



############## Create dataframe for Iterative Chisquared ###############

CreateIterativCS <- function(crop = c("Canola_yield", "Wheat_yield"), cropyield_df, soil_moisture_df, years, grid_spec, quantiles) {
  # Computes a dataframe that can be further used for the iterative chi-squared method. 
  
  # Args:
  # crop: Defines the crop for which the method should be used
  # soil_moisture_df: Soil moisture with observations per day in columns and SAD in rows 
  # years: Define included years
  # grid_spec: Vector that defines he grid that will be used for soil_moisture: c(from, to, in)
  # quantiles: Vector that defines the quantiles for the classes: c(lower, higher)
  
  grouped_crops <- GroupIntoClasses(cropyield_df, crop, quantiles, years)  # Classifies observations (SADs and years) into classes (low, normal, high)
  moisture_counts <- TrackOccurrences(soil_moisture_df, grid_spec)  # Uses grid to identify number of occurrences for soil moisture data in interval
  moisture_counts_prepared <- PrepareMerge(moisture_counts, years)  # Prepares merge of Soil moisture and crop data and add Year and SAD_ID for unique ID for merge
  SAD_moisture_crop_class <- MergeMoistureAndCrops(grouped_crops, moisture_counts_prepared)  # Runs merge of Soil moisture and crop data 
  return(list(SAD_moisture_crop_class, grouped_crops))
}

GroupIntoClasses <- function(cropyield_df, crop, quantiles, years) {
  
  quantiles_cropyield <- quantile(cropyield_df[[crop]], c(quantiles[1], quantiles[2]))  # Defines quantiles of crop yields
  
    cropyield_df_class <- cropyield_df |>  # Filter for crop and create labels for the respective classes
      dplyr::filter(Year %in% years) |>
      dplyr::select(Year, SAD_ID, crop) |>
      dplyr::mutate(class=cut(get(crop), breaks = c(-Inf, quantiles_cropyield[1], quantiles_cropyield[2], Inf), labels = c("low", "normal", "high")))

  cropyield_df_class$class <- as.character(cropyield_df_class$class)  # Store class labels as character column

  return(cropyield_df_class)
}

TrackOccurrences <- function(soil_moisture_df, grid_spec) {
  moisture_counts <- lapply(SAD_moisture_filled, FUN = function(year) {
    apply(year, 1, FUN = MoistureCountsInSAD, grid_spec = grid_spec, column_names = colnames(year), year_name = deparse(substitute(year)))  # apply to each SAD to get the number of moisture counts in each SAD
  }) 
}

MoistureCountsInSAD <- function(SAD, grid_spec, column_names, year_name) {
  

  sequence <- seq(1,length(SAD),7)  # Since daily data is available, a week is defined by 7 observations 
  grid <- seq(grid_spec[1], grid_spec[2], grid_spec[3])  # Define grid
  
  SAD <- unlist(SAD)
 
  if (length(SAD) %% 7 != 0) {
    sequence <- c(sequence,dim(SAD))  # If number of days cannot be divided by 7 (even number of weeks), define last week just until last observation
  }
  
  for (i in 2:(length(sequence)-2)) {
    cuts <- cut(SAD[sequence[i-1]:sequence[i+2]], seq(grid_spec[1], grid_spec[2], grid_spec[3]), include.lowest = T, right = F)  # Define intervals according to grid and three weeks moving window
    tab <- table(cuts)  # Create frequency table for first three weeks
    
    if (i == 2) {
      output <- cumsum(rev(tab))   # Create cumulative frequency table from high to low! 
      used_dates <- lubridate::week(ymd(column_names[sequence[i]]))  # Get current week
    }
    else {
      output <- cbind(output,cumsum(rev(tab))) 
      used_dates <- append(used_dates,lubridate::week(ymd(column_names[sequence[i]])))  # Get current week
    }

  }
  
  
  
  
  ## Extract in different function
  output <- data.frame(output)
  colnames(output) <- used_dates  # Create colnames as weeks
  output[["Interval"]] <- 1:(length(grid)-1)  # Create interval number
  
  output <- gather(output, key = "Week", value = "Moisture_Count", -Interval)  # Change data format to longitudinal with Week, Soilmoisture_Count and Interval number

  return(output)
}

PrepareMerge <- function(moisture_counts, years_count) {
  
  return_list_year <- list()
  
  for (year in years_count) {  # Go over all years
    return_data_frame_year <- moisture_counts[[toString(year)]]  # Filter moisture_counts after year
    for (SAD_count in 1:length(return_data_frame_year)) { # Go over all SADs and create SAD_ID and Year for each SAD and year
      return_data_frame_year[[SAD_count]][["Year"]] <- year
      return_data_frame_year[[SAD_count]][["SAD_ID"]] <- SAD_count
    }
    return_list_year[[toString(year)]] <- Reduce(function(df1,df2) rbind(df1,df2), return_data_frame_year)  # Combine all Years, SAD_IDs 
    
  }
  return_data_frame <- Reduce(function(df1,df2) rbind(df1,df2), return_list_year)
  
  return(return_data_frame)
}


MergeMoistureAndCrops <- function(grouped_crops, moisture_counts_prepared) {
  
  SAD_moisture_crop_class <- merge(moisture_counts_prepared, grouped_crops, by = c("Year", "SAD_ID"), all.y = T)  # Merge SAD's according to the grouped SAD and years
  
  SAD_moisture_crop_class_return <- SAD_moisture_crop_class |>
    group_by(Week, Interval, class) |>
    pivot_wider(id_cols = c("Week", "Interval"), names_from = class, values_from = Moisture_Count, values_fn = sum)  # Create Wide format and return
  
  return(SAD_moisture_crop_class_return)
}


iterativeCS_wheat <- CreateIterativCS(crop =  "Wheat_yield", 
                                           Saskatchewan_cropyield_to_merge, 
                                           SAD_moisture_filled, 
                                           2010:2022, 
                                           c(0,52.4,0.2), 
                                           c(0.25,0.75))
iterativeCS_canola <- CreateIterativCS(crop =  "Canola_yield", 
                                           Saskatchewan_cropyield_to_merge, 
                                           SAD_moisture_filled, 
                                           2010:2022, 
                                           c(0,52.4,0.2), 
                                           c(0.25,0.75))

