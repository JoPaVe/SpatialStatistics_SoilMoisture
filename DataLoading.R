library(terra)
library(ggplot2)
library(dplyr)
library(plyr)
library(sf)
library(parallel)
library(raster)

path <- paste0(getwd(),"/data",sep="")

############## Load crop yield data (Canada Agriculture) ###############

load_cropyield_data <- function(path) {
  cropyield_path <- paste0(path, "/crop_data")
  files_name_vec <- extract_files_vector(cropyield_path, file_type = NULL, recursive_set = F)
  cropyield_list <- load_cropyield_list(files_name_vec)  
  return(cropyield_list)
}

####

load_cropyield_list <- function(files_name_vec) {
  return_list <- list()
  for (file_name in files_name_vec) {
    file_name_current <- regmatches(file_name,regexpr("([a-zA-Z]+)_cropyields",file_name, ignore.case = TRUE))
    return_list[[file_name_current]] <- read.csv(file_name, header = T)
  }
  return(return_list)
}

####

############## Load TIF Pictures (SMOS data) ###############

load_tif_pictures <- function(initial_path, file_type) {
  ## Load all tif pictures and store them in a list of data frames for each year
  ## Output -> list(year): dataframe(x,y,date1,date2,....)
  
  files_vec <- extract_files_vector(initial_path, file_type = file_type) #Vector of files in directory
  list_dates_data <- create_list_dates_data(files_vec)
  return(list_dates_data)
}

####

create_list_dates_data <- function(files_vec) {
  files_list <- list()
  
  ## Rewrite for lapply
  for (file_path in files_vec) {
    year_match <- regmatches(file_path,regexpr("\\d{4}",file_path))
    date_match <- regmatches(file_path,regexpr("\\d{8}",file_path))
    rast_obj <- rast(file_path)
    rast_df <- as.data.frame(rast_obj, xy = T)
    colnames(rast_df)[3] <- date_match
    files_list[[year_match]][[date_match]] <- rast_df
  }
  
  year_df_list <- lapply(files_list, function(sublist) {
    year_df <- Reduce(function(df1,df2) merge(df1,df2,by = c("x","y"),all = T), sublist)
    return(year_df)
  })

  return(year_df_list)  
}

####

############## Load small_area_data_regions.gdb ###############
gdb_path <- paste0(path,"/small_area_data_regions.gdb")

canada_layers <- st_layers(dsn = gdb_path)

SADRegions2017 <- st_read(gdb_path, layer = "SADRegionsRDPI_2017")
SADRegions2008_2015 <- st_read(gdb_path, layer = "SADRegionsRDPI_2008_2015")

##########################################################################

############## General Functions ##############

extract_files_vector <- function(initial_path, file_type = NULL, recursive_set = T) {
  files <- list.files(initial_path, recursive = recursive_set, full.names = T)
  if (!is.null(file_type))
    files <- files[grep(file_type, files, ignore.case = TRUE)]
  return(files)
}

file_type = ".tif"

moisture_data <- load_tif_pictures(path, ".tif")
cropyield_data <- load_cropyield_data(path)
