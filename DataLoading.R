
## Loads shapefiles from >2017 and 2008-2016 (2008_2015)
SADRegions2017 <- st_read("data/small_area_data_regions.gdb", layer = "SADRegionsRDPI_2017")
SADRegions2008_2015 <- st_read("data/small_area_data_regions.gdb", layer = "SADRegionsRDPI_2008_2015")

## Loads cropyields for canola and wheat
cropyield_data <- list(
  canola_cropyields = read.csv(paste(getwd(),"/data/cropyield/canola_cropyields.csv", sep = "")),
  wheat_cropyields = read.csv(paste(getwd(),"/data/cropyield/wheat_cropyields.csv", sep = ""))
)

## Loads soil moisture data (If LoadSoilMoisture(NULL, NULL, NULL) the data is loaded from the provided data folder)
LoadSoilMoisture <- function(initial_path, file_type, years) {
  
  if (is.null(initial_path)) {  # is null if not provided as input
    moisture_objects_data <- lapply(excel_sheets(paste(getwd(),"/data/moisture_data/moisture_objects_data.xlsx", sep = "")), function(year) {
      read_excel(paste(getwd(),"/data/moisture_data/moisture_objects_data.xlsx", sep = ""), sheet = year)  # Loads soil moisture data from provided excel file
    }) 
    names(moisture_objects_data) <- excel_sheets(paste(getwd(),"/data/moisture_data/moisture_objects_data.xlsx", sep = ""))
    moisture_objects_tif <- rast(paste(getwd(),"/data/moisture_data/moisture_objects_tif.tif", sep = ""))  # Loads one TIFF file as representative
    moisture_objects <- list(moisture_objects_data, moisture_objects_tif)
    return(moisture_objects)
  }
  else {
    moisture_objects <- LoadTiffPictures(initial_path, file_type, years)
    return(moisture_objects)
  }
  
  
}


############# Functions for TIFF-Loading #############

LoadTiffPictures <- function(initial_path, file_type, years) {
  ## Load all TIFF pictures and store them in a xlsx file and store a representative TIFF raster object

  files_vec <- ExtractFilesFromFolder(initial_path, file_type = file_type)  # Vector of files in directory
  year_df_list <- ReadFilterFiles(files_vec, years)  #  Returns list of files_lists and rast_objects
  list_dates_data <- MergeDF(year_df_list[[1]])  # Returns list of data_frames
  StoreMoistureDataRaster(list_dates_data)  # Stores the soil moisture data in data folder for use in future
  return(list(list_dates_data, year_df_list[[2]]))
}

####

ReadFilterFiles <- function(files_vec, years) {
  # Creates a list with dataframe for each year with all observations 
  
  files_vec <- files_vec[regmatches(files_vec,regexpr("\\d{4}",files_vec)) %in% as.character(years)]
    
  files_list <- list()
  rast_objects <- list()
    
  for (file_path in files_vec) {
    year_match <- regmatches(file_path,regexpr("\\d{4}",file_path))  # Extract Year and date from common file naming YYYYMMDD
    date_match <- regmatches(file_path,regexpr("\\d{8}",file_path))
    rast_obj <- rast(file_path)
    rast_df <- as.data.frame(rast_obj, xy = T)
    colnames(rast_df)[3] <- date_match
      
    # Filter
    rast_df <- subset(rast_df,(rast_df$y > 48 & rast_df$y < 62) & (rast_df$x < -99 & rast_df$x > -115))  # filter dataset to lower size - reduces x and y coordinates to only entail Saskatchewan
    rast_date <- ymd(date_match) 
    if (month(rast_date) < 5 | month(rast_date) > 9) {
      next  # Remove all files that are for month later than October or earlier than May 
    }
      
    files_list[[year_match]][[date_match]] <- rast_df
    rast_objects[[date_match]] <- rast_obj
  }
  return(list(files_list,rast_objects))
}

MergeDF <- function(files_list) {
  # Combines all dataframes for each year into one dataframe  
  
  year_df_list <- lapply(files_list, function(sublist) {
    year_df <- Reduce(function(df1,df2) merge(df1,df2,by = c("x","y"), all = T), sublist)
    return(year_df)
  })

  return(year_df_list)  
}

StoreMoistureDataRaster <- function(list_date_data) {
  # Store the given list in the data folder for future use
  moisture_xlsx <- createWorkbook()
  for (df in 1:length(list_date_data[[1]])) {
    year <- names(list_date_data[[1]])[df]
    addWorksheet(moisture_xlsx, year)
    writeDataTable(moisture_xlsx, sheet = year, x = moisture_objects[[1]][[df]])  # Creates a xlsx file with every daily observation for each year 
  }
  
  saveWorkbook(moisture_xlsx, file = paste(getwd(),"/data/moisture_data/moisture_objects_data.xlsx", sep = ""))
  
  writeRaster(moisture_objects[[2]][[1]], paste(getwd(),"/data/moisture_data/moisture_objects_tif.tif", sep = ""), filetype = "GTiff", overwrite = T)
}

##########################################################################

############## General Functions ##############

ExtractFilesFromFolder <- function(initial_path, file_type = NULL, recursive_set = T) {
  # Stores all files in a folder structure in a vector
  files <- list.files(initial_path, recursive = recursive_set, full.names = T)
  if (!is.null(file_type))
    files <- files[grep(file_type, files, ignore.case = TRUE)]
  return(files)
}

