
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

load_tif_pictures <- function(initial_path, file_type, years) {
  ## Load all tif pictures and store them in a list of data frames for each year
  ## Output -> list(list(year): dataframe(x,y,date1,date2,....), rast_objects): 
  
  files_vec <- extract_files_vector(initial_path, file_type = file_type) #Vector of files in directory
  year_df_list <- create_list_dates_data(files_vec, years) # returns list of files_lists and rast_objects
  list_dates_data <- merge_year_df(year_df_list[[1]]) # returns list of data_frames
  return(list(list_dates_data, year_df_list[[2]]))
}

####

create_list_dates_data <- function(files_vec, years) {
    
    files_vec <- files_vec[regmatches(files_vec,regexpr("\\d{4}",files_vec)) %in% as.character(years)]
    
    files_list <- list()
    rast_objects <- list()
    
    for (file_path in files_vec) {
      year_match <- regmatches(file_path,regexpr("\\d{4}",file_path))
      date_match <- regmatches(file_path,regexpr("\\d{8}",file_path))
      rast_obj <- rast(file_path)
      rast_df <- as.data.frame(rast_obj, xy = T)
      colnames(rast_df)[3] <- date_match
      
      # Filter
      rast_df <- subset(rast_df,(rast_df$y > 48 & rast_df$y < 62) & (rast_df$x < -99 & rast_df$x > -115)) #crop dataset to lower size
      rast_date <- ymd(date_match) 
      if (month(rast_date) < 5 | month(rast_date) > 9) {
        next
      }
      
      files_list[[year_match]][[date_match]] <- rast_df
      rast_objects[[date_match]] <- rast_obj
      print(date_match)
    }
  return(list(files_list,rast_objects))
}

merge_year_df <- function(files_list) {
  year_df_list <- lapply(files_list, function(sublist) {
    year_df <- Reduce(function(df1,df2) merge(df1,df2,by = c("x","y"), all = T), sublist)
    return(year_df)
  })

  return(year_df_list)  
}

####

############## Load small_area_data_regions.gdb ###############

get_SAD_layers <- function(path, spec = c("SAD2017", "SAD2008_2015")) {
  gdb_path <- paste0(path,"/small_area_data_regions.gdb")
  if (spec == "2017") {
    return(st_read(gdb_path, layer = "SADRegionsRDPI_2017"))
  } 
  else {
    return(st_read(gdb_path, layer = "SADRegionsRDPI_2008_2015"))
  }
} 

##########################################################################

############## General Functions ##############

extract_files_vector <- function(initial_path, file_type = NULL, recursive_set = T) {
  files <- list.files(initial_path, recursive = recursive_set, full.names = T)
  if (!is.null(file_type))
    files <- files[grep(file_type, files, ignore.case = TRUE)]
  return(files)
}

