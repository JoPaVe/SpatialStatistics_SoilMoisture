library(terra)
library(ggplot2)
library(dplyr)


############## Load TIF Pictures (SMOS data) ###############

path <- paste(getwd(),"/data",sep="")
file_type = NULL

load_tif_pictures <- function(initial_path, file_type) {
  ## Load all tif pictures and store them in a list of data frames for each year
  ## Output -> list(year): dataframe(x,y,date1,date2,....)
  
  files_vec <- extract_files_vector(initial_path, file_type = NULL) #Vector of files in directory
  list_dates_data <- create_list_dates_data(files_vec)
  return(list_dates_data)
}

extract_files_vector <- function(initial_path, file_type = NULL) {
  files <- list.files(initial_path, recursive = T, full.names = T)
  if (!is.null(file_type))
    files <- files[grep(file_type, files, ignore.case = TRUE)]
  return(files)
}

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

##########################################################################


load_tif_pictures(path, file_type)

