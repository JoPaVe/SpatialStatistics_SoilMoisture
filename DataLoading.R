library(terra)
library(ggplot2)
library(dplyr)
## Add installation of Data


path <- paste(getwd(),"/data",sep="")

extract_files <- function(initial_path, file_type = NULL) {
  files <- list.files(initial_path, recursive = T, full.names = T)
  if (!is.null(file_type))
    files <- files[grep(file_type, files, ignore.case = TRUE)]
  return(files)
}

files_vec <- extract_files(path,".tif")

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


