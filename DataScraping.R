library(rvest)
library(httr)
library(xml2)
library(plyr)

link <- "https://agriculture.canada.ca/atlas/data_donnees/psssm/data_donnees/tif/absolute/"

extract_all_datafiles <- function(years, start_link) {
  ## Extract all files: 
  ## Arguments:
  ## years        vector containing start and end year (Exp.: c(2010,2022)) 
  ## start_link   string of the start_link
  
  create_sub_folders(years) 
  daily_links <- generate_daily_links(years, start_link)
  download_links_list <- read_download_links(years, daily_links)
  run_downloads(years, download_links_list) 
}

create_sub_folders <- function(years) {
  ## Create subfolders for each year in working directory in ./data/'year'.
  
    for (year in years) {
    dir.create(paste0(getwd(),"/data/",year,sep=""))
  }
  
}

generate_daily_links <- function(years, start_link) {
  ## Create vector of links for the daily data for each year.
  
  
  daily_links <- paste0(link,paste0(years),"/daily/")
  return(daily_links)
}

read_download_links <- function(years, daily_links) {
  ## Create vector of download links for each file that should be downloaded.
  
  return_list <- list()
  
  for (year_count in 1:length(years)) {
    start_page_year <- read_html(daily_links[year_count])
    
    download_endings_all <- html_attr(html_nodes(start_page_year, "a"), "href")
    download_endings_filtered <- download_endings_all[grep("\\.tif$",download_endings_all)]
    download_links_complete <- paste0(daily_links[year_count], download_endings_filtered)
    return_list[[toString(years[year_count])]] <- download_links_complete
    Sys.sleep(runif(1,0.5,2))
    print(toString(years[year_count]))
  }
  return(return_list)
}

run_downloads <- function(years, download_links_list) {
  ## Downloads the files into the respective folder created by create_sub_folders
  years <- seq(years[1],years[2],1)
  lapply(1:length(download_links_list), FUN = function(link_vector_count) {
    sapply(download_links_list[[link_vector_count]], FUN = function(link, folder_year) {
      date <- regmatches(link,regexpr("\\d{8}",link))
      download.file(link,paste0(getwd(),"/data/", folder_year[link_vector_count],"/", date, ".tif",sep=""), mode = "wb")
      Sys.sleep(1)
      print(date)
    }, folder_year = names(download_links_list))
  })
} 

extract_all_datafiles(c(2012,2013,2014), link)

