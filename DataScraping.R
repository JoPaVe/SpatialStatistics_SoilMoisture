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
  
  years_seq <- seq(years[1],years[2],1)
  
  for (year in years_seq) {
    dir.create(paste(getwd(),"/data/",year,sep=""))
  }
  
}

generate_daily_links <- function(years, start_link) {
  ## Create vector of links for the daily data for each year.
  
  years <- paste0(seq(years[1],years[2],1))
  daily_links <- paste0(link,years,"/daily/")
  return(daily_links)
}

read_download_links <- function(years, daily_links) {
  ## Create vector of download links for each file that should be downloaded.
  
  return_list <- list()
  years <- seq(years[1],years[2],1)
  
  for (year_count in 1:length(years)) {
    start_page_year <- read_html(daily_links[year_count])
    
    download_endings_all <- html_attr(html_nodes(start_page_year, "a"), "href")
    download_endings_filtered <- download_endings_all[grep("\\.tif$",download_endings_all)]
    download_links_complete <- paste0(daily_links[year_count], download_endings_filtered)
    return_list[[toString(years[year_count])]] <- download_links_complete
    Sys.sleep(3)
    print(toString(years[year_count]))
  }
  return(return_list)
}

run_downloads <- function(years, download_links_list) {
  ## Downloads the files into the respective folder created by create_sub_folders
  years <- seq(years[1],years[2],1)
  lapply(download_links_list, FUN = function(link_vector) {
    sapply(link_vector, FUN = function(link) {
      download.file(link,)
      Sys.sleep(1.5)
    })
  })
  
} 


debug(extract_all_datafiles)
extract_all_datafiles(c(2010,2022),link)
#download.file("https://agriculture.canada.ca/atlas/data_donnees/psssm/data_donnees/tif/absolute/2010/daily/GlobAv_SMUDP2_REPR_650_20100601_025K_M5.tif","downloadedex.tif")

