library(terra)
library(spData)
library(raster)

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(sf)
library(parallel)
library(raster)
library(stringr)
library(lubridate)

library(openxlsx)
library(readxl)

############# Data crawling
source(paste(getwd(), "/DataScraping.R", sep = ""))


#download_link <- "https://agriculture.canada.ca/atlas/data_donnees/psssm/data_donnees/tif/absolute/"

#extract_all_datafiles(c(2021:2022), download_link)


file_storage_path = c("E:/SpacialStatistics_MoistureData/data")

############ Data loading:
source(paste(getwd(), "/DataLoading.R", sep = ""))

############ Data preprocessing:
source(paste(getwd(), "/Preprocessing.R", sep = ""))

