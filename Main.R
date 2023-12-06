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

############# Data scraping:

# The soil moisture data (SMOS) is retrieved from https://agriculture.canada.ca/atlas/data_donnees/psssm/data_donnees/tif/absolute/.
# The data is stored as daily GeoTIFF images and can be downloaded free of charge from the website.
# As the downloading process would take very long and might be prone to errors the following function (ExtractAllDatafiles) loads all of the pictures in separate folders for each year.
# THIS IS NOT NECESSARY since the soil moisture data is also stored in the data/moisture_data/moisture_objects_data.xlsx file which can be loaded in Data Loading.

#source(paste(getwd(), "/DataScraping.R", sep = ""))

#download_link <- "https://agriculture.canada.ca/atlas/data_donnees/psssm/data_donnees/tif/absolute/"

#ExtractAllDatafiles(c(2010:2022), download_link)

############ Data loading:
# All data that is needed for the analysis can be loaded from the data folder. The Soil moisture data is stored in a xlsx file.
# If the raw TIFF picture need to be reloaded (due to updates or new data) one can run LoadSoilMoisture and specify the loading folder (typically the folders created by ExtractAllDatafiles) and the other inputs.
source(paste(getwd(), "/DataLoading.R", sep = ""))
moisture_objects <- LoadSoilMoisture(initial_path = NULL, file_type = NULL, years = NULL)

############ Data preprocessing:
# Process the data for further analysis
source(paste(getwd(), "/Preprocessing.R", sep = ""))

############ Descriptive summaries:
source(paste(getwd(), "/DescriptiveStatistics.R", sep = ""))