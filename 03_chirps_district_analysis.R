#### Analysis of CHIRPS rainfall by district in Somalia

#####
# Reading in required libraries

library(tidyverse)
library(sf)
library(ncdf4)

#####
# setting options
options(scipen = 999999, digits = 22)
som_dir <- Sys.getenv("SOM_ANALYSIS_DIR")
file_path <- paste0(Sys.getenv("AA_DATA_DIR"), 
                    "/public/raw/som/chirps/")
file_list <- list.files(file_path, pattern = "som_chirps_monthly")
somSF <- st_read(paste0(Sys.getenv("AA_DATA_DIR"), "/public/raw/som/cod_ab/som_cod_ab.shp.zip"), 
                 layer = "Som_Admbnda_Adm2_UNDP")
somalia_adm2_chirps_intersection <- read_csv("C:/Users/pauni/Desktop/Work/OCHA/SOM/data/somalia_adm2_chirps_intersection.csv")





