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
file_path <- paste0(Sys.getenv("AA_DATA_DIR"), "/public/raw/som/chirps/")
file_list <- list.files(file_path, pattern = "som_chirps_monthly")
somSF <- st_read(file.path(som_dir, "/data/cod_ab/som_cod_ab.shp.zip"), 
                 layer = "Som_Admbnda_Adm2_UNDP")
somalia_adm2_chirps_intersection <- read_csv(file.path(som_dir, "/data/grid_intersections/somalia_adm2_chirps_intersection.csv"))

#####
# setting values
start_yr <- 2000; end_yr <- 2022
years_interest <- seq(start_yr, end_yr, 1)
mam_months <- c("03", "04", "05")
ond_months <- c("10", "11", "12")
consec_seasons <- c("MAM2020", "OND2020", "MAM2021", "OND2021", "MAM2022")
sel_seasons <- c("MAM2011", "MAM2017", "MAM2022", "OND2011", "OND2017")


sel_files <- as.vector(outer(years_interest, c(mam_months, ond_months), paste, sep="_"))
som_files <- file_list[grepl(paste(sel_files, collapse = "|"), file_list)]

#####
# reading in chirps files
read_files <- function(filePath, fileName){
  ncObj <- nc_open(paste0(filePath, fileName))
  DataCube <- ncvar_get(ncObj, "precipitation") %>%
    data.frame() %>%
    bind_cols(paste0(round(ncvar_get(ncObj, "X") + 0.025, 3), 
                     ifelse(round(ncvar_get(ncObj, "X") + 0.025, 3) > 0, "E", "W"))) %>%
    setNames(c(paste0(round(ncvar_get(ncObj, "Y"), 3), 
                      ifelse(round(ncvar_get(ncObj, "Y"), 3) > 0, "N", "S")), 
               "Longitude")) %>%
    gather(key = "Latitude", value = "precipitation", -Longitude) %>%
    mutate(Date = str_extract(ncObj$filename,"(?<=monthly_).+(?=_r0.05)"))
  nc_close(ncObj)
  return(DataCube)
}

chirpsData <- som_files %>%
  map_df(~ read_files(file_path, .)) 

write_csv(chirpsData, file.path(som_dir, "/data/chirps/SomaliachirpsData.csv"))
