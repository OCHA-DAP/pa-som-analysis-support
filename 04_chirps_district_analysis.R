#### Analysis of CHIRPS rainfall by district in Somalia

#####
# Reading in required libraries

library(tidyverse)
library(sf)

#####
options(scipen = 999999, digits = 22)
som_dir <- Sys.getenv("SOM_ANALYSIS_DIR")
somSF <- st_read(file.path(som_dir, "/data/cod_ab/som_cod_ab.shp.zip"), 
                 layer = "Som_Admbnda_Adm2_UNDP")
somalia_adm2_chirps_intersection <- read_csv(file.path(som_dir, "/data/grid_intersections/somalia_adm2_chirps_intersection.csv"))

#####
# setting values
mam_months <- c("03", "04", "05")
ond_months <- c("10", "11", "12")
consec_seasons <- c("MAM2020", "OND2020", "MAM2021", "OND2021", "MAM2022")
sel_seasons <- c("MAM2011", "MAM2017", "MAM2022", "OND2011", "OND2017")

chirpsData <- read_csv(file.path(som_dir, "/data/chirps/Somalia_CHIRPSData.csv"), 
                       col_types = cols(Longitude = col_character()))
#####
# aggregating data
aggData <- chirpsData %>%
  mutate(Year = str_sub(Date, 1, 4), 
         Month = str_sub(Date, -2), 
         Season_Yr = if_else(Month %in% mam_months, "MAM", 
                             if_else(Month %in% ond_months, "OND","")), 
         Season = paste0(Season_Yr, Year)) %>%
  group_by(Date, Year, Season, Season_Yr, Month) %>%
  summarise(month_ave = mean(precipitation, na.rm = T)) %>%
  group_by(Season, Year, Season_Yr) %>%
  summarise(total = sum(month_ave, na.rm = T))

#####
# plotting MAM/OND for a grid 
grid_mam_ond_plot <- ggplot(aggData, aes(x=Year, y=total, fill=Season_Yr)) + 
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + 
  ggtitle("Average MAM/OND Seasonal Rainfall for a grid across the country") + 
  ylab("Seasonal Rainfall (mm)")

plot(somSF$geometry)

#####
# calculating area of each grid in district
somalia_adm2_chirps_intersection <- somalia_adm2_chirps_intersection %>% 
  mutate(GridPerc = GridArea / DistArea, 
         Centroid = paste0(round(Longitude, 3), "_", round(Latitude,3)))

chirpsData2 <- chirpsData %>%
  mutate(Centroid = paste0(str_sub(Longitude, 1, -2), "_", str_sub(Latitude, 1, -2)))

#####
# summarising info by district
chirps_adm2 <- somalia_adm2_chirps_intersection %>%
  left_join(chirpsData2, by = "Centroid") %>%
  mutate(PercPrec = precipitation * GridPerc) %>%
  group_by(admin2Pcod, Date) %>%
  summarise(adm2prec = sum(PercPrec, na.rm = T)) %>%
  mutate(Year = str_sub(Date, 1, 4), 
         Month = str_sub(Date, -2), 
         Season_Yr = if_else(Month %in% mam_months, "MAM", 
                             if_else(Month %in% ond_months, "OND","")), 
         Season = paste0(Season_Yr, Year)) %>%
  group_by(Season, Year, Season_Yr, admin2Pcod) %>%
  summarise(ssn_total = sum(adm2prec, na.rm = T)) %>%
  left_join(somSF, by = "admin2Pcod")

write_csv(chirps_adm2, file.path(som_dir, "/data/chirps/Somalia_CHIRPSData_byDistrict_2000_2022.csv"))
