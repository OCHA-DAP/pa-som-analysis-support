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

#####
# plotting MAM/OND by district
dist_mam_ond_plot <- chirps_adm2 %>%
  group_by(Season, Year, Season_Yr) %>%
  summarise(dist_mean = mean(ssn_total, na.rm = T)) %>% 
  ggplot(aes(x=Year, y=dist_mean, fill=Season_Yr)) + 
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + 
  ggtitle("Average MAM/OND Seasonal Rainfall for a district across the country") + 
  ylab("Seasonal Rainfall (mm)") 


#####
# plotting with facet
dist_mam_ond_plot2 <- chirps_adm2 %>%
  group_by(Season, Year, Season_Yr) %>%
  summarise(dist_mean = mean(ssn_total, na.rm = T)) %>% 
  ggplot(aes(x=Year, y=dist_mean, fill = Season_Yr)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Average Seasonal Rainfall for a district across the country") +
  labs(fill="Season") + 
  ylab("Seasonal Rainfall (mm)") + 
  facet_wrap(~Season_Yr) + 
  theme(text = element_text(size = 3.5), legend.key.size = unit(0.25, 'cm'))

ggsave(
  filename = file.path(som_dir, "/plots/dist_mam_ond_chirps.png"),
  plot = dist_mam_ond_plot2,
  height = 700,
  width = 1800,
  units = "px"
)

#####
# plotting maps for the whole time series
dist_mam_ond_plot3 <- ggplot(data = chirps_adm2, aes(geometry = geometry)) + 
  geom_sf(aes(fill = ssn_total), lwd = 0) + 
  ggtitle("Seasonal Rainfall") + 
  labs(fill = "Rainfall (mm)") + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(text = element_text(size = 3.5), legend.key.size = unit(0.25, 'cm')) + 
  scale_fill_stepsn(colours = c("#ff0000", "#ff7f00", "#ffffff", "#64f964", "#1d7407"),
                    breaks = c(0, 100, 140, 180, 220, 400),
                    limit = c(0, 400),
                    na.value = "#BEBEBE") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~Season, ncol = 23)

ggsave(
  filename = file.path(som_dir, "/plots/dist_mam_ond_chirps_all_maps.png"),
  plot = dist_mam_ond_plot3,
  height = 700,
  width = 2500,
  units = "px"
)

#####
# plotting maps for the 2020, 2021 and 2022 seasons
consec_seasonData <- chirps_adm2 %>%
  filter(Season %in% consec_seasons)

dist_mam_ond_plot4 <- ggplot(data = consec_seasonData, aes(geometry = geometry)) + 
  geom_sf(aes(fill = ssn_total), lwd = 0) + 
  ggtitle("Seasonal Rainfall") + 
  labs(fill = "Rainfall (mm)") + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(text = element_text(size = 3.5), legend.key.size = unit(0.25, 'cm')) + 
  scale_fill_stepsn(colours = c("#ff0000", "#ff7f00", "#ffffff", "#64f964", "#1d7407"),
                    breaks = c(0, 100, 140, 180, 220, 400),
                    limit = c(0, 400),
                    na.value = "#BEBEBE") + 
  facet_wrap(~Season)

ggsave(
  filename = file.path(som_dir, "/plots/dist_mam_ond_chirps_consec_maps.png"),
  plot = dist_mam_ond_plot4,
  height = 700,
  width = 1800,
  units = "px"
)
#####
# plotting maps for selected seasons
sel_seasonData <- chirps_adm2 %>%
  filter(Season %in% sel_seasons)

dist_mam_ond_plot5 <- ggplot(data = sel_seasonData, aes(geometry = geometry)) + 
  geom_sf(aes(fill = ssn_total), lwd = 0) + 
  ggtitle("Seasonal Rainfall") + 
  labs(fill = "Rainfall (mm)") + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(text = element_text(size = 3.5), legend.key.size = unit(0.25, 'cm')) + 
  scale_fill_stepsn(colours = c("#ff0000", "#ff7f00", "#ffffff", "#64f964", "#1d7407"),
                    breaks = c(0, 100, 140, 180, 220, 400),
                    limit = c(0, 400),
                    na.value = "#BEBEBE") + 
  facet_wrap(~Season)

ggsave(
  filename = file.path(som_dir, "/plots/dist_mam_ond_chirps_sel_maps.png"),
  plot = dist_mam_ond_plot5,
  height = 700,
  width = 1800,
  units = "px"
)
