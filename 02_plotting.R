library(tidyverse)
library(acled.api)
library(gghdx)
library(patchwork)
library(sf)
library(ggrepel)
library(ggsankey)
gghdx()

som_dir <- Sys.getenv("SOM_ANALYSIS_DIR")
data_dir <- file.path(som_dir, "data")
plot_dir <- file.path(som_dir, "plots")

df_som <- read_csv(
  file.path(
    data_dir,
    "somalia_all_data.csv"
  )
)

df_water_price <- read_csv(
  file.path(
    data_dir,
    "som_water_prices.csv"
  )
)

df_ipc <- read_csv(
  file.path(
    data_dir,
    "ipc_complete_data.csv"
  )
) %>%
  filter(
    country == "Somalia"
  )

df_conflict_adm2 <- read_csv(
  file.path(
    data_dir,
    "som_conflict_adm2.csv"
  )
)

df_chirps <- read_csv(
  file.path(
    data_dir,
    "chirps",
    "Somalia_CHIRPSData_byDistrict_2000_2022.csv"
  )
)

#################################
#### ADMIN DATA FOR PLOTTING ####
#################################

sf_adm1 <- read_sf(
  file.path(
    data_dir,
    "cod_ab",
    "som_adm_undp_shp",
    "Som_Admbnda_Adm1_UNDP.shp"
  )
)

sf_adm2 <- read_sf(
  file.path(
    data_dir,
    "cod_ab",
    "som_adm_undp_shp",
    "Som_Admbnda_Adm2_UNDP.shp"
  )
)

###################
#### IPC PLOTS ####
###################

p_p3_pct <- ggplot() +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(`phase_p3+_pct_current`),
        !ipc_interpolated_current
      ),
    mapping = aes(
      x = date,
      y = `phase_p3+_pct_current`
    ),
    color = hdx_hex("gray-black")
  ) +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(`phase_p3+_pct_proj`),
        !ipc_interpolated_proj
      ),
    mapping = aes(
      x = date,
      y = `phase_p3+_pct_proj`
    ),
    color = hdx_hex("tomato-hdx")
  ) +
  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = c(0.45, 0.333),
      label = c("Projected", "Current"),
      color = hdx_hex(c("tomato-hdx", "gray-black"))
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label,
      color = color
    ),
    fontface = "bold"
  ) +
  scale_color_identity() +
  labs(
    x = "",
    y = "% of population",
    title = "Population in phase 3+"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  )

p_p4_pct <- ggplot() +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(`phase_p4+_pct_current`),
        !ipc_interpolated_current
      ),
    mapping = aes(
      x = date,
      y = `phase_p4+_pct_current`
    ),
    color = hdx_hex("gray-black")
  ) +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(`phase_p4+_pct_proj`),
        !ipc_interpolated_proj
      ),
    mapping = aes(
      x = date,
      y = `phase_p4+_pct_proj`
    ),
    color = hdx_hex("tomato-hdx")
  ) +
  scale_color_identity() +
  labs(
    x = "",
    y = "% of population",
    title = "Population in phase 4+"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  )

p_p5_pct <- ggplot() +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(phase_5_pct_current),
        !ipc_interpolated_current
      ),
    mapping = aes(
      x = date,
      y = phase_5_pct_current
    ),
    color = hdx_hex("gray-black")
  ) +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(phase_5_pct_proj),
        !ipc_interpolated_proj
      ),
    mapping = aes(
      x = date,
      y = phase_5_pct_proj
    ),
    color = hdx_hex("tomato-hdx")
  ) +
  scale_color_identity() +
  labs(
    x = "",
    y = "% of population",
    title = "Population in phase 5",
    caption = "Data from the IPC, https://www.ipcinfo.org"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  )

# put IPC plots together

p_ipc <- p_p3_pct + p_p4_pct + p_p5_pct + 
  plot_annotation(title = 'IPC phases, 2017 - 2022',
                  subtitle = "Historical current and first projections",
                  theme = theme(
                    plot.title = element_text(size = 24),
                    plot.subtitle = element_text(size = 18)
                  ))

ggsave(
  filename = file.path(plot_dir, "ipc.png"),
  plot = p_ipc,
  height = 4,
  width = 12,
  units = "in"
)

##################
#### CONFLICT ####
##################

df_conflict_month <- df_som %>%
  filter(
    !is.na(fatalities)
  ) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  group_by(
    year, month
  ) %>%
  summarize(
    fatalities = sum(fatalities),
    events_battles = sum(events_battles),
    events_vac = sum(events_vac),
    events = sum(events),
    .groups = "drop"
  )

df_conflict_group <- df_conflict_month %>%
  filter(
    year != 2022
  ) %>%
  group_by(
    month
  ) %>%
  summarize(
    min_battles = min(events_battles),
    max_battles = max(events_battles),
    min_fatalities = min(fatalities),
    max_fatalities = max(fatalities),
    min_events = min(events),
    max_events = max(events),
    .groups = "drop"
  )

# fatalities since August

# monthly plotting

p_conflict <- ggplot(
  mapping = aes(
    x = month
  )
) +
  geom_ribbon(
    data = df_conflict_group,
    mapping = aes(
      ymin = min_fatalities - 10,
      ymax = max_fatalities + 10
    ),
    alpha = 0.3,
    fill = hdx_hex("tomato-hdx")
  ) +
  geom_line(
    data = df_conflict_month %>% filter(year == 2022, month <= 11),
    mapping = aes(
      y = fatalities
    ),
    color = hdx_hex("tomato-hdx"),
    lwd = 2
  ) +
  geom_text(
    data = data.frame(
      x = 12,
      y = 948,
      label = 2022
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label
    ),
    fontface = "bold",
    size = 5,
    color = hdx_hex("tomato-hdx"),
    hjust = 1
  ) +
  geom_text(
    data = data.frame(
      x = c(12, 1.75, 1.75),
      y = c(300, 450, 100),
      label = c("2010 - 2021", "Max", "Min")
    ),
    aes(
      x = x,
      y = y,
      label = label
    ),
    fontface = "bold",
    size = 5,
    color = "white",
    hjust = 1
  ) +
  labs(
    title = "Monthly reported fatalities due to violence, 2010 - 2022",
    subtitle = "2022 against max and min observed 2010 - 2021",
    caption = "Data from ACLED, https://acleddata.com",
    x = "",
    y = "Fatalities (monthly total)"
  ) +
  scale_x_continuous(
    breaks = seq(3, 12, 3),
    labels = month.name[seq(3, 12, 3)]
  )

ggsave(
  filename = file.path(plot_dir, "conflict.png"),
  plot = p_conflict,
  height = 5,
  width = 8,
  units = "in"
)

######################
#### DISPLACEMENT ####
######################

# not saved out as not really showing much usefulness for our narrative

df_som %>%
  filter(
    !is.na(displacement_total)
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = displacement_total
    )
  )

#####################
#### WATER PRICE ####
#####################

df_water_change_ts <- df_water_price %>%
  filter(
    month >= "2020-01-01"
  ) %>%
  group_by(
    Region,
    District
  ) %>%
  mutate(
    water_price = water_price / water_price[1]
  ) %>%
  filter(
    !is.na(water_price)
  ) %>%
  mutate(
    highlight = water_price[month == max(month)] > 2,
    District = case_when(
      District == "Garoowe" ~ "Garowe",
      District == "Baydhaba" ~ "Baidoa",
      TRUE ~ District
    )
  )

df_water_end <- df_water_change_ts %>%
  filter(
    month == max(month),
    highlight
  ) %>%
  mutate(
    month = month + lubridate::days(10),
    water_price = case_when(
      District == "Caluula" ~ 1.9,
      District == "Cabudwaaq" ~ 2.1,
      District == "Garowe" ~ 2.95,
      TRUE ~ water_price
    )
  )
  
p_water <- df_water_change_ts %>%
  ggplot(
    aes(
      x = month,
      y = water_price
    )
  ) +
  geom_line(
    aes(
      group = District,
      alpha = highlight
    )
  ) +
  geom_text_hdx(
    data = df_water_end,
    aes(
      label = District
    ),
    hjust = 0,
    color = hdx_hex("sapphire-hdx")
  ) +
  labs(
    x = "",
    y = "Price of water (relative to January 2020)",
    title = "Change in price of water, January 2020 to October 2022",
    caption = "Data from FSNAU, https://dashboard.fsnau.org"
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(r = 0.5, unit = "in")
  ) +
  coord_cartesian(
    clip = "off"
  )
  
ggsave(
  filename = file.path(plot_dir, "water.png"),
  plot = p_water,
  height = 5,
  width = 8,
  units = "in"
)

###################################
#### CONFLICT, PRICES, AND IPC ####
###################################

df_conf_sum <- df_conflict_adm2 %>%
  filter(
    date >= "2022-01-01"
  ) %>%
  group_by(
    admin1, admin2
  ) %>%
  summarize(
    events = sum(events),
    fatalities = sum(fatalities),
    .groups = "drop"
  )

df_water_change <- df_water_price %>%
  filter(
    month %in% as.Date(c("2020-01-01" , "2022-10-01"))
  ) %>%
  group_by(
    Region, District
  ) %>%
  filter(
    !any(is.na(water_price))
  ) %>%
  arrange(
    month,
    .by_group = TRUE
  ) %>%
  summarize(
    water_price = water_price[2] / water_price[1],
    .groups = "drop"
  ) %>%
  rename(
    admin1 = Region,
    admin2 = District
  )
  
df_ipc_rec <- df_ipc %>%
  rename(
    admin1 = area
  ) %>%
  group_by(
    admin1
  ) %>%
  filter(
    is.na(level_1_name),
    analysis_type == "first_projection",
    analysis_period_start == max(analysis_period_start)
  ) %>%
  ungroup() %>%
  select(
    admin1,
    matches("phase_(.*)")
  ) %>%
  mutate(
    admin1 = case_when(
      admin1 == "Juba dhexe" ~ "Middle Juba",
      admin1 == "Juba hoose" ~ "Lower Juba",
      admin1 == "Shabelle dhexe" ~ "Middle Shabelle",
      admin1 == "Shabelle hoose" ~ "Lower Shabelle",
      admin1 == "Woqooyi galbeed" ~ "Woqooyi Galbeed",
      TRUE ~ admin1
    )
  )

full_join(df_conf_sum, df_water_change, by = c("admin1", "admin2")) %>%
  left_join(
    df_ipc_rec,
    by = "admin1"
  ) %>%
  filter(
    !is.na(events),
    !is.na(phase_5_pct)
  ) %>%
  ggplot() +
  geom_point(
    aes(
      x = water_price,
      y = events,
      color = phase_5_num
    )
  ) +
  scale_color_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx")
  )

#################
#### MAPPING ####
#################

sf_p5 <- df_ipc_rec %>%
  select(
    admin1,
    `phase_p3+_pct`,
    phase_5_pct,
    phase_5_num
  ) %>%
  left_join(
    sf_adm1,
    by = c("admin1" = "admin1Name")
  ) %>%
  st_as_sf()

# points of conflict

df_conflict_points <- acled.api(
  email.address = Sys.getenv("EMAIL_ADDRESS_WORK"),
  access.key = Sys.getenv("ACLED_API_KEY"),
  country = "Somalia",
  start.date = "2022-01-01",
  end.date = "2022-12-12",
  all.variables = TRUE
) %>%
  mutate(
    date = as.Date(event_date),
    size = 1
  ) %>%
  filter(
    event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence")
  )

sf_conflict_points <- df_conflict_points %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = st_crs(sf_p5)
  )

# areas with high water price increases

sf_water_change <- df_water_change %>%
  filter(
    water_price > 2
  ) %>%
  mutate(
    linetype = "dashed"
  ) %>%
  left_join(
    sf_adm2,
    by = c("admin1" = "admin1Name", "admin2" = "admin2Name")
  ) %>%
  st_as_sf()

arrow_df <- data.frame(
  x = 44.5,
  xend = c(43.4, 44.18, 47.95),
  y = 6.5,
  yend = c(3.77, 4.6, 8.16)
)

p_ipc_conflict_water <- ggplot() +
  geom_sf(
    data = sf_p5,
    aes(
      fill = phase_5_pct
    ),
    lwd = 0.1
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    labels = scales::percent_format()
  ) +
  geom_sf(
    data = sf_water_change,
    mapping = aes(
      linetype = linetype
    ),
    fill = NA
  ) +
  scale_linetype_identity(
    name = "Abnormal\nwater prices",
    label = ""
  ) +
  geom_sf(
    data = sf_conflict_points,
    mapping = aes(
      size = size
    ),
    pch = 21,
    alpha = 0.1,
    color = "black"
  ) +
  geom_segment(
    data = arrow_df,
    mapping = aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    color = "#444444",
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_label_hdx(
    data = data.frame(
      x = 44.5,
      y = 6.5,
      label = "Districts with high\nwater price increases"
    ),
    aes(
      x = x,
      y = y,
      label = label
    ),
    fill = "#444444",
    color = "white",
    label.size = 0,
    fontface = "bold",
    size = 2.7
  ) +
  scale_size_identity(
    guide = "legend",
    name = "Violent\nevent",
    label = ""
  ) +
  coord_sf(
    datum = NA
  ) +
  guides(
    size = guide_legend(override.aes = list(alpha = 1))
  ) +
  labs(
    title = "IPC projections, conflict, and water prices",
    subtitle = "IPC Phase 5 projections October to December 2022; all violent events in 2022",
    fill = "P5 population\n% of total",
    caption = "Conflict data from ACLED, https://acleddata.com\nWater price data from FSNAU, https://dashboard.fsnau.org\nIPC data from the IPC, https://www.ipcinfo.org",
    x = "",
    y = ""
  )

ggsave(
  filename = file.path(
    plot_dir,
    "ipc_conflict_water_map.png"
  ),
  plot = p_ipc_conflict_water,
  width = 7,
  height = 8
)

##################
#### RAINFALL ####
##################

df_chirps_plot <- df_chirps %>%
  group_by(
    Year,
    Season_Yr
  ) %>%
  summarize(
    rf = mean(x = ssn_total),
    .groups = "drop"
  ) %>%
  mutate(
    year = ifelse(Season_Yr == "MAM", paste(Year - 1, Year, sep = " - "), paste(Year, Year + 1, sep = " - ")),
  ) %>%
  filter(
    !(year %in% c("1999 - 2000", "2022 - 2023"))
  ) %>%
  select(
    -Year
  ) %>%
  pivot_wider(
    names_from = Season_Yr,
    values_from = rf
  ) 

df_chirps_plot_4_yr <- df_chirps_plot %>%
  filter(
    year %in% c(
      "2018 - 2019",
      "2019 - 2020",
      "2020 - 2021",
      "2021 - 2022"
    )
  )

df_chirps_2011 <- df_chirps_plot %>%
  filter(
    year == "2010 - 2011"
  )

p_chirps <- df_chirps_plot %>%
  ggplot(
    mapping = aes(
      x = OND,
      y = MAM
    )
  ) +
  geom_point(
    color = hdx_hex("gray-dark"),
  ) +
  geom_text_hdx(
    data = df_chirps_plot_4_yr,
    mapping = aes(
      label = year
    ),
    nudge_y = -6,
    size = 3,
    color = hdx_hex("tomato-hdx")
  ) +
  geom_text_hdx(
    data = df_chirps_2011,
    mapping = aes(
      label = year
    ),
    nudge_y = -6,
    size = 3,
    color = hdx_hex("sapphire-hdx")
  ) +
  geom_point(
    data = df_chirps_plot_4_yr,
    color = hdx_hex("tomato-hdx"),
    size = 4
  ) +
  geom_point(
    data = df_chirps_2011,
    size = 4,
    color = hdx_hex("sapphire-hdx")
  ) +
  geom_segment(
    x = 90,
    xend = 65,
    y = 85,
    yend = 85,
    arrow = arrow(length = unit(0.1, "inches")),
    color = hdx_hex("tomato-hdx")
  ) +
  geom_text_hdx(
    x = 92,
    y = 85,
    label = "Last 2 seasons",
    color = hdx_hex("tomato-hdx"),
    hjust = 0,
    fontface = "bold",
    check_overlap = TRUE
  ) +
  geom_text_hdx(
    x = 95,
    y = 175,
    label = "Each dot represents 2 seasons:\nOND one year, and MAM the following",
    color = hdx_hex("gray-dark"),
    hjust = 0,
    fontface = "bold",
    check_overlap = TRUE
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    x = "October - December",
    y = "March - May",
    title = "Average district rainfall, millimeters, 2000-2022",
    subtitle = "October - December and March - May seasons",
    caption = "Data from CHIRPS, https://chc.ucsb.edu/data/chirps"
  )

ggsave(
  filename = file.path(plot_dir, "chirps.png"),
  plot = p_chirps,
  height = 5,
  width = 8,
  units = "in"
)

############################
#### CHIRPS ALTERNATIVE ####
############################

df_chirps2 <- df_chirps %>%
  group_by(
    Year,
    Season_Yr
  ) %>%
  summarize(
    rf = mean(x = ssn_total),
    .groups = "drop"
  ) %>%
  mutate(
    Year_Label = ifelse(
      Season_Yr == "OND",
      Year + 0.5,
      Year
    )
  )

p_chirps2 <- df_chirps2 %>%
  filter(
    Year_Label <= 2020
  ) %>%
  ggplot(
    aes(
      x = Year_Label,
      y = rf
    )
  ) +
  geom_point() +
  geom_point(
    data = filter(
      df_chirps2,
      Year_Label > 2020
    ),
    size = 3,
    color = hdx_hex("tomato-hdx")
  ) +
  geom_point(
    data = filter(
      df_chirps2,
      Year_Label == 2010.5
    ),
    size = 3,
    color = hdx_hex("gray-dark")
  ) +
  geom_text_hdx(
    x = 2010.5,
    y = 54,
    label = "2011",
    fontface = "bold",
    check_overlap = TRUE,
    size = 5
  ) +
  geom_text_hdx(
    x = 2021,
    y = 100,
    label = "Last 4 seasons",
    fontface = "bold",
    size = 5,
    color = hdx_hex("tomato-hdx"),
    check_overlap = TRUE
  ) +
  labs(
    x = "Year (March - May and October - December seasons)",
    y = "Average rainfall (mm)",
    title = "Average seasonal rainfall, Somalia, 2000 - 2022"
  )

ggsave(
  filename = file.path(plot_dir, "chirps_alt.png"),
  plot = p_chirps2,
  height = 4,
  width = 8,
  units = "in"
)


####################
#### CHIRPS MAP ####
####################

p_chirps_map <- df_chirps %>%
  group_by(
    admin2Name, Season_Yr
  ) %>%
  mutate(
    rf_anom = ssn_total / mean(ssn_total)
  ) %>%
  filter(
    Season == "MAM2022"
  ) %>%
  select(
    admin2Name, rf_anom
  ) %>%
  left_join(
    sf_adm2,
    by = "admin2Name"
  ) %>%
  ungroup() %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    aes(
      fill = rf_anom
    )
  ) +
  scale_fill_gradient(
    low = hdx_hex("sapphire-hdx"),
    high = "white",
    labels = scales::label_percent()
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    fill = "Rainfall anomaly (%)",
    title = "Rainfall anomaly, % of mean seasonal rainfall, 2000 - 2022",
    subtitle = "March to May season, 2022",
    caption = "Data from CHIRPS, https://chc.ucsb.edu/data/chirps"
  )

ggsave(
  filename = file.path(
    plot_dir,
    "chirps_map.png"
  ),
  plot = p_chirps_map,
  width = 7,
  height = 8
)

######################
#### WATER CHANGE ####
######################

df_water_change %>%
  left_join(
    sf_adm2,
    by = c("admin1" = "admin1Name", "admin2" = "admin2Name")
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    aes(
      fill = water_price
    )
  ) +
  scale_fill_gradient2(
    low = hdx_hex("mint-hdx"),
    mid = "white",
    high = hdx_hex("tomato-hdx"),
    midpoint = 1
  )

#################
#### IPC MAP ####
#################


p_ipc_map <- sf_p5 %>%
  ggplot() +
  geom_sf(
    aes(
      fill = `phase_p3+_pct`
    )
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("tomato-hdx"),
    labels = scales::label_percent()
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    fill = "% of population\nin phase 3+",
    title = "Projected percent of population in IPC phase 3+",
    subtitle = "Projected: October to December 2022",
    caption = "Data from the IPC, https://www.ipcinfo.org"
  )

ggsave(
  filename = file.path(
    plot_dir,
    "ipc_map.png"
  ),
  plot = p_ipc_map,
  width = 7,
  height = 8
)

################################
#### CONFLICT DISAGGREGATED ####
################################

df_conflict_all <- acled.api(
  email.address = Sys.getenv("EMAIL_ADDRESS_WORK"),
  access.key = Sys.getenv("ACLED_API_KEY"),
  country = "Somalia",
  start.date = "2010-01-01",
  end.date = "2022-12-12",
  all.variables = TRUE
) %>%
  mutate(
    date = as.Date(event_date)
  )

df_sub_event <- df_conflict_all %>%
  filter(
    lubridate::year(date) >= 2022
  ) %>%
  group_by(
    event_type,
    sub_event_type
  ) %>%
  summarize(
    n = n(),
    fatalities = sum(fatalities, na.rm = TRUE),
    .groups = "drop"
  ) 

sub_event_fct <- df_sub_event %>%
  arrange(
    n
  ) %>%
  pull(sub_event_type) %>%
  as.factor

df_conflict_se_month <- df_conflict_all %>%
  filter(
    !is.na(fatalities),
    !is.na(sub_event_type)
  ) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date),
    sub_event_type = factor(sub_event_type, levels = sub_event_fct)
  ) %>%
  group_by(
    year, month, sub_event_type
  ) %>%
  summarize(
    fatalities = sum(fatalities),
    .groups = "drop"
  ) %>%
  complete(
    year,
    month,
    sub_event_type,
    fill = list(
      fatalities = 0
    )
  )

df_conflict_se_group <- df_conflict_se_month %>%
  filter(
    year != 2022
  ) %>%
  group_by(
    month,
    sub_event_type
  ) %>%
  summarize(
    min_fatalities = min(fatalities),
    max_fatalities = max(fatalities),
    .groups = "drop"
  )

# monthly plotting

p_conflict_se <- ggplot(
  mapping = aes(
    x = month
  )
) +
  geom_ribbon(
    data = df_conflict_se_group,
    mapping = aes(
      ymin = min_fatalities - 10,
      ymax = max_fatalities + 10
    ),
    alpha = 0.3,
    fill = hdx_hex("tomato-hdx")
  ) +
  geom_line(
    data = df_conflict_se_month %>% filter(year == 2022, month < 11),
    mapping = aes(
      y = fatalities
    ),
    color = hdx_hex("tomato-hdx"),
    lwd = 2
  ) +
  labs(
    title = "Monthly reported fatalities due to violence",
    subtitle = "2022 against max and min observed 2010 - 2021",
    caption = "Data from ACLED, https://acleddata.com",
    x = "",
    y = "Fatalities (monthly total)"
  ) +
  scale_x_continuous(
    breaks = seq(3, 12, 3),
    labels = month.name[seq(3, 12, 3)]
  ) +
  facet_wrap(
    ~sub_event_type
  )

df_sub_event %>%
  mutate(
    event = case_when(
      event_type == "Battles" ~ "Battles",
      sub_event_type == "Shelling/artillery/missile attack" ~ "Shelling/artillery/missile attack",
      sub_event_type == "Suicide bomb" ~ "Suicide bomb",
      sub_event_type == "Air/drone strike" ~ "Air/drone strike",
      event_type == "Violence against civilians" ~ "Attack on civilians",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(
    event
  ) %>%
  summarize(
    fatalities = sum(fatalities)
  ) %>%
  arrange(
    fatalities
  ) %>%
  mutate(
    event = factor(event, levels = event)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = event,
      y = fatalities
    ),
    stat = "identity",
    fill = hdx_hex("tomato-hdx")
  ) +
  coord_flip() +
  labs(
    y = "Fatalities (total 2022)",
    x = "",
    title = "Causes of reported fatalities due to violence, Somalia, 2022"
  )
