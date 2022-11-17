library(tidyverse)
library(gghdx)
library(patchwork)
library(sf)
library(ggrepel)
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
      x = as.Date("2021-10-01"),
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
    y = "Phase 3+ (% of population)",
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
    y = "Phase 4+ (% of population)",
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
    y = "Phase 5 (% of population)",
    title = "Population in phase 5",
    caption = "Data from the IPC, https://www.ipcinfo.org"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  )

# put IPC plots together

p_ipc <- p_p3_pct + p_p4_pct + p_p5_pct + 
  plot_annotation(title = 'IPC phases in Somalia',
                  theme = theme(plot.title = element_text(size = 18)))

ggsave(
  filename = file.path(plot_dir, "ipc.png"),
  plot = p_ipc,
  height = 5,
  width = 16,
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
    .groups = "drop"
  )

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
    data = df_conflict_month %>% filter(year == 2022, month < 11),
    mapping = aes(
      y = fatalities
    ),
    color = hdx_hex("tomato-hdx"),
    lwd = 2
  ) +
  geom_text(
    data = data.frame(
      x = 11.1,
      y = 1030,
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
      x = 11.1,
      y = 300,
      label = "2010 - 2021"
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
    title = "Monthly reported fatalities due to violence",
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
    highlight = water_price[month == max(month)] > 2
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
      District == "Garoowe" ~ 2.95,
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
  end.date = "2022-11-16",
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
    guide = "legend",
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
  scale_size_identity(
    guide = "legend",
    name = "Violent\nevent",
    label = ""
  ) +
  guides(
    size = guide_legend(override.aes = list(alpha = 1))
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    title = "Projected P5 populations, conflict, and water prices",
    subtitle = "IPC projections October to December 2022; all violent events in 2022",
    fill = "P5 population\n% of total",
    caption = "Conflict data from ACLED, https://acleddata.com\nWater price data from FSNAU, https://dashboard.fsnau.org\nIPC data from the IPC, https://www.ipcinfo.org"
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
    color = "grey"
  ) +
  geom_text_hdx(
    data = bind_rows(df_chirps_plot_4_yr, df_chirps_2011),
    mapping = aes(
      label = year
    ),
    nudge_y = -6,
    size = 3
  ) +
  geom_point(
    data = df_chirps_plot_4_yr,
    color = hdx_hex("tomato-hdx"),
    size = 4
  ) +
  geom_point(
    data = df_chirps_2011,
    size = 4,
    color = hdx_hex("gray-dark")
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    x = "OND (average rainfall)",
    y = "MAM (average rainfall)",
    title = "Average district rainfall, millimeters",
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
    low = hdx_hex("tomato-hdx"),
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
