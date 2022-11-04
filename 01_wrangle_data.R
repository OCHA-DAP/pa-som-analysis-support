library(acled.api)
library(idmc)
library(rhdx)
library(tidyverse)

som_dir <- Sys.getenv("SOM_ANALYSIS_DIR")
data_dir <- file.path(som_dir, "data")

########################
#### ACLED CONFLICT ####
########################

df_conflict <- acled.api(
  email.address = Sys.getenv("EMAIL_ADDRESS_WORK"),
  access.key = Sys.getenv("ACLED_API_KEY"),
  country = "Somalia",
  start.date = "2010-01-01",
  end.date = "2022-11-01"
) %>%
  mutate(
    date = as.Date(event_date)
  )

# create daily summary data
df_conflict_adm2 <- df_conflict %>%
  filter(
    event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence")
  ) %>%
  group_by(
    admin1, admin2, date
  ) %>%
  summarize(
    events = n(),
    events_battles = sum(event_type == "Battles"),
    events_explosions = sum(event_type == "Explosions/Remote violence"),
    events_vac = sum(event_type == "Violence against civilians"),
    fatalities = sum(fatalities),
    .groups = "drop"
  )

# infill data

df_conflict_adm2_comp <- df_conflict_adm2 %>%
  group_by(
    admin1, admin2
  ) %>%
  complete(
    date = seq(
      from = min(df_conflict$date),
      to = max(df_conflict$date),
      by = "day"
    ),
    fill = list(
      events = 0,
      events_battles = 0,
      events_explosions = 0,
      events_vac = 0,
      fatalities = 0
    )
  )

# create country level data

df_conflict_country <- df_conflict_adm2_comp %>%
  group_by(
    date
  ) %>%
  summarize(
    across(
      .cols = events:fatalities,
      .fns = sum
    ),
    .groups = "drop"
  )

###########################
#### IDMC DISPLACEMENT ####
###########################

df_displacement <- idmc_get_data() %>%
  idmc_transform_daily() %>%
  filter(
    country == "Somalia",
    displacement_type == "Conflict"
  ) %>%
  complete(
    date = seq(min(.data$date), max(.data$date), by = "day"),
    fill = list(
      displacement_daily = 0
    )
  ) %>%
  select(
    date, displacement_daily
  )

#############
#### IPC ####
#############

set_rhdx_config(hdx_site = "prod")
get_rhdx_config()

df <- pull_dataset("ipc-country-data") %>%
  get_resource(1) %>%
  read_resource(
    sheet = "IPC"
  )

# rename the dataset

df_names <- as.data.frame(t(df[3:5,])) %>%
  tidyr::fill(
    tidyr::everything()
  ) %>%
  tidyr::unite(
    col = "names",
    sep = "_",
    na.rm = TRUE
  )

names(df) <- df_names$names %>%
  tolower() %>%
  stringr::str_replace_all(
    c(
      " " = "_",
      "#" = "num",
      "%" = "pct"
    )
  )

# adjust the data for Somalia since it's stored in a different way
som_districts <- c(
  "Awdal",
  "Bakool",
  "Banadir",
  "Bari",
  "Bay",
  "Galgaduud",
  "Gedo",
  "Hiraan",
  "Juba dhexe",
  "Juba hoose",
  "Mudug",
  "Nugaal",
  "Sanaag",
  "Shabelle dhexe",
  "Shabelle hoose",
  "Sool",
  "Togdheer",
  "Woqooyi galbeed"
)

df <- df %>%
  dplyr::mutate(
    "area" := ifelse(.data$country %in% som_districts & .data$date_of_analysis %in% c("Aug 2021", "Jan 2022", "Mar 2022", "May 2022", "Aug 2022"), som_districts, .data$area),
    "country" := ifelse(.data$country %in% som_districts & .data$date_of_analysis %in% c("Aug 2021", "Jan 2022", "Mar 2022", "May 2022", "Aug 2022"), "Somalia", .data$country)
  )


# filter and wrangle the dataset

# pivot so that each row is a phase/area and columns are variables for that
# specific analysis period (e.g. current or projection)
df_piv <- df %>%
  dplyr::slice(
    -c(1:5)
  ) %>%
  dplyr::filter(
    !is.na(.data[["area"]])
  ) %>%
  tidyr::pivot_longer(
    cols = tidyr::matches("^current|^first|^second"),
    names_to = c("analysis_type", "name"),
    names_pattern = "(^current|^first_projection|^second_projection)_(.*)"
  ) %>%
  tidyr::pivot_wider()

# cleaning up columns %>%
df_clean <- df_piv %>%
  dplyr::select(
    -c(
      "country_population",
      "population_analysed_pct_of_total_county_pop"
    )
  ) %>%
  dplyr::filter(
    !is.na(.data[["phase_1_num"]])
  ) %>%
  dplyr::rename(
    "population" := "population_analysed_num",
    "area_phase" := "population_analysed_area_phase",
    "analysis_period" := "population_analysed_analysis_period"
  ) %>%
  readr::type_convert() %>%
  dplyr::mutate(
    analysis_period_start = lubridate::dmy(
      x = paste(
        "15",
        stringr::str_extract(
          .data[["analysis_period"]],
          "(.*) - "
        )
      )
    ),
    analysis_period_end = lubridate::dmy(
      x = paste(
        "15",
        stringr::str_extract(
          .data[["analysis_period"]],
          " - (.*)$"
        )
      )
    ),
    .after = "analysis_period"
  )

# turn data into daily data

df_daily <- df_clean %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    date = list(
      seq(.data$analysis_period_start, .data$analysis_period_end, by = "day")
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(
    cols = "date"
  ) %>%
  dplyr::group_by(
    country, analysis_type, date
  ) %>%
  dplyr::summarize(
    across(
      .cols = c(population, ends_with("_num")),
      .fns = sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    `phase_p4+_num` = phase_4_num + phase_5_num,
    across(
      .cols = ends_with("_num"),
      .fns = ~ .x / population,
      .names = "{.col}_pct"
    )
  ) %>%
  dplyr::rename_with(
    .cols = ends_with("_pct"),
    .fn = ~ str_remove(.x, "_num")
  )

# impute data for the missing dates

df_ipc_complete <- df_daily %>%
  dplyr::group_by(
    country, analysis_type
  ) %>%
  tidyr::complete(
    date = seq(min(.data$date), max(.data$date), by = "day")
  ) %>%
  mutate(
    ipc_interpolated = is.na(phase_1_num),
    .after = date
  ) %>%
  mutate(
    across(
      .cols = population:`phase_p3+_pct`,
      .fns = zoo::na.approx
    )
  ) %>%
  dplyr::ungroup()

# get somalia IPC data for joining up

df_ipc_som_curr <- df_ipc_complete %>%
  filter(
    analysis_type == "current",
    country == "Somalia"
  ) %>%
  select(
    -analysis_type
  ) %>%
  rename_with(
    .cols = ipc_interpolated:`phase_p4+_pct`,
    .fn = ~paste0(.x, "_current")
  )

df_ipc_som_proj <- df_ipc_complete %>%
  filter(
    analysis_type == "first_projection",
    country == "Somalia"
  ) %>%
  select(
    -analysis_type
  ) %>%
  rename_with(
    .cols = ipc_interpolated:`phase_p4+_pct`,
    .fn = ~paste0(.x, "_proj")
  )

df_ipc_som <- full_join(
  df_ipc_som_curr,
  df_ipc_som_proj,
  by = c("country", "date")
)

####################
#### PRICE DATA ####
####################

df_prices <- pull_dataset("wfp-food-prices-for-somalia") %>%
  get_resource(1) %>%
  read_resource()

# normalize price data for analysis

df_prices_final <- df_prices %>%
  group_by(
    commodity,
    unit,
    date
  ) %>%
  summarize(
    usdprice = mean(usdprice, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  complete(
    date = seq(min(date), max(date), by = "day")
  ) %>%
  ungroup() %>%
  mutate(
    usdprice = zoo::na.approx(usdprice),
    across(
      .cols = c(commodity, unit),
      .fns = ~ str_replace_all(
        .x,
        c(
          " |\\/" = "_",
          "\\(|\\)|\\," = ""
        )
      ) %>% tolower()
    ),
    commodity_unit = paste(commodity, unit, sep = "_")
  ) %>%
  pivot_wider(
    id_cols = date,
    names_from = commodity_unit,
    values_from = usdprice
  ) %>%
  arrange(
    date
  )

##############################
#### FINAL OUTPUT DATASET ####
##############################

df_som <- reduce(
  .x = list(
    df_conflict_country,
    df_displacement,
    df_ipc_som,
    df_prices_final
  ),
  .f = full_join,
  by = "date"
)

write_csv(
  df_som,
  file.path(
    data_dir,
    "somalia_all_data.csv"
  )
)

write_csv(
  df_ipc_complete,
  file.path(
    data_dir,
    "ipc_complete_data.csv"
  )
)

