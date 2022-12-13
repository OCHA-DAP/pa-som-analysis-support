library(acled.api)
library(idmc)
library(rhdx)
library(tidyverse)
library(rvest)
library(tabulizer)
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
  end.date = "2022-11-30"
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
    country == "Somalia"
  ) %>%
  group_by(
    displacement_type
  ) %>%
  complete(
    date = seq(min(.data$date), max(.data$date), by = "day"),
    fill = list(
      displacement_daily = 0
    )
  ) %>%
  mutate(
    displacement_type = tolower(displacement_type)
  ) %>%
  pivot_wider(
    id_cols = date,
    values_from = displacement_daily,
    names_from = displacement_type,
    names_prefix = "displacement_"
  ) %>%
  mutate(
    across(
      .cols = starts_with("displacement"),
      .fns = replace_na,
      replace = 0
    ),
    displacement_total = displacement_conflict + displacement_disaster + displacement_other
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

# somalia data repair

df <- df %>%
  dplyr::slice(
    -c(1:5)
  ) %>%
  dplyr::mutate(
    country_group = cumsum(is.na(dplyr::lag(country)) | is.na(country))
  ) %>%
  dplyr::group_by(
    country_group
  ) %>%
  dplyr::mutate(
    mutate_group_temp_ = any(is.na(country)) | !all(is.na(area)) | n() == 1,
    area = dplyr::case_when(
      mutate_group_temp_ ~ area,
      dplyr::row_number() == 1 ~ NA_character_,
      TRUE ~ country
    ),
    country = dplyr::case_when(
      mutate_group_temp_ ~ country,
      dplyr::row_number() > 1 ~ stringr::str_extract(country[1], ".*(?=:)"),
      TRUE ~ country
    )
  ) %>%
  dplyr::ungroup()

# filter and wrangle the dataset

# pivot so that each row is a phase/area and columns are variables for that
# specific analysis period (e.g. current or projection)
df_piv <- df %>%
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
    "phase" := "population_analysed_area_phase",
    "analysis_period" := "population_analysed_analysis_period"
  ) %>%
  readr::type_convert() %>%
  dplyr::mutate(
    analysis_period_start = lubridate::floor_date(
      lubridate::dmy(
        x = paste(
          "15",
          stringr::str_extract(
            .data[["analysis_period"]],
            "(.*) - "
          )
        )
      ),
      "month"
    ),
    analysis_period_end = lubridate::ceiling_date(
      lubridate::dmy(
        x = paste(
          "15",
          stringr::str_extract(
            .data[["analysis_period"]],
            " - (.*)$"
          )
        )
      ),
      "month"
    ) - lubridate::days(1),
    .after = "analysis_period"
  )

# Add in manual data for the latest IPC release since has not been updated

ipc_pdf <- extract_tables(
  "https://fsnau.org/downloads/Multi-Partner-Technical-Release-on-Updated-IPC-Analysis-for-Somalia-fo-October-2022-to-June-2023-Final-(English)-13-Dec-2022.pdf",
  pages = 4
)

# country level totals
som_lvls <- str_split(ipc_pdf[[1]][23,3], pattern = "  ", simplify = TRUE) %>%
  parse_number()

# country population
som_pop <- ipc_pdf[[1]][23,2] %>%
  parse_number()

som_type <- c(
  rep(c("current", "first_projection", "second_projection"), each = 3)
)

som_period <- c(
  rep(c("Oct 2022 - Dec 2022", "Jan 2023 - Mar 2023", "Apr 2023 - Jun 2023"), each = 3)
)

som_period_start <- c(
  rep(c(as.Date("2022-10-01"), as.Date("2023-01-01"), as.Date("2023-04-01")), each = 3)
)

som_period_end <- c(
  rep(c(as.Date("2022-12-31"), as.Date("2023-03-31"), as.Date("2023-06-30")), each = 3)
)

som_phase <- c(
  rep(c("phase_3", "phase_4", "phase_5"), by = 3)
)

df_ipc_new <- data.frame(
  country = "Somalia",
  level_1_name = NA_character_,
  area = NA_character_,
  area_id = NA_integer_,
  analysis_name = "Acute Food Insecurity December 2022",
  date_of_analysis = "Dec 2022",
  analysis_type = som_type,
  analysis_period = som_period,
  analysis_period_start = som_period_start,
  analysis_period_end = som_period_end,
  population = som_pop, # from PDF
  phase = som_phase,
  num = som_lvls
) %>%
  pivot_wider(
    names_from = phase,
    names_glue = "{phase}_{.value}",
    values_from = num
  ) %>%
  mutate(
    phase_1_num = 0,
    phase_2_num = 0,
    `phase_p3+_num` = phase_3_num + phase_4_num + phase_5_num
  )

df_clean <- df_clean %>%
  bind_rows(
    df_ipc_new
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
#### FSNAU DASHBOARD DATA ####
##############################

# reading price of water data
urls <- paste0(
  "https://dashboard.fsnau.org/climate/price-of-water/28-",
  paste(
    rep(c("June", "December"), 12),
    rep(2011:2022, each = 2),
    sep = "-"
  )
)

df_water_price <- map(
  .x = urls,
  .f = ~ read_html(.x) %>%
    html_elements("table") %>%
    html_table() %>%
    pluck(1) %>%
    select(
      - `#`
    )
) %>%
  reduce(
    left_join,
    by = c("Region", "District")
  ) %>%
  pivot_longer(
    cols = - c(Region, District),
    names_to = "month",
    values_to = "water_price"
  ) %>%
  mutate(
    month = as.Date(paste0(month, "-01"), format = "%b-%Y-%d")
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
  df_clean,
  file.path(
    data_dir,
    "ipc_complete_data.csv"
  )
)

write_csv(
  df_ipc_complete,
  file.path(
    data_dir,
    "ipc_complete_daily_data.csv"
  )
)

write_csv(
  df_water_price,
  file.path(
    data_dir,
    "som_water_prices.csv"
  )
)

write_csv(
  df_conflict_adm2,
  file.path(
    data_dir,
    "som_conflict_adm2.csv"
  )
)
