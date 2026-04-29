
library(purrr)
library(tidycensus)
library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(readxl)
library(janitor)

census_api_key("3256bc5e0343cacd70f9f681995f33050910c491", install = TRUE, overwrite = TRUE)
readRenviron('~/.Renviron')

#Variables
years <- c(2005:2019, 2021:2023)
years <- as.integer(years)

my_vars =  c(
  median_rent = "B25064_001",
  median_owner_cost = 'B25088_002',
  total_units = 'B25003_001',
  owner_units = "B25003_002",
  renter_units = "B25003_003",
  total_pop = "B01003_001"
)

acs_data <- purrr::map_dfr(years, function(year) {
  get_acs(
    geography = "us",
    variables =  c(
      median_rent = "B25064_001",
      median_owner_cost = 'B25088_002',
      total_units = 'B25003_001',
      owner_units = "B25003_002",
      renter_units = "B25003_003",
      total_pop = "B01003_001"
    ),
    year = year,
    survey = "acs1"
    ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(
      year = year,
      region_type = "National",
      homeownership_rate = owner_units/total_units,
      renter_share = renter_units/total_units
    )
})

city_data <- purrr::map_dfr(years, function(yr) {
  get_acs(
    geography = "place",
    variables =  c(
      median_rent = "B25064_001",
      median_owner_cost = 'B25088_002',
      total_units = 'B25003_001',
      owner_units = "B25003_002",
      renter_units = "B25003_003",
      total_pop = "B01003_001"
    ),
    year = yr,
    survey = "acs1"
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(
      year = yr,
      region_type = "City",
      homeownership_rate = owner_units/total_units,
      renter_share = renter_units/total_units
    )
})

city_data <- city_data %>%
  mutate(NAME = str_replace(NAME, regex("(?i) city(?=, )"), ''))

city_pop_2023 <- city_data %>%
  filter(year == 2023) %>%
  arrange(desc(total_pop))

top50_cities <- city_pop_2023 %>%
  slice(1:50) %>%
  pull(GEOID)

top50_city_names <- city_pop_2023 %>%
  slice(1:50) %>%
  mutate(NAME = str_replace(NAME, regex("(?i) city(?=, )"), "")) %>%
  pull(NAME)

top50_city_names <- unname(top50_city_names)

combined_data <- bind_rows(acs_data, city_data)

combined_data <- combined_data %>%
  mutate(NAME = str_replace(NAME, regex("(?i) city(?=, )"), ""), NAME)

cpi_df <- read_excel("../Connor/r-cpi-u-rs-allitems.xlsx")

cpi_df <- cpi_df %>%
  row_to_names(row_number = 5)

inflation_df <- cpi_df %>%
  filter(YEAR %in% c(2005:2019,2021:2023)) %>%
  rename(year = YEAR) %>%
  mutate(year = as.numeric(year),
         avg = as.numeric(AVG))
base_cpi <- inflation_df$avg[inflation_df$year == 2023]

acs_inflation <- combined_data %>%
  left_join(inflation_df, by = 'year') %>%
  mutate(
    inflation_factor = base_cpi/avg,
    rent_real = median_rent * inflation_factor,
    owner_cost_real = median_owner_cost * inflation_factor
  )

write_rds(acs_inflation, "../Connor/final_data.rds")
saveRDS(list(
  data = acs_inflation,
  top50_cities = top50_cities,
  top50_names = top50_city_names
), file = "../Connor/final_data.rds")






















