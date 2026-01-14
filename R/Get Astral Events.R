library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(polite)
library(rvest)
library(lubridate)

# Data-----

source("R/scrape_moon_data.R")
source("R/scrape_astral_events.R")

# Get current year
year <- year(now(tzone = "America/Los_Angeles"))

astral <- scrape_astral_events()

# Get data for previous year, current year, and next year
moon <- bind_rows(
  get_moon_data(year - 1),
  get_moon_data(year),
  get_moon_data(year + 1)
) |>
  arrange(full_date) |>
  mutate(moon_group = cumsum(illumination == 0.00) + 1) |>
  mutate(moon_age = row_number(), .by = moon_group) |>
  mutate(days_until_new_moon = (max(moon_age) - moon_age) + 1, .by = moon_group)

all_data <- full_join(moon, astral, by = "full_date")

readr::write_csv(all_data, paste0("Data/monicas_astral_data.csv"))
