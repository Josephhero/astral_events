library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(polite)
library(rvest)
library(lubridate)

#Data----- 

year = 2025
url <- paste0("https://aa.usno.navy.mil/calculated/moon/fraction?year=", 
              year, 
              "&task=00&tz=8.00&tz_sign=-1&tz_label=false&submit=Get+Data")

#"https://aa.usno.navy.mil/calculated/moon/fraction?year=2025&task=00&tz=8.00&tz_sign=-1&tz_label=false&submit=Get+Data"

# Just copy/paste the data into a csv. Keep it easy. 
# Delete the "--" for non-days

moon_data <- read_csv("moon_data.csv") |> 
  janitor::clean_names() |> 
  pivot_longer(cols = jan:dec) |> 
  rename(month = name, illumination = value) |> 
  filter(!is.na(illumination)) |> 
  mutate(full_date = lubridate::as_date(paste0("2024-", month, "-", day))) |> 
  select(full_date, illumination)  |> 
  mutate(moon_group = cumsum(illumination == 0) + 1) |> 
  mutate(moon_age = row_number(), .by = moon_group) |> 
  mutate(days_until_new_moon = (max(moon_age) - moon_age) + 1, .by = moon_group)
  

write_csv(moon_data, paste0(year, "_moon_data.csv"))

  




