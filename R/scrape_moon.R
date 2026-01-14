library(lubridate)
library(rvest)
library(dplyr)
library(tidyr)

# Data-----

# Function to scrape moon data for a given year
get_moon_data <- function(year, timezone = 8, midnight_offset = 3) {
  # set url with year and timezone
  url <- paste0(
    "https://aa.usno.navy.mil/calculated/moon/fraction?year=",
    year,
    "&task=00&tz=",
    (timezone - midnight_offset),
    ".00&tz_sign=-1&tz_label=false&submit=Get+Data"
  )

  # Try direct scraping
  page <- read_html(url)

  # Extract table rows manually
  rows <- page %>%
    html_elements("table tr")

  # Extract the data from each row
  moon_list <- rows %>%
    lapply(function(row) {
      row %>%
        html_elements("td") %>%
        html_text()
    })

  # Remove empty rows and convert to dataframe
  moon_list <- moon_list[sapply(moon_list, length) > 0]

  moon_data <- do.call(rbind, moon_list) %>%
    as.data.frame()

  # Remove first row
  moon_data <- moon_data[-1, ]

  # Use first row as column names
  colnames(moon_data) <- moon_data[1, ]

  # Remove that first row (which is now the header)
  moon_data <- moon_data[-1, ]

  moon_clean <- moon_data |>
    janitor::clean_names() |>
    pivot_longer(
      cols = jan:dec,
      names_to = "month",
      values_to = "illumination"
    ) |>
    filter(illumination != "--") |>
    mutate(date = lubridate::as_date(paste0(year, "-", month, "-", day))) |>
    mutate(minus_date = date - 1) |>
    select(full_date = minus_date, illumination) |>
    arrange(full_date) |>
    mutate(illumination = as.numeric(illumination))

  return(moon_clean)
}
