library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(polite)
library(rvest)
library(lubridate)

#Data-----

url <- paste0("https://www.timeanddate.com/astronomy/sights-to-see.html")

url_bow <- polite::bow(url)
html_raw <- polite::scrape(url_bow)

events_raw <- html_raw |> 
  html_elements(".post-row") |> 
  html_text2()

date <- html_raw |> 
  html_elements(".text-color-link--active") |> 
  html_text2() |> 
  as_tibble() |> 
  separate_rows(value, sep = ":") |> 
  mutate(event_length = str_length(value)) |> 
  filter(event_length > 0) |> 
  select(date = value)

title <- html_raw |> 
  html_elements(".post-row h3 a") |> 
  html_text2() |> 
  as_tibble() |> 
  rename(title = value)

text <- html_raw |> 
  html_elements(".post-row p:nth-child(3)") |> 
  html_text2() |> 
  as_tibble() |> 
  rename(text = value)

astral_data <- bind_cols(date, title, text) |> 
  # separate_wider_delim(date,
  #                      delim = " ",
  #                      names = c("month", "date1"),
  #                      too_few = "align_start",
  #                      too_many = "merge",
  #                      cols_remove = FALSE) |>
  separate_wider_delim(date, 
                       delim = "/", 
                       names = c("date1", "date2"), 
                       too_few = "align_start", 
                       cols_remove = FALSE) |> 
  mutate(full_date = lubridate::as_date(paste0("2025-", date1))) |> 
  select(full_date, date, title, text)

readr::write_csv(astral_data, paste0(year, "_astral_data.csv"))
  