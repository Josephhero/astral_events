library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(polite)
library(rvest)
library(lubridate)

#Data-----

# year <- year(now(tzone = "America/Los_Angeles"))
# url <- paste0("https://www.timeanddate.com/astronomy/sights-to-see.html")

scrape_astral_events <- function(year = NULL) {
  # Use current year if not specified
  if (is.null(year)) {
    year <- year(now(tzone = "America/Los_Angeles"))
  }

  # Scrape the webpage
  url <- "https://www.timeanddate.com/astronomy/sights-to-see.html"
  url_bow <- polite::bow(url)
  html_raw <- polite::scrape(url_bow)

  # Extract dates
  date <- html_raw |>
    html_elements(".text-color-link--active") |>
    html_text2() |>
    as_tibble() |>
    separate_rows(value, sep = ":") |>
    mutate(event_length = str_length(value)) |>
    filter(event_length > 0) |>
    select(date = value)

  # Extract titles
  title <- html_raw |>
    html_elements(".post-row h3 a") |>
    html_text2() |>
    as_tibble() |>
    rename(title = value)

  # Extract text descriptions
  text <- html_raw |>
    html_elements(".post-row p:nth-child(3)") |>
    html_text2() |>
    as_tibble() |>
    rename(text = value)

  # Combine and clean data
  astral_data <- bind_cols(date, title, text) |>
    separate_wider_delim(
      date,
      delim = "/",
      names = c("date1", "date2"),
      too_few = "align_start",
      cols_remove = FALSE
    ) |>
    # Extract month to detect rollover
    mutate(month = month(lubridate::as_date(paste0(year, "-", date1)))) |>
    # Detect year rollover: if current month < previous month, we've rolled to next year
    mutate(year_offset = cumsum(coalesce(month < lag(month), FALSE))) |>
    mutate(
      full_date = lubridate::as_date(paste0(year + year_offset, "-", date1))
    ) |>
    # Fix encoding issues
    mutate(across(
      c(title, text),
      ~ str_replace_all(
        .,
        c(
          "‚Äî" = "—", # em dash
          "‚Äô" = "'", # apostrophe/single quote
          "‚Äú" = "\"", # left double quote
          "‚Äù" = "\"", # right double quote
          "‚Ä?" = "–", # en dash
          "√¢‚Ç¨" = "…" # ellipsis
        )
      )
    )) |>
    # Categorize event types
    mutate(
      type = case_when(
        str_detect(text, regex("eclipse", ignore_case = TRUE)) ~ "eclipse",
        str_detect(text, regex("equinox", ignore_case = TRUE)) ~ "equinox",
        str_detect(text, regex("solstice", ignore_case = TRUE)) ~ "solstice",
        str_detect(text, regex("meteor", ignore_case = TRUE)) ~ "meteor",
        .default = "other"
      )
    ) |>
    select(full_date, days = date, type, title, text)

  return(astral_data)
}
  
