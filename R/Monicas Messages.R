library(dplyr)
library(readr)
library(gmailr)
library(lubridate)

# Data-----

year <- 2025
astral <- read_csv(paste0(year, "_astral_data.csv")) |> 
  mutate(full_date = lubridate::mdy(full_date))

moon <- read_csv(paste0(year, "_moon_data.csv")) |> 
  mutate(full_date = lubridate::mdy(full_date))

moon_astral <- left_join(moon, astral, by = "full_date") |> 
  mutate(season = if_else(type %in% c("equinox", "solstice"), 1, 0, missing = 0)) |> 
  mutate(season_group = cumsum(season == 1) + 1) |> 
  mutate(meteor = if_else(type == "meteor", 1, 0, missing = 0)) |> 
  mutate(meteor_group = cumsum(meteor == 1) + 1) |> 
  mutate(season_days = row_number(), .by = season_group) |> 
  mutate(meteor_days = row_number(), .by = meteor_group) |> 
  mutate(days_until_season = if_else(
    type %in% c("equinox", "solstice"), 
    0, 
    (max(season_days) - season_days) + 1), .by = season_group) |> 
  mutate(days_until_meteor = case_when(
    type == "meteor" ~ 0, 
    TRUE ~ (max(meteor_days) - meteor_days) + 1), 
    .by = meteor_group) |> 
  mutate(season_type = case_when(
    season_group %in% c(1, 3) ~ "Equinox", 
    season_group %in% c(2, 4) ~ "Solstice", 
  ))
  

events_today <- moon_astral |> 
  filter(full_date == Sys.Date())

next_meteor <- moon_astral |> 
  filter(full_date > Sys.Date(), type == "meteor") |> 
  head(n = 1)
  

# Message-----
em_date <- paste0("<b>", format(events_today$full_date[1], "%b %d, %Y"), "</b>")
em_illum <- paste0("<b>Lunar Illumination:</b> ", events_today$illumination[1] * 100, "%")
em_age <- paste0("<b>Age of Moon:</b> ", events_today$moon_age[1], " Days")
em_new <- paste0("<b>New Moon:</b> ", events_today$days_until_new_moon[1], " Days")
em_title <- paste0(events_today$title[1])
em_text <- paste0(events_today$text[1])
em_title2 <- paste0(events_today$title[2])
em_text2 <- paste0(events_today$text[2])
em_title3 <- paste0(events_today$title[3])
em_text3 <- paste0(events_today$text[3])
em_sols <- paste0("<b>Next ", events_today$season_type, ":</b> ", events_today$days_until_season[1], " Days")
em_meteor <- paste0("<b>Next Meteor Shower:</b> ", events_today$days_until_season[1], " Days")
em_meteor_shower <- paste0("~", next_meteor$title, " (", next_meteor$days, ")")


if (em_title3 != "NA") {
  em_events <- 
    paste0("<b>", em_title, "</b>", "<br>", em_text, "<br><br>", 
           "<b>", em_title2, "</b>", "<br>", em_text2, "<br><br>", 
           "<b>", em_title3, "</b>", "<br>", em_text3 
    )
} else if (em_title2 != "NA") {
  em_events <- 
    paste0("<b>", em_title, "</b>", "<br>", em_text, "<br><br>", 
           "<b>", em_title2, "</b>", "<br>", em_text2 
    )
} else if (em_title != "NA") {
  em_events <- 
    paste0("<b>", em_title, "</b>", "<br>", em_text 
    )
} else {
  em_events <- "No Current Events"
}
  
message <- 
  paste0(
    em_date, "<br>", 
    em_illum, "<br>", 
    em_age, "<br>", 
    em_new, "<br><br>", 
    em_events, "<br><br>", 
    em_sols, "<br>", 
    em_meteor, "<br>", 
    em_meteor_shower
  )


message




send_text(message)


# Email-----

astral_email <-
  gm_mime() |>
  gm_to("mmrojas1986@gmail.com") |> 
  gm_from("hefnerjoseph87@gmail.com") |>
  gm_subject(paste0("Jan 31 Astral Report")) |>
  gm_html_body(message
  )

d <- gm_create_draft(astral_email)
 

gm_send_message(astral_email)









