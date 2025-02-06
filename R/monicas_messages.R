library(dplyr)
library(readr)
library(lubridate)

# Data-----

year <- 2025
astral <- read_csv("./Data/monicas_astral_data.csv")
current_time_ca <- with_tz(Sys.time(), tzone = "America/Los_Angeles")

today <- lubridate::as_date(Sys.time(), tz = "America/Los_Angeles")
events_today <- astral |> 
  filter(full_date == today)

next_meteor <- astral |> 
  filter(full_date > today, type == "meteor") |> 
  head(n = 1)


# Message-----
em_date <- paste0("<b>", format(events_today$full_date[1], "%b %d, %Y"), "</b>")
em_subject <- paste0("Astral Report: ", format(events_today$full_date[1], "%b %d, %Y"))
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

