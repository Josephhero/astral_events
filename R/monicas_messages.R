library(dplyr)
library(lubridate)
library(readr)
library(glue)
library(ggplot2)
library(gggibbous)
library(base64enc)

year <- year(now(tzone = "America/Los_Angeles"))
astral <- read_csv("./Data/monicas_astral_data.csv")
today <- as_date(Sys.time(), tz = "America/Los_Angeles")

# Get today's events
events_today <- astral |>
  filter(full_date == today)

# Get next events of interest
next_sol <- astral |>
  filter(full_date > today, type %in% c("solstice", "equinox")) |>
  slice(1)

next_meteor <- astral |>
  filter(full_date > today, type == "meteor") |>
  slice(1)

next_eclipse <- astral |>
  filter(full_date > today, type == "eclipse") |>
  slice(1)

# Get tomorrow's illumination to determine waxing vs waning
tomorrow <- today + 1
events_tomorrow <- astral |>
  filter(full_date == tomorrow)

# Determine if waxing (growing) or waning (shrinking)
is_waxing <- events_tomorrow$illumination[1] > events_today$illumination[1]

# Create moon phase image
moon_plot <- ggplot() +
  gggibbous::geom_moon(
    aes(x = 0, y = 0, ratio = 1, fill = "gray20"),
    size = 22
  ) +
  gggibbous::geom_moon(
    aes(
      x = 0,
      y = 0,
      ratio = events_today$illumination[1],
      fill = "#FFFAF0",
      right = !is_waxing
    ),
    size = 20
  ) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Save the plot
ggsave(
  "Images/moon_phase.png",
  moon_plot,
  width = 1,
  height = 1,
  units = "in",
  dpi = 150,
  bg = "transparent"
)

# Convert to base64
moon_base64 <- base64encode("Images/moon_phase.png")

# Helper function to format date display
format_date <- function(date, days = NA) {
  if (is.na(date)) {
    return("None listed")
  }
  if (is.na(days)) {
    format(date, "%b %d, %Y")
  } else {
    paste0(days, ", ", year(date))
  }
}

# Build current events section
current_events <- events_today |>
  filter(!is.na(title)) |>
  mutate(event_text = glue("<b>{title}</b><br>{text}")) |>
  pull(event_text) |>
  paste(collapse = "<br><br>")

if (current_events == "") {
  current_events <- "No Current Events"
}

# Build email subject
em_subject <- glue(
  "Astral Report: {format(events_today$full_date[1], '%b %d, %Y')}"
)

# Build email content
message <- glue(
  "<b>{format(events_today$full_date[1], '%b %d, %Y')}</b><br><br>",
  "<b>Lunar Illumination:</b> {events_today$illumination[1] * 100}%  ",
  "<img src='data:image/png;base64,{moon_base64}' alt='Moon' style='width:30px;height:30px;vertical-align:middle;'>",
  "<br>",
  "<b>Age of Moon:</b> {events_today$moon_age[1]} Days<br>",
  "<b>New Moon:</b> {events_today$days_until_new_moon[1]} Days<br><br>",
  "{current_events}<br><br>",
  "<b>Next {ifelse(is.na(next_sol$type[1]), 'Solstice/Equinox', next_sol$type[1])}:</b> {format_date(next_sol$full_date[1], next_sol$days[1])}<br>",
  "<b>Next Eclipse:</b> {format_date(next_eclipse$full_date[1], next_eclipse$days[1])}<br>",
  "<b>Next Meteor Shower:</b> {format_date(next_meteor$full_date[1], next_meteor$days[1])}<br>"
)

# Write outputs for GitHub Actions
cat(em_subject, file = "email_subject.txt", sep = "")
cat(message, file = "email_body.txt", sep = "")
