library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(gggibbous)

astral <- read_csv("./Data/astral_data.csv")
today <- as_date(Sys.time(), tz = "America/Los_Angeles")

# Get today's events
events_today <- astral |>
  filter(full_date == today)

# Get tomorrow's illumination to determine waxing vs waning
tomorrow <- today + 1
events_tomorrow <- astral |>
  filter(full_date == tomorrow)

# Determine if waxing (growing) or waning (shrinking)
is_waxing <- events_tomorrow$illumination[1] > events_today$illumination[1]

# Create moon phase image
moon_plot <- ggplot() +
  gggibbous::geom_moon(
    aes(x = 0, y = 0, ratio = 1, fill = "gray40"),
    size = 21.5,
    stroke = 0.5
  ) +
  gggibbous::geom_moon(
    aes(
      x = 0,
      y = 0,
      ratio = events_today$illumination[1],
      fill = "#FFFAF0",
      # color = "gray40",
      right = !is_waxing
    ),
    size = 20,
    color = "#FFFAF0"
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
