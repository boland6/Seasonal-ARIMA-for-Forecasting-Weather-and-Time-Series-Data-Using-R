setwd("~/Documents/Boston College/Class Material/Spring 2023 Semester/BZAN6612 - Forecasting for Business Analytics/Project")

# Load necessary libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("readr")
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Read the filled CSV file
boston_weather_data_filled <- read_csv("boston_weather_data_filled.csv")

# Process the data: extract year and month from the 'time' column
boston_weather_data_monthly <- boston_weather_data_filled %>%
  mutate(year = year(time), month = month(time)) %>%
  group_by(year, month) %>%
  summarize(tavg = mean(tavg, na.rm = TRUE), prcp = mean(prcp, na.rm = TRUE))

# Create the line plot for temperature
temperature_plot <- ggplot(boston_weather_data_monthly, aes(x = month, y = tavg, group = year, color = as.factor(year))) +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Temperature (tavg)", title = "Monthly Temperature Throughout the Years", color = "Year") +
  theme_bw()

# Save the temperature plot to a file
ggsave("monthly_temperature_plot.png", width = 10, height = 6)

# Create the line plot for precipitation
precipitation_plot <- ggplot(boston_weather_data_monthly, aes(x = month, y = prcp, group = year, color = as.factor(year))) +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Precipitation (prcp)", title = "Monthly Precipitation Throughout the Years", color = "Year") +
  theme_bw()

# Save the precipitation plot to a file
ggsave("monthly_precipitation_plot.png", width = 10, height = 6)
