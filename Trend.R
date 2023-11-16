setwd("~/Documents/Boston College/Class Material/Spring 2023 Semester/BZAN6612 - Forecasting for Business Analytics/Project")


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Read the filled CSV file
boston_weather_data_filled <- read_csv("boston_weather_data_filled.csv")

# Filter the data to include only years between 2014 and 2022
boston_weather_data_filtered <- boston_weather_data_filled %>%
  mutate(year = year(time)) %>%
  filter(year >= 2014 & year <= 2022)

# Process the data: calculate annual average temperature
boston_weather_data_annual <- boston_weather_data_filtered %>%
  group_by(year) %>%
  summarize(annual_tavg = mean(tavg, na.rm = TRUE))

# Calculate the interquartile range (IQR) and define lower and upper bounds for outliers
iqr <- IQR(boston_weather_data_annual$annual_tavg)
lower_bound <- quantile(boston_weather_data_annual$annual_tavg, 0.25) - 1.5 * iqr
upper_bound <- quantile(boston_weather_data_annual$annual_tavg, 0.75) + 1.5 * iqr

# Remove outliers
boston_weather_data_annual_no_outliers <- boston_weather_data_annual %>%
  filter(annual_tavg > lower_bound & annual_tavg < upper_bound)

# Calculate the moving average with a window of 3 years
boston_weather_data_annual_no_outliers <- boston_weather_data_annual_no_outliers %>%
  mutate(moving_avg = zoo::rollapply(annual_tavg, width = 3, FUN = mean, align = "right", fill = NA))

# Create the line plot for annual average temperature and moving average
annual_trend_plot <- ggplot() +
  geom_line(data = boston_weather_data_annual_no_outliers, aes(x = year, y = annual_tavg, group = 1), color = "blue") +
  geom_line(data = boston_weather_data_annual_no_outliers, aes(x = year, y = moving_avg, group = 1), color = "red", linetype = "dashed") +
  labs(x = "Year", y = "Temperature (tavg)", title = "Annual Average Temperature and Moving Average (3 Years) - No Outliers") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("blue", "red"), labels = c("Annual Average Temperature", "3-Year Moving Average"), name = "Legend")

# Display the plot
print(annual_trend_plot)

# Save the plot to a file
ggsave("annual_trend_plot_no_outliers.png", plot = annual_trend_plot, width = 10, height = 6)
