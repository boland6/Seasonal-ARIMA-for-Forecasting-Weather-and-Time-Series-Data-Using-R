setwd("~/Documents/Boston College/Class Material/Spring 2023 Semester/BZAN6612 - Forecasting for Business Analytics/Project")

# Load necessary libraries
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("zoo")
library(dplyr)
library(tidyr)
library(readr)
library(zoo)

# Read the CSV file and replace empty values with NA
boston_weather_data <- read_csv("boston_weather_data.csv", na = "")

# Define a custom function to fill NA values with the average of the preceding and succeeding non-empty values
fill_na_with_avg <- function(column) {
  filled_column <- na.approx(column, na.rm = FALSE)
  return(filled_column)
}

# Apply the custom function to all columns except 'time'
boston_weather_data_filled <- boston_weather_data %>%
  mutate(across(-time, fill_na_with_avg))

# Save the filled dataset to a new CSV file
write_csv(boston_weather_data_filled, "boston_weather_data_filled.csv")


