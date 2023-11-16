setwd("~/Documents/Boston College/Class Material/Spring 2023 Semester/BZAN6612 - Forecasting for Business Analytics/Project")

# Load required libraries
install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

# Read the dataset
weather_data <- read.csv("boston_weather_data.csv")

# Convert the time column to a proper date format
#weather_data$Time <- as.Date(weather_data$Time, format="%Y-%m-%d")

# Set the date column as the index
rownames(weather_data) <- weather_data$Time
weather_data <- weather_data[,-1]

# Remove NAs from the dataset
weather_data <- na.omit(weather_data)

# Create a time series object for the average temperature
tavg_ts <- ts(weather_data$tavg, frequency = 365)

# Check stationarity using the Augmented Dickey-Fuller Test
adf_test <- adf.test(tavg_ts, alternative = "stationary")

# If the p-value is greater than 0.05, apply differencing to make the series stationary
if (adf_test$p.value > 0.05) {
  tavg_ts_diff <- diff(tavg_ts)
} else {
  tavg_ts_diff <- tavg_ts
}

# Plot ACF and PACF for the stationary series to find optimal p and q values
acf(tavg_ts_diff, main="ACF of Stationary Average Temperature")
pacf(tavg_ts_diff, main="PACF of Stationary Average Temperature")

#######################################################################################
# Fit the ARIMA model using the selected p, d, and q values
# Replace p_value and q_value with the values you determine from the ACF and PACF plots
#p_value <- 1
#d_value <- 1
#q_value <- 1
#arima_model <- auto.arima(tavg_ts, order=c(p_value, d_value, q_value), seasonal=FALSE)
#######################################################################################
##############################Commented out because auto,arima automatically determines p,d,q, based on
##############################available data###########################################


# Fit the ARIMA model using the auto.arima function
arima_model <- auto.arima(tavg_ts, seasonal=FALSE)

# Generate a forecast
forecast_days <- 30
tavg_forecast <- forecast(arima_model, h=forecast_days)

# Plot the forecast
plot(tavg_forecast, main="30-Day Average Temperature Forecast")
