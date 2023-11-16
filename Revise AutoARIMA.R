# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)
library(tseries)
library(forecast)
library(ggplot2)

# Set the working directory
setwd("~/Documents/Boston College/Class Material/Spring 2023 Semester/BZAN6612 - Forecasting for Business Analytics/Project")

# Read the filled CSV file
boston_weather_data_filled <- read_csv("boston_weather_data_filled.csv")

# Process the data: extract date and aggregate data by month
boston_weather_data_monthly <- boston_weather_data_filled %>%
  mutate(date = floor_date(time, unit = "month")) %>%
  group_by(date) %>%
  summarize(tavg = mean(tavg, na.rm = TRUE), prcp = mean(prcp, na.rm = TRUE))

# Create time series objects
tavg_ts <- ts(boston_weather_data_monthly$tavg, frequency = 12)
prcp_ts <- ts(boston_weather_data_monthly$prcp, frequency = 12)

# Find the best SARIMA model using auto.arima
tavg_sarima <- auto.arima(tavg_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
prcp_sarima <- auto.arima(prcp_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)

# Print the summary of the fitted SARIMA models
summary(tavg_sarima)
summary(prcp_sarima)

# Fit the SARIMA model using the best parameters found by auto.arima
tavg_sarima_fit <- forecast::Arima(tavg_ts, model = tavg_sarima)
prcp_sarima_fit <- forecast::Arima(prcp_ts, model = prcp_sarima)

# Forecast the future values using the fitted SARIMA models
tavg_forecast <- forecast(tavg_sarima_fit, h = 12)
prcp_forecast <- forecast(prcp_sarima_fit, h = 12)

# Plot the temperature forecast and save it to the working directory
tavg_forecast_plot <- plot(tavg_forecast, main = "Temperature Forecast (tavg)", ylab = "Temperature (°F)", xlab = "Time", ylim = range(tavg_ts, tavg_forecast$mean), col = "blue", fcol = "lightblue")
lines(tavg_ts, col = "red")
dev.copy(png, "tavg_forecast_plot.png")
dev.off()

# Plot the precipitation forecast and save it to the working directory
prcp_forecast_plot <- plot(prcp_forecast, main = "Precipitation Forecast (prcp)", ylab = "Precipitation (inches)", xlab = "Time", ylim = range(prcp_ts, prcp_forecast$mean), col = "blue", fcol = "lightblue")
lines(prcp_ts, col = "red")
dev.copy(png, "prcp_forecast_plot.png")
dev.off()

# Calculate the Root Mean Squared Error (RMSE) for the fitted models
tavg_rmse <- sqrt(mean(tavg_sarima_fit$residuals^2))
prcp_rmse <- sqrt(mean(prcp_sarima_fit$residuals^2))

# Output the RMSE
cat("Temperature RMSE:", tavg_rmse, "\n")
cat("Precipitation RMSE:", prcp_rmse, "\n")

# Plot the Error vs. Predicted values for temperature and save it to the working directory
tavg_error_plot <- ggplot(data.frame(Predicted = tavg_sarima_fit$fitted, Error = tavg_sarima_fit$residuals), aes(x = Predicted, y = Error)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Error vs. Predicted Temperature (tavg)") +
  xlab("Predicted Temperature") +
  ylab("Error") +
  theme_bw()
print(tavg_error_plot)
ggsave("tavg_error_plot.png", tavg_error_plot)

# Plot the Error vs. Predicted values for precipitation and save it to the working directory
prcp_error_plot <- ggplot(data.frame(Predicted = prcp_sarima_fit$fitted, Error = prcp_sarima_fit$residuals), aes(x = Predicted, y = Error)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Error vs. Predicted Precipitation (prcp)") +
  xlab("Predicted Precipitation") +
  ylab("Error") +
  theme_bw()
print(prcp_error_plot)
ggsave("prcp_error_plot.png", prcp_error_plot)


