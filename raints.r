# Set the working directory to this file's location.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Install and load necessary add-on packages.
if (!require(forecast)) {
  install.packages("forecast")
} else {
  library(forecast)
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
} else {
  library(tidyverse)
}
if (!require(tseries)) {
  install.packages("tseries")
} else {
  library(tseries)
}


# Set the seed for R's random number generator.
set.seed(1)
# Remove scientific notation in plots.
options(scipen = 10)


#~~~~~ Functions ~~~~~#

# Normal quantile-quantile plot of standardised residuals.
normal.qqplot <- function(x) {
  qqnorm(x, main = "", xlab = "", ylab = "")
  qqline(x)
}

# Moving average with equal and symmetric weights.
smooth.sym <- function(my.ts, window.q) {
  window.size <- 2 * window.q + 1
  my.ts.sm <- rep(0, length(my.ts) - window.size)
  for(i in 1:length(my.ts.sm)) {
    my.ts.sm[i] <- mean(my.ts[i:(i + window.size - 1)])
  }
  my.ts.sm
}

# Spencer's 15-point moving average.
smooth.spencer <- function(my.ts) {
  weight <- c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3)/320
  my.ts.sm <- rep(0 ,length(my.ts) - 15)
  for(i in 1:length(my.ts.sm)) {
    my.ts.sm[i] <- sum(my.ts[i:(i + 14)] * weight)
  }
  my.ts.sm
}


#~~~~~ Data ~~~~~#

# Variables specifying start and end month and year.
start.year = 2015; start.month = 1; end.year = 2018; end.month = 12;


# Read in and mutate data for Albany (009500).
rainfall.albany.overview <- read_csv(
  file = "data/raw/IDCJAC0009_009500_1800/IDCJAC0009_009500_1800_Data.csv",
  col_names = TRUE,
  col_types = cols(col_character(), col_number(), col_integer(), col_integer(), col_integer(), col_double(), col_number(), col_character(), .default = col_guess()),
  na = c("", "NA", "NULL"),
  trim_ws = TRUE,
  progress = show_progress(),
  skip_empty_rows = TRUE) %>%
  rename("product_code" = 1, "station_number" = 2, "year" = 3, "month" = 4, "day" = 5, "rainfall" = 6, "period" = 7, "quality" = 8) %>%
  drop_na(rainfall) %>%
  mutate(date  = lubridate::as_date(sprintf("%04d-%01d-%01d", year, month, day))) %>%
  complete(date = seq.Date(lubridate::as_date(sprintf("%04d-%01d-%01d", start.year, start.month, 1)), lubridate::as_date(sprintf("%04d-%01d-%01d", end.year, end.month, lubridate::days_in_month(end.month))), by = "day"), fill = list(rainfall = 0.0)) %>%
  mutate(year  = as.integer(lubridate::year(date)),
         month = as.integer(lubridate::month(date)),
         day   = as.integer(lubridate::day(date)),
         week  = as.integer(lubridate::week(date)),
         wday  = as.integer(lubridate::wday(date, week_start = 1))) %>%
  filter(date >= lubridate::as_date(sprintf("%04d-%01d-%01d", start.year, start.month, 1)) & date <= lubridate::as_date(sprintf("%04d-%01d-%01d", end.year, end.month, lubridate::days_in_month(end.month)))) %>%
  summarise(station_number, date, year, month, day, week, wday, rainfall)

# Read in and mutate data for Cape Naturaliste (009519).
rainfall.capenaturaliste.overview <- read_csv(
  file = "data/raw/IDCJAC0009_009519_1800/IDCJAC0009_009519_1800_Data.csv",
  col_names = TRUE,
  col_types = cols(col_character(), col_number(), col_integer(), col_integer(), col_integer(), col_double(), col_number(), col_character(), .default = col_guess()),
  na = c("", "NA", "NULL"),
  trim_ws = TRUE,
  progress = show_progress(),
  skip_empty_rows = TRUE) %>%
  rename("product_code" = 1, "station_number" = 2, "year" = 3, "month" = 4, "day" = 5, "rainfall" = 6, "period" = 7, "quality" = 8) %>%
  drop_na(rainfall) %>%
  mutate(date  = lubridate::as_date(sprintf("%04d-%01d-%01d", year, month, day))) %>%
  complete(date = seq.Date(lubridate::as_date(sprintf("%04d-%01d-%01d", start.year, start.month, 1)), lubridate::as_date(sprintf("%04d-%01d-%01d", end.year, end.month, lubridate::days_in_month(end.month))), by = "day"), fill = list(rainfall = 0.0)) %>%
  mutate(year  = as.integer(lubridate::year(date)),
         month = as.integer(lubridate::month(date)),
         day   = as.integer(lubridate::day(date)),
         week  = as.integer(lubridate::week(date)),
         wday  = as.integer(lubridate::wday(date, week_start = 1))) %>%
  filter(date >= lubridate::as_date(sprintf("%04d-%01d-%01d", start.year, start.month, 1)) & date <= lubridate::as_date(sprintf("%04d-%01d-%01d", end.year, end.month, lubridate::days_in_month(end.month)))) %>%
  summarise(station_number, date, year, month, day, week, wday, rainfall)


#~~~~~ Subsetting ~~~~~#

# Aggregate data for Albany (009500).
rainfall.albany.weekly <-
  rainfall.albany.overview %>%
  group_by(date = lubridate::as_date(lubridate::floor_date(x = date, unit = "1 week"))) %>%
  summarise(rainfall = sum(rainfall))


# Aggregate data for Cape Naturaliste (009519).
rainfall.capenaturaliste.weekly <-
  rainfall.capenaturaliste.overview %>%
  group_by(date = lubridate::as_date(lubridate::floor_date(x = date, unit = "1 week"))) %>%
  summarise(rainfall = sum(rainfall))


#~~~~~ Summary Statistics ~~~~~#

rainfall.weekly.overview <-
  bind_rows(
    rainfall.albany.weekly %>%
      summarise(station = 009500, rainfall),
    rainfall.capenaturaliste.weekly %>%
      summarise(station = 009519, rainfall)
    )

rainfall.weekly.summary <-
  bind_rows(
    rainfall.albany.weekly %>%
      summarise("Station"   = "Albany",
            "Min."      = min(rainfall),
            "1st Qu."   = quantile(rainfall, .25),
            "Median"    = median(rainfall),
            "Mean"      = mean(rainfall),
            "3rd Qu."   = quantile(rainfall, .75),
            "Max."      = max(rainfall),
            "IQR"       = IQR(rainfall),
            "Std. Dev." = sd(rainfall)),
    rainfall.capenaturaliste.weekly %>%
      summarise("Station"   = "Cape Naturaliste",
            "Min."      = min(rainfall),
            "1st Qu."   = quantile(rainfall, .25),
            "Median"    = median(rainfall),
            "Mean"      = mean(rainfall),
            "3rd Qu."   = quantile(rainfall, .75),
            "Max."      = max(rainfall),
            "IQR"       = IQR(rainfall),
            "Std. Dev." = sd(rainfall))
  ) %>% column_to_rownames(var = "Station")


par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Create box-plots for rainfall by station.
boxplot(rainfall ~ station, data = rainfall.weekly.overview,
        xlab = "", ylab = "",
        ylim = c(0, 90),
        at = c(1,2), col = grey.colors(2, start = 0.5, alpha = 0.5),
        names = c("Albany", "Cape Nat."), horizontal = TRUE)
title(main = "", xlab = "Rainfall (mm)", ylab = "Location", line = 2)
# Create overlaid histograms for rainfall by station.
hist(rainfall.weekly.overview$rainfall[rainfall.weekly.overview$station == 009500],
     breaks = 20,
     main = "", xlab = "", ylab = "",
     xlim = c(0, 90), ylim = c(0, 110),
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
hist(rainfall.weekly.overview$rainfall[rainfall.weekly.overview$station == 009519],
     breaks = 20,
     main = "", xlab = "", ylab = "",
     xlim = c(0, 90), ylim = c(0, 100),
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), add = TRUE)
legend("topright", c("Albany", "Cape Nat."), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Rainfall (mm)", ylab = "Frequency", line = 2)
box()
# Create overlaid density plots for rainfall by station.
plot(density(rainfall.weekly.overview$rainfall[rainfall.weekly.overview$station == 009500]),
     main = "", xlab = "", ylab = "",
     xlim = c(0, 90), ylim = c(0, 0.06))
polygon(density(rainfall.weekly.overview$rainfall[rainfall.weekly.overview$station == 009500]),
        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
lines(density(rainfall.weekly.overview$rainfall[rainfall.weekly.overview$station == 009519]),
      main = "", xlab = "", ylab = "",
      xlim = c(0, 90), ylim = c(0, 0.06))
polygon(density(rainfall.weekly.overview$rainfall[rainfall.weekly.overview$station == 009519]),
        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
legend("topright", c("Albany", "Cape Nat."), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Rainfall (mm)", ylab = "Density", line = 2)


#~~~~~ Time Series Visualisation ~~~~~#

par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mar = c(3,4,3,4))
# Plot time series, acf, pacf for Albany (009500).
plot(rainfall ~ date, rainfall.albany.weekly, type = "l", xlab = "", ylab ="")
title(main = "Weekly Rainfall in Albany (009500)", xlab = "", ylab = "Rainfall (mm)", line = 2)
acf(rainfall.albany.weekly$rainfall, lag.max = nrow(rainfall.albany.weekly), ylab = "", main = "")
title(xlab = "Lag", ylab = "ACF", line = 2)
acf(rainfall.albany.weekly$rainfall, lag.max = nrow(rainfall.albany.weekly), type = "partial", ylab = "", main = "")
title(xlab = "Lag", ylab = "Partial ACF", line = 2)

# Evaluate the smoothed time series for Albany (009500).
rainfall.albany.weekly.sm <- smooth.sym(rainfall.albany.weekly$rainfall, 7)
# Plot smoothed time series, acf, pacf for Albany (009500).
plot(rainfall.albany.weekly.sm, type = "l", xlab = "", ylab ="")
title(main = "Smoothed Weekly Rainfall in Albany (009500)", xlab = "", ylab = "Rainfall (mm)", line = 2)
acf(rainfall.albany.weekly.sm, lag.max = nrow(rainfall.albany.weekly), ylab = "", main = "")
title(xlab = "Lag", ylab = "ACF", line = 2)
acf(rainfall.albany.weekly.sm, lag.max = nrow(rainfall.albany.weekly), type = "partial", ylab = "", main = "")
title(xlab = "Lag", ylab = "Partial ACF", line = 2)

# Evaluate the smoothed time series using Spencer's 15-point moving average for Albany (009500).
rainfall.albany.weekly.spencer <- smooth.spencer(rainfall.albany.weekly$rainfall)
# Plot smoothed time series using Spencer's 15-point moving average, acf, pacf for Albany (009500).
plot(rainfall.albany.weekly.spencer, type = "l", xlab = "", ylab ="")
title(main = "Smoothed (Spencer's 15-point MA) Weekly Rainfall in Albany (009500)", xlab = "", ylab = "Rainfall (mm)", line = 2)
acf(rainfall.albany.weekly.spencer, lag.max = nrow(rainfall.albany.weekly), ylab = "", main = "")
title(xlab = "Lag", ylab = "ACF", line = 2)
acf(rainfall.albany.weekly.spencer, lag.max = nrow(rainfall.albany.weekly), type = "partial", ylab = "", main = "")
title(xlab = "Lag", ylab = "Partial ACF", line = 2)

par(cex.axis = 0.8, cex.lab = 0.7, cex.main = 0.9, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(1,1), mar = c(3,4,3,4))
# Plot decompositions for Albany (009500).
rainfall.albany.weekly.ts <-
  ts(rainfall.albany.weekly$rainfall, frequency = 365.25/7)
rainfall.albany.weekly.de <-
  decompose(rainfall.albany.weekly.ts, type = "additive")
plot(rainfall.albany.weekly.de, mar = c(0,4,0,4))


par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mar = c(3,4,3,4))
# Plot time series, acf, pacf for Cape Naturaliste (009519).
plot(rainfall ~ date, rainfall.capenaturaliste.weekly, type = "l", xlab = "", ylab ="")
title(main = "Weekly Rainfall in Cape Naturaliste (009519)", xlab = "", ylab = "Rainfall (mm)", line = 2)
acf(rainfall.capenaturaliste.weekly$rainfall, lag.max = nrow(rainfall.capenaturaliste.weekly), ylab = "", main = "")
title(xlab = "Lag", ylab = "ACF", line = 2)
acf(rainfall.capenaturaliste.weekly$rainfall, lag.max = nrow(rainfall.capenaturaliste.weekly), type = "partial", ylab = "", main = "")
title(xlab = "Lag", ylab = "Partial ACF", line = 2)

# Evaluate the smoothed time series for Cape Naturaliste (009519).
rainfall.capenaturaliste.weekly.sm <- smooth.sym(rainfall.capenaturaliste.weekly$rainfall, 7)
# Plot smoothed time series, acf, pacf for Cape Naturaliste (009519).
plot(rainfall.capenaturaliste.weekly.sm, type = "l", xlab = "", ylab ="")
title(main = "Smoothed Weekly Rainfall in Cape Naturaliste (009519)", xlab = "", ylab = "Rainfall (mm)", line = 2)
acf(rainfall.capenaturaliste.weekly.sm, lag.max = nrow(rainfall.capenaturaliste.weekly), ylab = "", main = "")
title(xlab = "Lag", ylab = "ACF", line = 2)
acf(rainfall.capenaturaliste.weekly.sm, lag.max = nrow(rainfall.capenaturaliste.weekly), type = "partial", ylab = "", main = "")
title(xlab = "Lag", ylab = "Partial ACF", line = 2)

# Evaluate the smoothed time series using Spencer's 15-point moving average for Cape Naturaliste (009519).
rainfall.capenaturaliste.weekly.spencer <- smooth.spencer(rainfall.capenaturaliste.weekly$rainfall)
# Plot smoothed time series using Spencer's 15-point moving average, acf, pacf for Cape Naturaliste (009519).
plot(rainfall.capenaturaliste.weekly.spencer, type = "l", xlab = "", ylab ="")
title(main = "Smoothed (Spencer's 15-point MA) Weekly Rainfall in Cape Naturaliste (009519)", xlab = "", ylab = "Rainfall (mm)", line = 2)
acf(rainfall.capenaturaliste.weekly.spencer, lag.max = nrow(rainfall.capenaturaliste.weekly), ylab = "", main = "")
title(xlab = "Lag", ylab = "ACF", line = 2)
acf(rainfall.capenaturaliste.weekly.spencer, lag.max = nrow(rainfall.capenaturaliste.weekly), type = "partial", ylab = "", main = "")
title(xlab = "Lag", ylab = "Partial ACF", line = 2)

par(cex.axis = 0.8, cex.lab = 0.7, cex.main = 0.9, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(1,1), mar = c(3,4,3,4))
# Plot decompositions for Cape Naturaliste (009519).
rainfall.capenaturaliste.weekly.ts <-
  ts(rainfall.capenaturaliste.weekly$rainfall, frequency = 365.25/7)
rainfall.capenaturaliste.weekly.de <-
  decompose(rainfall.capenaturaliste.weekly.ts, type = "additive")
plot(rainfall.capenaturaliste.weekly.de, mar = c(0,4,0,4))


#~~~~~ Differenced Time Series ~~~~~#

# Difference the time series at lag 1.
rainfall.albany.weekly.diff <-
  diff(rainfall.albany.weekly$rainfall,
       lag = 1, differences = 1)
rainfall.capenaturaliste.weekly.diff <-
  diff(rainfall.capenaturaliste.weekly$rainfall,
       lag = 1, differences = 1)


#~~~~~ Differenced Time Series Visualisation ~~~~~#

par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mar = c(3,4,3,4))
# Plot differenced time series, acf, pacf for Albany (009500).
plot(rainfall.albany.weekly.diff, type = "l", xlab = "", ylab ="")
title(main = "Weekly Rainfall in Albany (009500) Differenced at Lag 1", xlab = "", ylab = "Rainfall (mm)", line = 2)
acf(rainfall.albany.weekly.diff, lag.max = length(rainfall.albany.weekly.diff), ylab = "", main = "")
title(xlab = "Lag", ylab = "ACF", line = 2)
acf(rainfall.albany.weekly.diff, lag.max = length(rainfall.albany.weekly.diff), type = "partial", ylab = "", main = "")
title(xlab = "Lag", ylab = "Partial ACF", line = 2)

par(cex.axis = 0.8, cex.lab = 0.7, cex.main = 0.9, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(1,1), mar = c(3,4,3,4))
# Plot differenced decompositions for Albany (009500).
rainfall.albany.weekly.diff.ts <-
  ts(rainfall.albany.weekly.diff, frequency = 365.25/7)
rainfall.albany.weekly.diff.de <-
  decompose(rainfall.albany.weekly.diff.ts, type = "additive")
plot(rainfall.albany.weekly.diff.de, mar = c(0,4,0,4))


par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mar = c(3,4,3,4))
# Plot differenced time series, acf, pacf for Cape Naturaliste (009519).
plot(rainfall.capenaturaliste.weekly.diff, type = "l", xlab = "", ylab ="")
title(main = "Weekly Rainfall in Cape Naturaliste (009519) Differenced at Lag 1", xlab = "", ylab = "Rainfall (mm)", line = 2)
acf(rainfall.capenaturaliste.weekly.diff, lag.max = length(rainfall.capenaturaliste.weekly.diff), ylab = "", main = "")
title(xlab = "Lag", ylab = "ACF", line = 2)
acf(rainfall.capenaturaliste.weekly.diff, lag.max = length(rainfall.capenaturaliste.weekly.diff), type = "partial", ylab = "", main = "")
title(xlab = "Lag", ylab = "Partial ACF", line = 2)

par(cex.axis = 0.8, cex.lab = 0.7, cex.main = 0.9, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(1,1), mar = c(3,4,3,4))
# Plot differenced decompositions for Cape Naturaliste (009519).
rainfall.capenaturaliste.weekly.diff.ts <-
  ts(rainfall.capenaturaliste.weekly.diff, frequency = 365.25/7)
rainfall.capenaturaliste.weekly.diff.de <-
  decompose(rainfall.capenaturaliste.weekly.diff.ts, type = "additive")
plot(rainfall.capenaturaliste.weekly.diff.de, mar = c(0,4,0,4))


#~~~~~ Stationarity ~~~~~#

# Perform augmented Dickey-Fuller (ADF) tests to determine stationarity. The null hypothesis is that the time
# series is non-stationary. So, we will be looking for p-values < 0.05 (or other sig. level).
adf.test(rainfall.albany.weekly$rainfall,
         alternative = "stationary")
adf.test(rainfall.capenaturaliste.weekly$rainfall,
         alternative = "stationary")

# Perform augmented Dickey-Fuller (ADF) tests to determine stationarity of the differenced series. The null
# hypothesis is that the time series is non-stationary.
adf.test(rainfall.albany.weekly.diff,
         alternative = "stationary")
adf.test(rainfall.capenaturaliste.weekly.diff,
         alternative = "stationary")


#~~~~~ Model Fitting ~~~~~#

# Attempt to fit a SARIMA model using aggregated data for Albany (009500).
rainfall.albany.weekly.model <-
  auto.arima(ts(rainfall.albany.weekly$rainfall, frequency = 365.25/7),
             d = 1, D = 1,
             max.p = 10, max.q = 10, max.P = 10, max.Q = 10, max.d = 1, max.D = 1,
             start.p = 0, start.q = 0, start.P = 0, start.Q = 0,
             seasonal = TRUE)
rainfall.albany.weekly.model


# Attempt to fit a SARIMA model using aggregated data for Cape Naturaliste (009519).
rainfall.capenaturaliste.weekly.model <-
  auto.arima(ts(rainfall.capenaturaliste.weekly$rainfall, frequency = 365.25/7),
             d = 1, D = 1,
             max.p = 10, max.q = 10, max.P = 10, max.Q = 10, max.d = 1, max.D = 1,
             start.p = 0, start.q = 0, start.P = 0, start.Q = 0,
             seasonal = TRUE)
rainfall.capenaturaliste.weekly.model

#~~~~~ Residual Analysis ~~~~~#

par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mar = c(3,4,3,4))
# Compute standardised residuals, sample acf of residuals, and p-values for Ljung-Box statistic for fitted
# model using aggregated data for Albany (009500).
tsdiag(rainfall.albany.weekly.model)

par(cex.axis = 0.8, cex.lab = 0.7, cex.main = 0.9, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(2,1), mar = c(3,4,3,4))
# Check the normality of residuals for fitted model using aggregated data for Albany (009500).
normal.qqplot(rainfall.albany.weekly.model$residuals)
title(main = "Residual Analysis for Albany (009500)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", line = 2)
hist(rainfall.albany.weekly.model$residuals, breaks = 10, main = "", xlab = "", ylab = "")
box()
title(xlab = "Residuals", ylab = "Frequency", line = 2)


par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mar = c(3,4,3,4))
# Compute standardised residuals, sample acf of residuals, and p-values for Ljung-Box statistic for fitted
# model using aggregated data for Cape Naturaliste (009519).
tsdiag(rainfall.capenaturaliste.weekly.model)

par(cex.axis = 0.8, cex.lab = 0.7, cex.main = 0.9, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(2,1), mar = c(3,4,3,4))
# Check the normality of residuals for fitted model using aggregated data for Cape Naturaliste (009519).
normal.qqplot(rainfall.capenaturaliste.weekly.model$residuals)
title(main = "Residual Analysis for Cape Naturaliste (009519)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", line = 2)
hist(rainfall.capenaturaliste.weekly.model$residuals, breaks = 10, main = "", xlab = "", ylab = "")
box()
title(xlab = "Residuals", ylab = "Frequency", line = 2)


# Perform a formal Ljung-Box test on residuals for each fitted model to assess the suuitability of the fit.
Box.test(rainfall.albany.weekly.model$residuals,
         lag = 365.25/7, type = "Ljung-Box", fitdf = 1)
Box.test(rainfall.capenaturaliste.weekly.model$residuals,
         lag = 365.25/7, type = "Ljung-Box", fitdf = 1)


#~~~~~ Forecasting ~~~~~#

# Seasonal frequency.
m = 365.25/7;


par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mar = c(3,4,3,4))
# Compute forecasted values based on the fitted model with h = m.
rainfall.albany.weekly.forecast <-
  forecast(rainfall.albany.weekly.model, h = m)
# Plot the forecasted values with the original time series.
plot(rainfall.albany.weekly.forecast, type = "l", main = "", xlab = "", ylab ="")
title(main = "Forecasts for Albany (009500) with h = m", xlab = "", ylab = "Rainfall (mm)", line = 2)
# Compute forecasted values based on the fitted model with h = 2m.
rainfall.albany.weekly.forecast <-
  forecast(rainfall.albany.weekly.model, h = 2 * m)
# Plot the forecasted values with the original time series.
plot(rainfall.albany.weekly.forecast, type = "l", main = "", xlab = "", ylab ="")
title(main = "Forecasts for Albany (009500) with h = 2m", xlab = "", ylab = "Rainfall (mm)", line = 2)
# Compute forecasted values based on the fitted model with h = 4m.
rainfall.albany.weekly.forecast <-
  forecast(rainfall.albany.weekly.model, h = 4 * m)
# Plot the forecasted values with the original time series.
plot(rainfall.albany.weekly.forecast, type = "l", main = "", xlab = "", ylab ="")
title(main = "Forecasts for Albany (009500) with h = 4m", xlab = "", ylab = "Rainfall (mm)", line = 2)


par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mar = c(3,4,3,4))
# Compute forecasted values based on the fitted model with h = m.
rainfall.capenaturaliste.weekly.forecast <-
  forecast(rainfall.capenaturaliste.weekly.model, h = m)
# Plot the forecasted values with the original time series.
plot(rainfall.capenaturaliste.weekly.forecast, type = "l", main = "", xlab = "", ylab ="")
title(main = "Forecasts for Cape Naturaliste (009519) with h = m", xlab = "", ylab = "Rainfall (mm)", line = 2)
# Compute forecasted values based on the fitted model with h = 2m.
rainfall.capenaturaliste.weekly.forecast <-
  forecast(rainfall.capenaturaliste.weekly.model, h = 2 * m)
# Plot the forecasted values with the original time series.
plot(rainfall.capenaturaliste.weekly.forecast, type = "l", main = "", xlab = "", ylab ="")
title(main = "Forecasts for Cape Naturaliste (009519) with h = 2m", xlab = "", ylab = "Rainfall (mm)", line = 2)
# Compute forecasted values based on the fitted model with h = 2m.
rainfall.capenaturaliste.weekly.forecast <-
  forecast(rainfall.capenaturaliste.weekly.model, h = 4 * m)
# Plot the forecasted values with the original time series.
plot(rainfall.capenaturaliste.weekly.forecast, type = "l", main = "", xlab = "", ylab ="")
title(main = "Forecasts for Cape Naturaliste (009519) with h = 4m", xlab = "", ylab = "Rainfall (mm)", line = 2)

