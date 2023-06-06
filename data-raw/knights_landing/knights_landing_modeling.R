library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(rnoaa)
library(caret)

# hourly water temperature data on VON (fahrenheit)
#07/29/1998 to present

CDECRetrieve::cdec_stations("VON")

# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=VON
# VON was closest location to Knights Landing and on the Sacramento R that had water temperature
# Station located downstream of Knights Landing
von <- CDECRetrieve::cdec_query(station = 'VON', sensor_num = '25', dur_code = 'E',
                                start_date = '2008-02-13', end_date = '2017-06-30')

# bcd = durham
knights_landing_water_temp <- von %>%
  select(datetime, temp_f = parameter_value) %>%
  filter(between(temp_f, 10, 100)) %>%
  group_by(year = year(datetime), month = month(datetime)) %>%
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  select(date, mean_water_temp_c) %>%
  filter(!is.na(date))

# air temperature data near stream
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# model training data 9/1998-9/2017
# stn <- "GHCND:USC00049781"
knights_ferry1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00049781', startdate = '1998-01-01',
                         datatypeid = 'TAVG', enddate = '2007-12-31', token = token, limit = 120)

knights_ferry2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00049781', startdate = '2008-01-01',
                         datatypeid = 'TAVG', enddate = '2017-10-31', token = token, limit = 120)


knights_landing_air_temp <- knights_ferry1$data %>%
  bind_rows(knights_ferry2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)


knights_landing <- knights_landing_water_temp %>%
  left_join(knights_landing_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

knights_landing %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

knights_landing_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = knights_landing)
summary(knights_landing_model)

knights_landing_model$coefficients
# air temp thresholds
y <- c(18, 20)
air_temp_thresholds <- (y - knights_landing_model$coefficients[[1]]) / knights_landing_model$coefficients[[2]]

pred <- broom::augment(knights_landing_model) %>% pull(.fitted)
truth <- knights_landing$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
caret::confusionMatrix(xtab)


knights_ferry3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00049781', startdate = '1979-01-01',
                         datatypeid = 'TAVG', enddate = '1979-12-31', token = token, limit = 12)

knights_ferry4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00049781', startdate = '1980-01-01',
                         datatypeid = 'TAVG', enddate = '1989-12-31', token = token, limit = 120)

knights_ferry5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00049781', startdate = '1990-01-01',
                         datatypeid = 'TAVG', enddate = '1999-12-31', token = token, limit = 120)

# Add year 2000
knights_ferry6 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00049781', startdate = '2000-01-01',
                         datatypeid = 'TAVG', enddate = '2000-12-31', token = token, limit = 120)

knights_ferry3$data %>%
  bind_rows(knights_ferry4$data) %>%
  bind_rows(knights_ferry5$data) %>%
  bind_rows(knights_ferry6$data) %>%
  mutate(date = ymd_hms(date), year = year(date),
         month = factor(month.abb[month(date)],
                        levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
  select(date, month, mean_air_temp_c = value) %>%
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

knights_ferry_air_temp <- knights_ferry3$data %>%
  bind_rows(knights_ferry4$data) %>%
  bind_rows(knights_ferry5$data) %>%
  bind_rows(knights_ferry6$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
           mean_air_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(mean_air_temp_c = max(mean_air_temp_c)) %>%
  ungroup() %>%
  mutate(mean_air_temp_c = ifelse(mean_air_temp_c == 0, NA, mean_air_temp_c))


ts_knights_ferry <- ts(knights_ferry_air_temp$mean_air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)
ts_knights_ferry

na.interp(ts_knights_ferry) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_knights_ferry, series = 'Original')

knights_landing_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_knights_ferry)))


knights_ferry_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = knights_landing_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

knights_landing_pred_water_temp <- predict(knights_landing_model, knights_landing_air_temp_c)

knights_landing_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  watershed = 'Lower-mid Sacramento River',
  monthly_mean_temp_c = knights_landing_pred_water_temp)

knights_landing_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = monthly_mean_temp_c))

write_rds(knights_landing_water_temp_c, 'data-raw/knights_landing/lower_mid_sacramento_river_water_temp_c.rds')
