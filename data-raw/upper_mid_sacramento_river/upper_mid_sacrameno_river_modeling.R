library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(rnoaa)
library(caret)

# hourly water temperature data on RDB (fahrenheit)
#07/29/1998 to present

CDECRetrieve::cdec_stations("RDB")

# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=rdb
rdb <- CDECRetrieve::cdec_query(station = 'RDB', sensor_num = '25', dur_code = 'H',
                                start_date = '2008-02-13', end_date = '2017-06-30')

upper_mid_water_temp <- rdb %>%
  select(datetime, temp_f = parameter_value) %>%
  filter(between(temp_f, 10, 100)) %>%
  group_by(year = year(datetime), month = month(datetime)) %>%
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  select(date, mean_water_temp_c) %>%
  filter(!is.na(date))

rdb %>%
  select(datetime, temp_f = parameter_value) %>%
  filter(between(temp_f, 10, 100)) |>
  mutate(mean_water_temp_c = (temp_f - 32) * 5 / 9) |>
  ggplot() +
  geom_line(aes(x = datetime, y = mean_water_temp_c))

ggplot() +
  geom_line(data = upper_mid_water_temp, aes(x = date, y = mean_water_temp_c))

# air temperature data near stream
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# model training data 9/1998-9/2017
redbluff1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', startdate = '1998-01-01',
                         datatypeid = 'TAVG', enddate = '2007-12-31', token = token, limit = 120)

redbluff2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', startdate = '2008-01-01',
                         datatypeid = 'TAVG', enddate = '2017-10-31', token = token, limit = 120)


redbluff_air_temp <- redbluff1$data %>%
  bind_rows(redbluff2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)


redbluff <- upper_mid_water_temp %>%
  left_join(redbluff_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

redbluff %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

redbluff_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = redbluff)
summary(redbluff_model)

redbluff_model$coefficients
# air temp thresholds
y <- c(18, 20)
air_temp_thresholds <- (y - redbluff_model$coefficients[[1]]) / redbluff_model$coefficients[[2]]

pred <- broom::augment(redbluff_model) %>% pull(.fitted)
truth <- redbluff$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)


redbluff3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', startdate = '1979-01-01',
                         datatypeid = 'TAVG', enddate = '1979-12-31', token = token, limit = 12)

redbluff4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', startdate = '1980-01-01',
                         datatypeid = 'TAVG', enddate = '1989-12-31', token = token, limit = 120)

redbluff5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', startdate = '1990-01-01',
                         datatypeid = 'TAVG', enddate = '1999-12-31', token = token, limit = 120)

# Add year 2000
redbluff6 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', startdate = '2000-01-01',
                         datatypeid = 'TAVG', enddate = '2000-12-31', token = token, limit = 120)

redbluff3$data %>%
  bind_rows(redbluff4$data) %>%
  bind_rows(redbluff5$data) %>%
  bind_rows(redbluff6$data) %>%
  mutate(date = ymd_hms(date), year = year(date),
         month = factor(month.abb[month(date)],
                        levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
  select(date, month, mean_air_temp_c = value) %>%
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

redbluff_air_temp <- redbluff3$data %>%
  bind_rows(redbluff4$data) %>%
  bind_rows(redbluff5$data) %>%
  bind_rows(redbluff6$data) %>%
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


ts_redbluff <- ts(redbluff_air_temp$mean_air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)
ts_redbluff

forecast::na.interp(ts_redbluff) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_redbluff, series = 'Original')

redbluff_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(forecast::na.interp(ts_redbluff)))


redbluff_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = redbluff_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

redbluff_pred_water_temp <- predict(redbluff_model, redbluff_air_temp_c)

redbluff_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  watershed = 'Upper-mid Sacramento River',
  monthly_mean_temp_c = redbluff_pred_water_temp)

redbluff_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = monthly_mean_temp_c))

write_rds(redbluff_water_temp_c, 'data-raw/upper_mid_sacramento_river/upper_mid_sacramento_river_water_temp_c.rds')
