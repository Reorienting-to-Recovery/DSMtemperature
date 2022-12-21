library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(rnoaa)
library(caret)

# Pull water temperature data --------------------------------------------------
# CDEC (SJR)
# 2008 to present
sjr <- CDECRetrieve::cdec_query(station = 'SJR', sensor_num = '25', dur_code = 'H',
                                start_date = '2008-01-01', end_date = '2022-09-30')

# bcd = durham
san_joaquin_water_temp <- sjr %>%
  filter(!is.na(datetime)) %>%
  select(datetime, temp_f = parameter_value) %>%
  filter(between(temp_f, 10, 100)) %>%
  group_by(year = year(datetime), month = month(datetime)) %>%
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  select(date, mean_water_temp_c)

san_joaquin_water_temp %>%
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

ts_san_joaquin <- ts(san_joaquin_water_temp$mean_water_temp_c, start = c(2000, 01), end = c(2020, 9), frequency = 12)

na.interp(ts_san_joaquin) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_san_joaquin, series = 'Original')

# air temperature data near stream
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# Some potential air temp stations are

# Use Tracy Carbona gage
# model training data 9/1998-9/2017
tracy1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999',
                                startdate = '2001-01-01', datatypeid = 'TAVG',
                                enddate = '2010-12-31', token = token, limit = 120)
tracy2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999',
                                startdate = '2011-01-01', datatypeid = 'TAVG',
                                enddate = '2020-12-31', token = token, limit = 120)



san_joaquin_air_temp <- tracy1$data %>%
  bind_rows(tracy2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)


san_joaquin <- san_joaquin_water_temp %>%
  left_join(san_joaquin_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

san_joaquin %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

san_joaquin_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = san_joaquin)
summary(san_joaquin_model)

san_joaquin_model$coefficients
# air temp thresholds
y <- c(18, 20)
air_temp_thresholds <- (y - san_joaquin_model$coefficients[[1]]) / san_joaquin_model$coefficients[[2]]

pred <- broom::augment(san_joaquin_model) %>% pull(.fitted)
truth <- san_joaquin$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
confusionMatrix(xtab)

# chico univ farm, between chico and durham, GHCND:USC00041715, perfect but too much missing data
# de sabla, GHCND:USC00042402 too north east
# paradise GHCND:USC00046685, most appropriate for san_joaquin creek rearing extent
tracy3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999', startdate = '1979-01-01',
                         datatypeid = 'TAVG', enddate = '1979-12-31', token = token, limit = 12)

tracy4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999', startdate = '1980-01-01',
                         datatypeid = 'TAVG', enddate = '1989-12-31', token = token, limit = 120)

tracy5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999', startdate = '1990-01-01',
                         datatypeid = 'TAVG', enddate = '1999-12-31', token = token, limit = 120)

# Add year 2000
tracy6 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999', startdate = '2000-01-01',
                         datatypeid = 'TAVG', enddate = '2000-12-31', token = token, limit = 120)

tracy3$data %>%
  bind_rows(tracy4$data) %>%
  bind_rows(tracy5$data) %>%
  bind_rows(tracy6$data) %>%
  mutate(date = ymd_hms(date), year = year(date),
         month = factor(month.abb[month(date)],
                        levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
  select(date, month, mean_air_temp_c = value) %>%
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

paradise_air_temp <- tracy3$data %>%
  bind_rows(tracy4$data) %>%
  bind_rows(tracy5$data) %>%
  bind_rows(tracy6$data) %>%
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

# Quite a lot of missing values, we should see if there is a better gage for air temperature
ts_paradise <- ts(paradise_air_temp$mean_air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)
ts_paradise


na.interp(ts_paradise) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_paradise, series = 'Original')

san_joaquin_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_paradise)))


paradise_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = san_joaquin_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

san_joaquin_pred_water_temp <- predict(san_joaquin_model, san_joaquin_air_temp_c)

san_joaquin_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  watershed = 'San Joaquin River',
  monthly_mean_temp_c = san_joaquin_pred_water_temp)

san_joaquin_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = monthly_mean_temp_c))

write_rds(san_joaquin_water_temp_c, 'data-raw/san_joaquin/san_joaquin_creek_water_temp_c.rds')
