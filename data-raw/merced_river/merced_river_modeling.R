library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(forecast)
library(rnoaa)
library(caret)

# Pull water temperature data --------------------------------------------------

# CDEC Gage
# CRS - merced river at cressy
# 2000 to present
crs <- CDECRetrieve::cdec_query(station = 'CRS', sensor_num = '25', dur_code = 'H',
                                start_date = '2000-09-16', end_date = '2021-09-30')

merced_water_temp <- crs %>%
  filter(!is.na(datetime)) %>%
  select(datetime, temp_f = parameter_value) %>%
  filter(between(temp_f, 10, 100)) %>%
  group_by(year = year(datetime), month = month(datetime)) %>%
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  select(date, mean_water_temp_c) %>%
  filter(!is.na(date))

# looks like we will have to stick with 2000 - 2010
merced_water_temp %>%
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

ts_merced <- ts(merced_water_temp$mean_water_temp_c, start = c(2000, 01), end = c(2010, 9), frequency = 12)

na.interp(ts_merced) %>% autoplot(series = 'Interpolated') +
forecast::autolayer(ts_merced, series = 'Original')


# air temperature data near stream ---------------------------------------------
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# Currently using
# Modesto airport used because new melones stops collecting data in early 2000 no
# overlap with stream gage data

# model training data 1/2011-12/2021
merced_air <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045532',
                               startdate = '2000-01-02', datatypeid = 'TAVG',
                               enddate = '2010-01-01', token = token, limit = 120)


merced_air_temp <- merced_air$data %>%
  bind_rows(merced_air$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>% glimpse()


merced <- merced_water_temp %>%
  left_join(merced_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

merced %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

merced_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = merced)
summary(merced_model)

merced_model$coefficients
# air temp thresholds
y <- c(18, 20)
merced_temp_thresholds <- (y - merced_model$coefficients[[1]]) / merced_model$coefficients[[2]]

pred <- broom::augment(merced_model) %>% pull(.fitted)
truth <- merced$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
confusionMatrix(xtab)

# Sticking with modesto airport for now, may want to update later
merced_air2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045532',
                           startdate = '1979-01-01', datatypeid = 'TAVG',
                           enddate = '1979-12-31', token = token, limit = 12)

merced_air3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045532',
                           startdate = '1980-01-01', datatypeid = 'TAVG',
                           enddate = '1989-12-31', token = token, limit = 120)

merced_air4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045532',
                           startdate = '1990-01-01', datatypeid = 'TAVG',
                           enddate = '1999-12-31', token = token, limit = 120)

# Add year 2000
merced_air5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045532',
                           startdate = '2000-01-01', datatypeid = 'TAVG',
                           enddate = '2000-12-31', token = token, limit = 120)

merced_air2$data %>%
  bind_rows(merced_air3$data) %>%
  bind_rows(merced_air4$data) %>%
  bind_rows(merced_air5$data) %>%
  mutate(date = ymd_hms(date), year = year(date),
         month = factor(month.abb[month(date)],
                        levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
  select(date, month, mean_air_temp_c = value) %>%
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

merced_air_temp <- merced_air2$data %>%
  bind_rows(merced_air3$data) %>%
  bind_rows(merced_air4$data) %>%
  bind_rows(merced_air5$data) %>%
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


ts_merced <- ts(merced_air_temp$mean_air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)
ts_merced

na.interp(ts_merced) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_merced, series = 'Original')

merced_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_merced)))


modesto_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = merced_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

merced_pred_water_temp <- predict(merced_model, merced_air_temp_c)

merced_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  watershed = 'Butte Creek',
  monthly_mean_temp_c = merced_pred_water_temp)

merced_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = monthly_mean_temp_c))

write_rds(merced_water_temp_c, 'data-raw/merced_river/merced_river_water_temp_c.rds')
