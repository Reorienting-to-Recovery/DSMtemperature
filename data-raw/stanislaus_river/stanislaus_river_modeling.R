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
# RIP - stanislaus river at Ripon
# 07/29/1998 to present
rip <- CDECRetrieve::cdec_query(station = 'RIP', sensor_num = '25', dur_code = 'E',
                                start_date = '2000-09-16', end_date = '2021-09-30')

# USGS gage
# 11303000

stanislaus_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11303000', parameterCd = '00010',
                                                   startDate = '2007-10-01', endDate = '2022-11-10',
                                                   statCd = c('00001', '00002'))

stanislaus_water_temp %>% summarise(min(Date), max(Date))

stan_water <- stanislaus_water_temp %>%
  select(date = Date, max = X_00010_00001, min = X_00010_00002) %>%
  mutate(mean_min_max = (max + min) / 2) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_temp_c = mean(mean_min_max, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  ungroup() %>%
  select(date, mean_water_temp_c)


stan_water %>%
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

#  ts_stan <- ts(stan_water$mean_water_temp_c, start = c(1989, 01), end = c(2003, 9), frequency = 12)

# na.interp(ts_stan) %>% autoplot(series = 'Interpolated') +
#   forecast::autolayer(ts_stan, series = 'Original')


# air temperature data near stream ---------------------------------------------
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# Currently using
# Modesto airport used because new melones stops collecting data in early 2000 no
# overlap with stream gage data

# model training data 1/2011-12/2021
modesto_airport <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                               startdate = '2011-01-01', datatypeid = 'TAVG',
                               enddate = '2020-12-31', token = token, limit = 120)


stan_air_temp <- modesto_airport$data %>%
  bind_rows(modesto_airport$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>% glimpse()


stan <- stan_water %>%
  left_join(stan_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

stan %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

stan_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = stan)
summary(stan_model)

stan_model$coefficients
# air temp thresholds
y <- c(18, 20)
stan_temp_thresholds <- (y - stan_model$coefficients[[1]]) / stan_model$coefficients[[2]]

pred <- broom::augment(stan_model) %>% pull(.fitted)
truth <- stan$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
confusionMatrix(xtab)

# Sticking with modesto airport for now, may want to update later
modesto_airport2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                                startdate = '1979-01-01', datatypeid = 'TAVG',
                                enddate = '1979-12-31', token = token, limit = 12)

modesto_airport3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                                startdate = '1980-01-01', datatypeid = 'TAVG',
                                enddate = '1989-12-31', token = token, limit = 120)

modesto_airport4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                                startdate = '1990-01-01', datatypeid = 'TAVG',
                         enddate = '1999-12-31', token = token, limit = 120)

# Add year 2000
modesto_airport5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                                startdate = '2000-01-01', datatypeid = 'TAVG',
                                enddate = '2000-12-31', token = token, limit = 120)

modesto_airport2$data %>%
  bind_rows(modesto_airport3$data) %>%
  bind_rows(modesto_airport4$data) %>%
  bind_rows(modesto_airport5$data) %>%
  mutate(date = ymd_hms(date), year = year(date),
         month = factor(month.abb[month(date)],
                        levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
  select(date, month, mean_air_temp_c = value) %>%
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

modesto_air_temp <- modesto_airport2$data %>%
  bind_rows(modesto_airport3$data) %>%
  bind_rows(modesto_airport4$data) %>%
  bind_rows(modesto_airport5$data) %>%
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


ts_modesto <- ts(modesto_air_temp$mean_air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)
ts_modesto

na.interp(ts_modesto) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_modesto, series = 'Original')

stanislaus_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_modesto)))


modesto_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = stanislaus_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

stan_pred_water_temp <- predict(stan_model, stanislaus_air_temp_c)

stan_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  watershed = 'Stanislaus River',
  monthly_mean_temp_c = stan_pred_water_temp)

stan_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = monthly_mean_temp_c))

write_rds(stan_water_temp_c, 'data-raw/stanislaus_river/stanislaus_river_water_temp_c.rds')
