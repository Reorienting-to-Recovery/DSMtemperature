library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(rnoaa)
library(caret)

# hourly water temperature data on BNF (fahrenheit)
#07/29/1998 to present

CDECRetrieve::cdec_stations("BNF")

# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=bnf
bnf <- CDECRetrieve::cdec_query(station = 'BNF', sensor_num = '25', dur_code = 'E',
                                start_date = '2008-02-13', end_date = '2017-06-30')

battle_creek_water_temp <- bnf %>%
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
# stn <- "GHCND:USW00024257"
redding1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024257', startdate = '1998-01-01',
                        datatypeid = 'TAVG', enddate = '2007-12-31', token = token, limit = 120)

redding2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024257', startdate = '2008-01-01',
                        datatypeid = 'TAVG', enddate = '2017-10-31', token = token, limit = 120)


redding_air_temp <- redding1$data %>%
  bind_rows(redding2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)


redding <- battle_creek_water_temp %>%
  left_join(redding_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

redding %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

redding_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = redding)
summary(redding_model)

redding_model$coefficients
# air temp thresholds
y <- c(18, 20)
air_temp_thresholds <- (y - redding_model$coefficients[[1]]) / redding_model$coefficients[[2]]

pred <- broom::augment(redding_model) %>% pull(.fitted)
truth <- redding$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
caret::confusionMatrix(xtab)


redding3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024257', startdate = '1979-01-01',
                        datatypeid = 'TAVG', enddate = '1979-12-31', token = token, limit = 12)

redding4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024257', startdate = '1980-01-01',
                        datatypeid = 'TAVG', enddate = '1989-12-31', token = token, limit = 120)

redding5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024257', startdate = '1990-01-01',
                        datatypeid = 'TAVG', enddate = '1999-12-31', token = token, limit = 120)

# Add year 2000
redding6 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024257', startdate = '2000-01-01',
                        datatypeid = 'TAVG', enddate = '2000-12-31', token = token, limit = 120)

redding3$data %>%
  bind_rows(redding4$data) %>%
  bind_rows(redding5$data) %>%
  bind_rows(redding6$data) %>%
  mutate(date = ymd_hms(date), year = year(date),
         month = factor(month.abb[month(date)],
                        levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
  select(date, month, mean_air_temp_c = value) %>%
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

redding_air_temp <- redding3$data %>%
  bind_rows(redding4$data) %>%
  bind_rows(redding5$data) %>%
  bind_rows(redding6$data) %>%
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


ts_redding <- ts(redding_air_temp$mean_air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)
ts_redding

forecast::na.interp(ts_redding) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_redding, series = 'Original')

redding_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(forecast::na.interp(ts_redding)))


redding_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = redding_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

redding_pred_water_temp <- predict(redding_model, redding_air_temp_c)

redding_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  watershed = 'Battle Creek',
  monthly_mean_temp_c = redding_pred_water_temp)

redding_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = monthly_mean_temp_c))

write_rds(redding_water_temp_c, 'data-raw/battle_creek/battle_creek_water_temp_c.rds')
