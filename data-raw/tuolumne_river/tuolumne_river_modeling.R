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
# MOD - tuolumne river at modesto
# 2000 to present
mod <- CDECRetrieve::cdec_query(station = 'MOD', sensor_num = '25', dur_code = 'D',
                                start_date = '2000-09-16', end_date = '2022-09-30')

tuolumne_water_temp <- mod %>%
  select(datetime, temp_f = parameter_value) %>%
  filter(between(temp_f, 10, 100)) %>%
  group_by(year = year(datetime), month = month(datetime)) %>%
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  select(date, mean_water_temp_c) %>%
  filter(mean_water_temp_c > 4, mean_water_temp_c < 28)

tuolumne_water_temp %>%
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

ts_tuolumne <- ts(tuolumne_water_temp$mean_water_temp_c, start = c(2000, 01), end = c(2020, 9), frequency = 12)

na.interp(ts_tuolumne) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_tuolumne, series = 'Original')


# water temp test new gage
# TSB is no better
# TSB date range: 2005-2012
# tsb <- CDECRetrieve::cdec_query(station = 'TSB', sensor_num = '25', dur_code = 'H',
#                                 start_date = '2000-09-16', end_date = '2022-09-30')
# tsb <- readxl::read_xlsx("~/Downloads/Tuolumne_CDEC_TSB_dat.xlsx") |>
#   janitor::clean_names() |> rename(datetime = date_time, parameter_value = value) |>
#   mutate(datetime = as.Date(datetime), parameter_value = as.numeric(parameter_value)) |>
#   glimpse()
#
# tuolumne_water_temp <- tsb %>%
#   select(datetime, temp_f = parameter_value) %>%
#   filter(between(temp_f, 10, 100)) %>%
#   group_by(year = year(datetime), month = month(datetime)) %>%
#   summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(date = ymd(paste(year, month, '01', sep = '-')),
#          mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
#   select(date, mean_water_temp_c) %>%
#   filter(mean_water_temp_c > 4, mean_water_temp_c < 28)
#
# tuolumne_water_temp %>%
#   ggplot(aes(x = date, y = mean_water_temp_c)) +
#   geom_col()
#
# ts_tuolumne <- ts(tuolumne_water_temp$mean_water_temp_c, start = c(2000, 01), end = c(2020, 9), frequency = 12)
#
# na.interp(ts_tuolumne) %>% autoplot(series = 'Interpolated') +
#   forecast::autolayer(ts_tuolumne, series = 'Original')


# cimis water temp --------------------------------------------------------
# these are from the California irigation Management Information System (cimis.water.ca.gov)
# map with stations can be found here: https://cimis.water.ca.gov/Stations.aspx
# one anomaly to note: Station 194 on the map renamed in download process to Station 77.
# (on the map, station 77 is north). The data I queried and titled "cimis_77" should be
# from Station 194 on the map.
cimis_71 <- readxl::read_xlsx("~/Downloads/CIMIS_Station71_dat.xlsx") |>
  janitor::clean_names() |> select(date = month_year,
                                   mean_air_temp_f = avg_air_temp_f) |>
  mutate(mean_air_temp_c = (mean_air_temp_f - 32) * 5 / 9)

cimis_77 <- readxl::read_xlsx("~/Downloads/CIMIS_Station77_dat.xlsx") |>
  janitor::clean_names() |> select(date = month_year,
                                   mean_air_temp_f = avg_air_temp_f) |>
  mutate(mean_air_temp_c = (mean_air_temp_f - 32) * 5 / 9)

cimis_71 |> ggplot(aes(x=date, y=mean_air_temp_c)) + geom_col()
cimis_77 |> ggplot(aes(x=date, y=mean_air_temp_c)) + geom_col()

# UCANR data ------------------------------------------------------------
# this is data from the UCANR statewide integrated pest management program
# which lists several stations with air temperature data: http://ipm.ucanr.edu/WEATHER/wxactstnames.html
# "waterford" station is at 37.38N, 120.45W (near UC Merced)

waterford <- read_csv("~/Downloads/waterford_data.csv") |>
  janitor::clean_names() |> mutate(date = ymd(date)) |>
  group_by(year(date), month(date)) |>
  rename(year = `year(date)`, month = `month(date)`) |>
  summarise(mean_air_temp_c = mean(air_max)) |>
  ungroup() |>
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) |>
  select(date, mean_air_temp_c) |> glimpse()
waterford |> ggplot(aes(x=date, y=mean_air_temp_c)) + geom_col()

# air temperature data near stream ---------------------------------------------
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# Currently using
# Modesto airport used because new melones stops collecting data in early 2000 no
# overlap with stream gage data

# model training data 1/2000-12/2021
modesto_airport1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                               startdate = '2001-01-01', datatypeid = 'TAVG',
                               enddate = '2010-12-31', token = token, limit = 120)
modesto_airport2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                               startdate = '2011-01-01', datatypeid = 'TAVG',
                               enddate = '2020-12-31', token = token, limit = 120)

modesto_air_temp <- modesto_airport1$data %>%
  bind_rows(modesto_airport2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>% glimpse()
modesto_air_temp |> ggplot(aes(x=date, y=mean_air_temp_c)) + geom_col()

# which air temperature is best?
# Modesto water temperature and Waterford air temperature have highest Rsquared

# modesto airport (2001-2020)
# tuolumne_air_temp <- modesto_air_temp
# cimis
# tuolumne_air_temp <- cimis_71 # 2000-2022
# tuolumne_air_temp <- cimis_77 # 2000-2022
# UCANR
tuolumne_air_temp <- waterford # 2000-2022

tuolumne <- tuolumne_water_temp %>%
  left_join(tuolumne_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

tuolumne %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

tuolumne_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = tuolumne)
summary(tuolumne_model)

tuolumne_model$coefficients
# air temp thresholds
y <- c(18, 20)
tuolumne_temp_thresholds <- (y - tuolumne_model$coefficients[[1]]) / tuolumne_model$coefficients[[2]]

pred <- broom::augment(tuolumne_model) %>% pull(.fitted)
truth <- tuolumne$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
confusionMatrix(xtab)

# Tracy air gage is no better
# tracy_test_gage_1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999',
#                                  startdate = '2001-01-01', datatypeid = 'TAVG',
#                                  enddate = '2010-12-31', token = token, limit = 120)
# tracy_test_gage_2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999',
#                                  startdate = '2011-01-01', datatypeid = 'TAVG',
#                                  enddate = '2020-12-31', token = token, limit = 120)
#
# tracy_air_temp <- tracy_test_gage_1$data %>%
#   bind_rows(tracy_test_gage_2$data) %>%
#   mutate(date = as_date(ymd_hms(date))) %>%
#   select(date, mean_air_temp_c = value) %>% glimpse()
#
# tuolumne_test <- tuolumne_water_temp %>%
#   left_join(tracy_air_temp) %>%
#   filter(!is.na(mean_air_temp_c))
#
# tuolumne_test %>%
#   ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
#   geom_point() +
#   geom_smooth(method = 'lm', se = FALSE) +
#   geom_hline(yintercept = 18, alpha = .3) +
#   geom_hline(yintercept = 20, alpha = .3)
#
# tuolumne_model_test <- lm(mean_water_temp_c ~ mean_air_temp_c, data = tuolumne_test)
# summary(tuolumne_model_test)
#
# tuolumne_model$coefficients
# # air temp thresholds
# y <- c(18, 20)
# tuolumne_temp_thresholds <- (y - tuolumne_model$coefficients[[1]]) / tuolumne_model$coefficients[[2]]
#
# pred <- broom::augment(tuolumne_model) %>% pull(.fitted)
# truth <- tuolumne$mean_water_temp_c
# xtab <- table(pred > 18, truth > 18)
# xtab <- table(pred > 20, truth > 20)
# confusionMatrix(xtab)

# Waterford air temp data
tuolumne_air <- read_csv("~/Downloads/waterford_full.csv") |>
  janitor::clean_names() |> mutate(date = ymd(date)) |>
  group_by(year(date), month(date)) |>
  rename(year = `year(date)`, month = `month(date)`) |>
  summarise(mean_air_temp_c = mean(air_max)) |>
  ungroup() |>
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) |>
  select(date, mean_air_temp_c)

tuolumne_air |> mutate(month = factor(month.abb[month(date)],
                                      levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) |>
  select(date, month, mean_air_temp_c) |>
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

tuolumne_air_temp <- tuolumne_air |>
  bind_rows(
    tibble(date = seq.Date(ymd('1981-01-01'), ymd('2000-12-01'), by = 'month'),
           mean_air_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(mean_air_temp_c = max(mean_air_temp_c)) %>%
  ungroup() %>%
  mutate(mean_air_temp_c = ifelse(mean_air_temp_c == 0, NA, mean_air_temp_c))


# Modesto airport code - using Waterford (above) instead
# tuolumne_air2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
#                            startdate = '1979-01-01', datatypeid = 'TAVG',
#                            enddate = '1979-12-31', token = token, limit = 12)
#
# tuolumne_air3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
#                            startdate = '1980-01-01', datatypeid = 'TAVG',
#                            enddate = '1989-12-31', token = token, limit = 120)
#
# tuolumne_air4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
#                            startdate = '1990-01-01', datatypeid = 'TAVG',
#                            enddate = '1999-12-31', token = token, limit = 120)
#
# # Add year 2000
# tuolumne_air5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
#                            startdate = '2000-01-01', datatypeid = 'TAVG',
#                            enddate = '2000-12-31', token = token, limit = 120)
#
# tuolumne_air2$data %>%
#   bind_rows(tuolumne_air3$data) %>%
#   bind_rows(tuolumne_air4$data) %>%
#   bind_rows(tuolumne_air5$data) %>%
#   mutate(date = ymd_hms(date), year = year(date),
#          month = factor(month.abb[month(date)],
#                         levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
#   select(date, month, mean_air_temp_c = value) %>%
#   ggplot(aes(x = month, y = mean_air_temp_c)) +
#   geom_boxplot() +
#   geom_point(alpha = .5, pch = 1, size = 1) +
#   labs(y = 'monthly average air temperature (°C)') +
#   theme_minimal()
#
# tuolumne_air_temp <- tuolumne_air2$data %>%
#   bind_rows(tuolumne_air3$data) %>%
#   bind_rows(tuolumne_air4$data) %>%
#   bind_rows(tuolumne_air5$data) %>%
#   mutate(date = as_date(ymd_hms(date))) %>%
#   select(date, mean_air_temp_c = value) %>%
#   bind_rows(
#     tibble(date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
#            mean_air_temp_c = 0)
#   ) %>%
#   group_by(date) %>%
#   summarise(mean_air_temp_c = max(mean_air_temp_c)) %>%
#   ungroup() %>%
#   mutate(mean_air_temp_c = ifelse(mean_air_temp_c == 0, NA, mean_air_temp_c))


ts_tuolumne <- ts(tuolumne_air_temp$mean_air_temp_c, start = c(1981, 1), end = c(2000, 12), frequency = 12)
ts_tuolumne

na.interp(ts_tuolumne) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_tuolumne, series = 'Original')

tuolumne_air_temp_c <- tibble(
  date = seq.Date(ymd('1981-01-01'), ymd('2000-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_tuolumne)))


tuolumne_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = tuolumne_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

tuolumne_pred_water_temp <- predict(tuolumne_model, tuolumne_air_temp_c)

tuolumne_water_temp_c <- tibble(
  date = seq.Date(ymd('1981-01-01'), ymd('2000-12-01'), by = 'month'),
  watershed = 'Tuolumne River',
  monthly_mean_temp_c = tuolumne_pred_water_temp)

tuolumne_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = monthly_mean_temp_c))

write_rds(tuolumne_water_temp_c, 'data-raw/tuolumne_river/tuolumne_river_water_temp_c.rds')
