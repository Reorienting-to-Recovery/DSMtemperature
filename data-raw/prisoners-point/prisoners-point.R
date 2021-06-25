library(CDECRetrieve)
library(tidyverse)
library(lubridate)

# Query for obtaining updated air temps, needs to be filtered to the stockton air
# temperature, and token needs to be obtained from NOAA CDO service.
# stockton_air <-
#   rnoaa::ncdc(datasetid = 'GSOM', locationid = 'CITY:US060042', datatypeid = 'TAVG',
#               startdate = '1998-01-01', enddate = '2007-12-31', token = token, limit = 1000)


# Air temperature at the stockton airport
stockton_air <- read_csv("data-raw/prisoners-point/stockton-air-temp-1979-2020.csv",
                         skip = 1,
                         col_names = c(
                           "station", "name", "date", "temp_avg",
                           "temp_max", "temp_min", "temp_obs"
                         ),
                         col_types = cols(
                           station = col_character(),
                           name = col_character(),
                           date = col_date(format = ""),
                           temp_avg = col_double(),
                           temp_max = col_double(),
                           temp_min = col_double(),
                           temp_obs = col_double()
                         )) %>%
  filter(name == "STOCKTON METROPOLITAN AIRPORT, CA US")

# Clean up the data a bit, most of the time there is no average temp just
# min and max, in this case I am taking the average between the min and max
stockton_air_monthly <- stockton_air %>%
  mutate(air_value = case_when(
    is.na(temp_avg) ~ map2_dbl(temp_max, temp_min, ~mean(c(.x, .y))),
    TRUE ~ temp_avg
  )) %>%
  select(date, air_value) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(
    air_temp = mean(air_value, na.rm = TRUE)
  ) %>% ungroup() %>%
  transmute(
    date = ymd(paste0(year, "-", month, "-01")),
    air_temp
  )

# Water temperature
# rri_water_temp <- cdec_query("rri", "25", "d", "2004-01-01")
# write_rds(rri_water_temp, "data-raw/prisoners-point/rri-water-temps-2004-2020.rds")

rri_water_temp <- read_rds("data-raw/prisoners-point/rri-water-temps-2004-2020.rds")

# clean up some outlier values from water temperatures and left
# join the air temp
ppoint_data <- rri_water_temp %>%
  mutate(parameter_value = ifelse(parameter_value > 150, NA, parameter_value),
         parameter_value = ifelse(parameter_value < 20, NA, parameter_value)) %>%
  group_by(year = year(datetime), month = month(datetime)) %>%
  summarise(
    water_temp = mean(parameter_value, na.rm = TRUE)
  ) %>% ungroup() %>%
  transmute(
    date = ymd(paste0(year, "-", month, "-01")),
    water_temp
  ) %>%
  left_join(stockton_air_monthly, by = c("date" = "date"))


ppoint_data %>%
  ggplot(aes(air_temp, water_temp)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

ppoint_temp_model <- lm(water_temp ~ air_temp, data=ppoint_data)
summary(ppoint_temp_model)

ppoint_model_results <- tibble(
  date = ppoint_data$date,
  observed = ppoint_data$water_temp,
  preds = predict(ppoint_temp_model),
  resid = observed - preds
)

# what do these look when compared to observed results
ppoint_model_results %>%
  ggplot() +
  geom_line(aes(date, observed, color = "Observed")) +
  geom_line(aes(date, preds, color = "Prediction"))

# residual distributions by month
ppoint_model_results %>%
  mutate(month = factor(month.abb[month(date)], levels = month.abb)) %>%
  ggplot(aes(resid, fill = month)) + geom_density(alpha=0.4)

# model input is air temp
model_input <- stockton_air_monthly %>%
  filter(between(year(date), 1980, 2000))

prisoners_point_water_temps <-
  tibble(
    date = model_input$date,
    water_temp = predict(rri_water_temp_model, newdata = model_input[, "air_temp"]),
    location = "Prisoner's Point"
  )

# restructure to fit into cvpiaModels
prisoners_point_temperature <- prisoners_point_water_temps %>%
  transmute(
    year = year(date),
    month = month(date),
    water_temp) %>%
  spread(year, water_temp) %>%
  select(-month) %>%
  as.matrix()

row.names(prisoners_point_temperature) <- month.abb

usethis::use_data(prisoners_point_temperature)










