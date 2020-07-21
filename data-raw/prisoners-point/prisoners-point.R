library(CDECRetrieve)
library(tidyverse)
library(lubridate)

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
rri_water_temp <- cdec_query("rri", "25", "d", "2004-01-01")

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
    water_temp,
    water_year = case_when(
      month(date) %in% 10:12 ~ year(date) + 1,
      TRUE ~ year(date)
    )
  ) %>%
  left_join(stockton_air_monthly, by = c("date" = "date"))


ppoint_data %>%
  ggplot(aes(air_temp, water_temp)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

ppoint_temp_model <- lm(water_temp ~ air_temp, data=rri_water)
summary(ppoint_temp_model)

ppoint_model_results <- tibble(
  date = ppoint_data$date,
  observed = ppoint_data$water_temp,
  preds = predict(ppoint_temp_model),
  resid = observed - preds
)

# what do these look when compared to observed results
rri_water_temp_model_results %>%
  ggplot() +
  geom_line(aes(date, observed, color = "Observed")) +
  geom_line(aes(date, preds, color = "Prediction"))

# residual distributions by month
rri_water_temp_model_results %>%
  mutate(month = factor(month.abb[month(date)], levels = month.abb)) %>%
  ggplot(aes(resid, fill = month)) + geom_density(alpha=0.4)

# model input is air temp
model_input <- stockton_air_monthly %>%
  filter(between(year(date), 1979, 2000))

prisoners_point_water_temps <-
  tibble(
    date = model_input$date,
    water_temp = predict(rri_water_temp_model, newdata = model_input[, "air_temp"]),
    location = "Prisoner's Point"
  )


