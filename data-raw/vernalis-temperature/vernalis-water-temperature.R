library(CDECRetrieve)
library(tidyverse)
library(lubridate)

# Air temperature-----------------------

stockton_air <- read_csv("data-raw/vernalis-temperature/stockton-air-temp-1979-2020.csv",
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


# Water Temperature --------------------
cdec_datasets("sjr")

sjr_water_temp_data_raw <- cdec_query(station = "sjr", sensor_num = "25", dur_code = "e",
           start_date = "2009-01-01")

sjr_water_temp_data <- sjr_water_temp_data_raw %>%
  mutate(
    parameter_value = ifelse(parameter_value > 120, NA_real_, parameter_value),
    parameter_value = ifelse(parameter_value < 40, NA_real_, parameter_value)
  )

sjr_monthly_temps <- sjr_water_temp_data %>%
  select(datetime, parameter_value) %>%
  group_by(year = year(datetime), month = month(datetime)) %>%
  summarise(
    water_temp = mean(parameter_value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    date = ymd(paste0(year, "-", month, "-01"))
  ) %>%
  select(date, water_temp)

sjr_monthly_temps %>%
  ggplot(aes(date, water_temp)) + geom_line()



water_air_temps <- stockton_air_monthly %>%
  left_join(sjr_monthly_temps, by = c())


# Explore relationship

water_air_temps %>%
  ggplot(aes(air_temp, water_temp)) +
  geom_point() +
  geom_smooth(method = "lm")

sjr_model <- lm(water_temp ~ air_temp, data = water_air_temps)
summary(sjr_model)

model_input <- stockton_air_monthly %>%
  filter(year(date) >= 1970, year(date) <= 2000)

vernalis_water_temperature_df <- tibble(
  date = model_input$date,
  water_temp = predict(sjr_model, newdata = model_input[, "air_temp"])
)


vernalis_water_temperature_df %>%
  ggplot(aes(date, water_temp)) + geom_line()



# restructure for cvpiaModels package
vernalis_water_temperature <-
  vernalis_water_temperature_df %>%
  mutate(year = year(date), month = month(date)) %>%
  select(-date) %>%
  spread(year, water_temp) %>%
  select(-month) %>%
  as.matrix()

row.names(vernalis_water_temperature) <- month.abb


usethis::use_data(vernalis_water_temperature, overwrite = TRUE)

