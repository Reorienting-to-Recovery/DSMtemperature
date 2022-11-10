library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)

cvpia_watershed <- DSMflow::watershed_ordering$watershed

# 2008 2009 Hec5Q data prep ----------------------------------------------------
# mike wright's notes on temperature

# Here 5q_date is the 6-hourly time stamps on the 5Q output; note that it's on
# the CalLite-CV calendar i.e. starting in 2014 (except that the SJR run starts
# in 2011, but then stops in 2011 and picks back up in 2014 so I deleted the
# 2011 data, and the American River run ends 9 months early... I don't know
# why). I've named each field after its DSM stream; tempoverview.csv contains
# the original 5Q names of the records whose values are copied here. ALL UNITS
# IN FAHRENHEIT!

# read in date mapping calLite -> calsim
cl_dates <- read_csv('data-raw/calLite_calSim_date_mapping.csv')

# clean mike wright's temperature modeling output
temperatures_2008_2009 <- read_csv('data-raw/tempmaster.csv', skip = 1) %>%
  mutate(day_month = str_sub(`5q_date`, 1, 6),
         year = str_sub(`5q_date`, 8, 9),
         year = str_c('20', year),
         date = dmy(paste(day_month, year))) %>%
  select(-day_month, -year, -`5q_date`) %>%
  gather(watershed, temp_F, -date) %>%
  group_by(date, watershed) %>%
  summarise(mean_daily_temp_F = mean(temp_F, na.rm = TRUE),
            mean_daily_temp_C = (mean_daily_temp_F - 32) * (5/9)) %>%
  ungroup()

# mike wright has also provided estimates for Antelope Creek, Bear Creek, Elder
# Creek, Paynes Creek, Bear River, Feather River, and Calaveras River using a
# regression analysis. More details can be found in
# 'data-raw/mike_wright_temperature_regression/create_estimated_timeseries.r'

# add additional modeled temperature data from sadie & erin
monthly_mean_temperature_2008_2009 <- temperatures_2008_2009 %>%
  group_by(year = year(date), month = month(date), watershed) %>%
  summarise(monthly_mean_temp_c = mean(mean_daily_temp_C)) %>%
  ungroup() %>%
  mutate(cl_date = ymd(paste(year, month, 1, sep = '-'))) %>%
  left_join(cl_dates) %>%
  filter(between(year(cs_date), 1980, 2000)) %>%
  mutate(date = ymd(paste(year(cs_date), month(cs_date), 1, sep = '-'))) %>%
  select(date, watershed, monthly_mean_temp_c) %>%
  bind_rows(read_rds('data-raw/big_chico_creek/big_chico_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/butte_creek/butte_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/cosumnes_river/cosumnes_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/deer_creek/deer_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/lower_sacramento/lower_sac_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/mill_creek/mill_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/mokelumne_river/mokelumne_river_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/yuba_river/yuba_river_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/yolo/yolo_bypass_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/sutter/sutter_bypass_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/mike_wright_temperature_regression/juv_temp_regression.rds')) %>%
  spread(watershed, monthly_mean_temp_c) %>%
  filter(year(date) >= 1980 & year(date) <= 2000) %>%
  gather(watershed, monthly_mean_temp_c, -date)



# 2018 2019 Hec5Q data prep ----------------------------------------------------
temperatures_2018_2019 <- read_csv('data-raw/tempmaster_2019_BiOp_update.csv') %>%
  mutate(date = lubridate::as_date(date, format = "%m/%d/%Y")) %>%
  gather(watershed, mean_daily_temp_F, -date) %>%
  mutate(mean_daily_temp_C = (mean_daily_temp_F - 32) * (5/9)) %>% glimpse()

monthly_mean_temperature_2018_2019 <- temperatures_2018_2019 %>%
  group_by(year = year(date), month = month(date), watershed) %>%
  summarise(monthly_mean_temp_c = mean(mean_daily_temp_C)) %>%
  ungroup() %>%
  filter(between(year, 1980, 2000)) %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  select(date, watershed, monthly_mean_temp_c) %>%
  bind_rows(read_rds('data-raw/big_chico_creek/big_chico_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/butte_creek/butte_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/cosumnes_river/cosumnes_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/deer_creek/deer_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/lower_sacramento/lower_sac_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/mill_creek/mill_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/mokelumne_river/mokelumne_river_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/yuba_river/yuba_river_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/yolo/yolo_bypass_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/sutter/sutter_bypass_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/mike_wright_temperature_regression/juv_temp_regression.rds')) %>%
  # TODO add San Joaquin River, Stanislaus River, Merced, & Toulumne need to do regression modeling
  spread(watershed, monthly_mean_temp_c) %>%
  filter(year(date) >= 1980 & year(date) <= 2000) %>%
  gather(watershed, monthly_mean_temp_c, -date)


# stream temperature -----------------------------------------------------------
generate_stream_temperature <- function(monthly_mean_temperature_data) {
stream_temperature <- monthly_mean_temperature_data %>%
  spread(date, monthly_mean_temp_c) %>%
  left_join(DSMflow::watershed_ordering) %>%
  arrange(order) %>%
  select(-watershed, -order) %>%
  DSMflow::create_model_array()

dimnames(stream_temperature) <- list(cvpia_watershed, month.abb, 1980:2000)

return(stream_temperature)
}

stream_temp_2008_2009 <- generate_stream_temperature(monthly_mean_temperature_2008_2009)
# TODO need to add streams so that it works
stream_temp_2018_2019 <- generate_stream_temperature(monthly_mean_temperature_2018_2019)

# create temp with both 2008-2009 biop and 2018-2019 biop/itp ---------------
stream_temperature <- list(biop_2008_2009 = stream_temp_2008_2009,
                           biop_itp_2018_2019 = stream_temp_2018_2019)

usethis::use_data(stream_temperature, overwrite = TRUE)

# delta temps ----------------------------------
dn <- read_rds('data-raw/deltas/north_delta_water_temp_c.rds')
ds <- read_rds('data-raw/deltas/south_delta_water_temp_c.rds')

delta_temperature <- array(NA, dim = c(12, 21, 2))

delta_temperature[ , , 1] <- dn %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year >= 1980 & year <= 2000) %>%
  select(-date) %>%
  spread(year, `North Delta`) %>%
  select(-month) %>%
  as.matrix()

delta_temperature[ , , 2] <- ds %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year >= 1980 & year <= 2000) %>%
  select(-date) %>%
  spread(year, `South Delta`) %>%
  select(-month) %>%
  as.matrix()

dimnames(delta_temperature) <- list(month.abb, 1980:2000, c('North Delta', 'South Delta'))

usethis::use_data(delta_temperature, overwrite = TRUE)

# degree days -----
cl_years <- cl_dates %>%
  mutate(cl_year = year(cl_date),
         cs_year = year(cs_date)) %>%
  select(cl_year, cs_year) %>%
  unique()

# watershed id zeros: 16*, 17, 21, 22, 24, 31 (no spawning)
# *upper mid sac (16) spawning area is represented within upper sac in model
no_spawning_regions <- cvpiaFlow::watershed_ordering %>%
  filter(order %in% c(16, 17, 21, 22, 24, 31)) %>%
  pull(watershed)

hec5q_degree_days <- temperatures %>%
  filter(!(watershed %in% no_spawning_regions)) %>% #no spawning
  group_by(cl_year = year(date), month = month(date), watershed) %>%
  summarise(degdays = sum(mean_daily_temp_C, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(cl_years) %>%
  filter(between(cs_year, 1979, 2000)) %>%
  mutate(date = ymd(paste(cs_year, month, 1, sep = '-'))) %>%
  select(date, watershed, degdays)

# take modeled mean monthly flow and multiple by number of days to estimate degree days
estimate_watersheds <- cvpia_watershed[!cvpia_watershed %in% c(unique(hec5q_degree_days$watershed), no_spawning_regions)]

estimated_degree_days <- monthly_mean_temperature %>%
  mutate(num_days = days_in_month(date),
         degdays = monthly_mean_temp_c * num_days,
         date = ymd(paste(year(date), month(date), 1, sep = '-'))) %>%
  filter(watershed %in% estimate_watersheds) %>%
  select(date, watershed, degdays)

zero_degree_days <- tibble(
  date = rep(seq(as.Date('1979-01-01'), as.Date('2000-12-01'), by = 'month'), each = 6),
  watershed = rep(no_spawning_regions, times = 264),
  degdays = 0
)

degree_days <- zero_degree_days %>%
  bind_rows(hec5q_degree_days) %>%
  bind_rows(estimated_degree_days) %>%
  spread(date, degdays) %>%
  left_join(cvpiaData::watershed_ordering) %>%
  arrange(order) %>%
  select(-watershed, -order) %>%
  cvpiaFlow::create_model_array()

dimnames(degree_days) <- list(cvpia_watershed, month.abb, 1979:2000)

usethis::use_data(degree_days, overwrite = TRUE)

# FR and  SR Egg temperature effect -----
mean_temperature_effect <- read_csv('data-raw/egg2fry_temp.csv') %>%
  mutate(mean_temp_effect = (Dry + Wet)/2) %>%
  select(watershed = Watershed.full, mean_temp_effect) %>%
  pull(mean_temp_effect)


# WR Egg temperature effect -----
wr_egg_temperature_effect <- rep(0.6466230, 31) # Winter-run value was calibrated.

# Combine into dataframe
egg_temperature_effect <- data.frame(watershed = cvpia_watershed,
                                     fall_run = mean_temperature_effect,
                                     spring_run = mean_temperature_effect,
                                     winter_run = wr_egg_temperature_effect)


usethis::use_data(egg_temperature_effect, overwrite = TRUE)

