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
  filter(between(year(cs_date), 1979, 2000)) %>%
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
  filter(year(date) >= 1979 & year(date) <= 2000) %>%
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
  filter(between(year, 1979, 2000)) %>%
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
  bind_rows(read_rds('data-raw/san_joaquin/san_joaquin_creek_water_temp_c.rds')) %>%  # TODO check regressions
  bind_rows(read_rds('data-raw/stanislaus_river/stanislaus_river_water_temp_c.rds')) %>% # TODO check regressions
  bind_rows(read_rds('data-raw/merced_river/merced_river_water_temp_c.rds')) %>% # TODO check regressions
  bind_rows(read_rds('data-raw/tuolumne_river/tuolumne_river_water_temp_c.rds')) %>% # TODO check regressions
  bind_rows(read_rds('data-raw/mike_wright_temperature_regression/juv_temp_regression.rds')) %>%
  spread(watershed, monthly_mean_temp_c) %>%
  filter(year(date) >= 1979 & year(date) <= 2000) %>%
  gather(watershed, monthly_mean_temp_c, -date)


# Calsim Run of River temperature data prep -------------------------------
run_of_river_tmp <- readxl::read_excel('data-raw/hec5_sac_update.xlsx') %>%
  bind_cols(readxl::read_excel("data-raw/hec5_american.xlsx")) |>
  rename('date' = `...2`) |>
  select(date, `Clear Creek`, `Cottonwood Creek`, `Stony Creek`, `Cow Creek`, `American River`) |>
  pivot_longer(cols = c(`Clear Creek`:`American River`), names_to = "watershed") |>
  rename('mean_daily_temp_F' = 'value') |>
  mutate(mean_daily_temp_C = (mean_daily_temp_F - 32) * (5/9))

run_of_river_pre_2003 <- run_of_river_tmp |>
  filter(year(date) <= 2003)

temperatures_run_of_river <- run_of_river_tmp |>
  filter(year(date) > 2003) |>
  mutate(date = as.Date(date)-lubridate::years(100)) |>
  bind_rows(run_of_river_pre_2003) |>
  filter(year(date) >= 1922, year(date) <= 2002)

unique(temperatures_run_of_river$watershed)

monthly_mean_temperature_run_of_river <- temperatures_run_of_river |>
  group_by(year = year(date), month = month(date), watershed) %>%
  summarise(monthly_mean_temp_c = mean(mean_daily_temp_C)) %>%
  ungroup() %>%
  filter(between(year, 1979, 2000)) %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  select(date, watershed, monthly_mean_temp_c) |>
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
  bind_rows(read_rds('data-raw/san_joaquin/san_joaquin_creek_water_temp_c.rds')) %>%  # TODO check regressions
  bind_rows(read_rds('data-raw/stanislaus_river/stanislaus_river_water_temp_c.rds')) %>% # TODO check regressions
  bind_rows(read_rds('data-raw/merced_river/merced_river_water_temp_c.rds')) %>% # TODO check regressions
  bind_rows(read_rds('data-raw/tuolumne_river/tuolumne_river_water_temp_c.rds')) %>% # TODO check regressions
  bind_rows(read_rds('data-raw/mike_wright_temperature_regression/juv_temp_regression.rds')) %>%
  bind_rows(read_rds('data-raw/lower_mid_sacramento_river/lower_mid_sacramento_river_water_temp_c.rds')) |>
  bind_rows(read_rds('data-raw/battle_creek/battle_creek_water_temp_c.rds')) |>
  bind_rows(read_rds('data-raw/upper_sacramento_river/upper_sacramento_river_water_temp_c.rds')) |>
  bind_rows(read_rds('data-raw/upper_mid_sacramento_river/upper_mid_sacramento_river_water_temp_c.rds')) |>
  bind_rows(monthly_mean_temperature_2018_2019 |> filter(watershed == "Thomes Creek")) |> #TODO: double check this logic since there is no water temperature gage on Thomes crek
  spread(watershed, monthly_mean_temp_c) %>%
  filter(year(date) >= 1979 & year(date) <= 2000) %>%
  gather(watershed, monthly_mean_temp_c, -date)

length(unique(monthly_mean_temperature_run_of_river$watershed))
setdiff(fallRunDSM::watershed_labels, monthly_mean_temperature_run_of_river$watershed)

ggplot() +
  geom_line(data = monthly_mean_temperature_run_of_river, aes(x = date, y = monthly_mean_temp_c, color = "run of river")) +
  geom_line(data = monthly_mean_temperature_2018_2019, aes(x = date, y = monthly_mean_temp_c, color = "2019 Biop")) +
  geom_line(data = monthly_mean_temperature_2008_2009, aes(x = date, y = monthly_mean_temp_c, color = "2008 Biop")) +
  facet_wrap(~watershed)

# stream temperature -----------------------------------------------------------
generate_stream_temperature <- function(monthly_mean_temperature_data) {
stream_temperature <- monthly_mean_temperature_data %>%
  filter(year(date) >= 1980 & year(date) <= 2000) %>%
  spread(date, monthly_mean_temp_c) %>%
  left_join(DSMflow::watershed_ordering) %>%
  arrange(order) %>%
  select(-watershed, -order) %>%
  DSMflow::create_model_array()

dimnames(stream_temperature) <- list(cvpia_watershed, month.abb, 1980:2000)

return(stream_temperature)
}

stream_temp_2008_2009 <- generate_stream_temperature(monthly_mean_temperature_2008_2009)
stream_temp_2018_2019 <- generate_stream_temperature(monthly_mean_temperature_2018_2019)
stream_temp_run_of_river <- generate_stream_temperature(monthly_mean_temperature_run_of_river)


# Check how new modeled results compare to old calsim results
# TODO more digging into temp modeling on these ones
# San Joaquin - temps tend to look slightly lower mostly pretty similar - largest difference -4.4 degs
# Merced - temps tend to look slightly lower mostly pretty similar - largest difference -5.6 degs
# Tuolumne - temps look like they are often higher in summer/fall and colder in winter biggest difference is 12 degrees warmer
# Stanislaus - temps look warmer biggest difference is 11.35 degrees

# create temp with both 2008-2009 biop and 2018-2019 biop/itp ---------------
stream_temperature <- list(biop_2008_2009 = stream_temp_2008_2009,
                           biop_itp_2018_2019 = stream_temp_2018_2019,
                           run_of_river = stream_temp_run_of_river)

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
no_spawning_regions <- DSMflow::watershed_ordering %>%
  filter(order %in% c(16, 17, 21, 22, 24, 31)) %>%
  pull(watershed)

no_spawning_regions_sr <- DSMflow::watershed_ordering %>%
  filter(order %in% c(16, 17, 21, 22, 24)) %>%
  pull(watershed)


# take modeled mean monthly flow and multiple by number of days to estimate degree days
generate_degree_days <- function(monthly_mean_temperature, temperatures, hec_version, no_spawning_regions) {

  if (hec_version == "2008 & 2009 Hec5q") {
  hec5q_degree_days <- temperatures %>%
    filter(!(watershed %in% no_spawning_regions)) %>% #no spawning
    group_by(cl_year = year(date), month = month(date), watershed) %>%
    summarise(degdays = sum(mean_daily_temp_C, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(cl_years) %>%
    filter(between(cs_year, 1979, 2000)) %>%
    mutate(date = ymd(paste(cs_year, month, 1, sep = '-'))) %>%
    select(date, watershed, degdays)
  } else {
    hec5q_degree_days <- temperatures %>%
      filter(!(watershed %in% no_spawning_regions)) %>% #no spawning
      group_by(year = year(date), month = month(date), watershed) %>%
      summarise(degdays = sum(mean_daily_temp_C, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(between(year, 1979, 2000)) %>%
      mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
      select(date, watershed, degdays)
  }

  estimate_watersheds <- cvpia_watershed[!cvpia_watershed %in%
                                           c(unique(hec5q_degree_days$watershed),
                                                                 no_spawning_regions)]

  estimated_degree_days <- monthly_mean_temperature %>%
    mutate(num_days = days_in_month(date),
           degdays = monthly_mean_temp_c * num_days,
           date = ymd(paste(year(date), month(date), 1, sep = '-'))) %>%
    filter(watershed %in% estimate_watersheds) %>%
    select(date, watershed, degdays)

  zero_degree_days <- tibble(
    date = rep(seq(as.Date('1979-01-01'), as.Date('2000-12-01'), by = 'month'), each = length(no_spawning_regions)),
    watershed = rep(no_spawning_regions, times = 264),
    degdays = 0
  )

  degree_days <- zero_degree_days %>%
    bind_rows(hec5q_degree_days) %>%
    bind_rows(estimated_degree_days) %>%
    spread(date, degdays) %>%
    left_join(DSMflow::watershed_ordering) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    DSMflow::create_model_array()
  # TODO fix how we handle NAs in 1979 - not sure how we avoided this before - for now replaceing with 0
  degree_days[is.na(degree_days)] <- 0
  dimnames(degree_days) <- list(cvpia_watershed, month.abb, 1979:2000)
  return(degree_days)
}

degree_days_2008_2009 <- generate_degree_days(monthly_mean_temperature_2008_2009,
                                              temperatures_2008_2009, "2008 & 2009 Hec5q", no_spawning_regions)
degree_days_2018_2019 <- generate_degree_days(monthly_mean_temperature_2018_2019,
                                              temperatures_2018_2019, "2018 & 2019 Hec5q", no_spawning_regions)
degree_days_run_of_river <- generate_degree_days(monthly_mean_temperature_run_of_river,
                                              temperatures_run_of_river, "Run of River Hec5q", no_spawning_regions)

degree_days <- list(biop_2008_2009 = degree_days_2008_2009,
                    biop_itp_2018_2019 = degree_days_2018_2019,
                    run_of_river = degree_days_run_of_river)

usethis::use_data(degree_days, overwrite = TRUE)

# spring run degree days:
degree_days_2008_2009_sr <- generate_degree_days(monthly_mean_temperature_2008_2009,
                                              temperatures_2008_2009, "2008 & 2009 Hec5q", no_spawning_regions_sr)
degree_days_2018_2019_sr <- generate_degree_days(monthly_mean_temperature_2018_2019,
                                              temperatures_2018_2019, "2018 & 2019 Hec5q", no_spawning_regions_sr)
degree_days_run_of_river_sr <- generate_degree_days(monthly_mean_temperature_run_of_river,
                                                 temperatures_run_of_river, "Run of River Hec5q", no_spawning_regions_sr)

degree_days_sr <- list(biop_2008_2009 = degree_days_2008_2009_sr,
                    biop_itp_2018_2019 = degree_days_2018_2019_sr,
                    run_of_river = degree_days_run_of_river_sr)

usethis::use_data(degree_days_sr, overwrite = TRUE)

# degree days for above dam that max out at 17 degrees
# TODO: is 17 the correct threshold value. Double check.
# 2008 2009
monthly_mean_temperature_2008_2009_abv_dam <- monthly_mean_temperature_2008_2009 |>
  mutate(monthly_mean_temp_c = ifelse(monthly_mean_temp_c >= 17, 17, monthly_mean_temp_c))
temperatures_2008_2009_abv_dam <- temperatures_2008_2009 |>
  mutate(mean_daily_temp_C = ifelse(mean_daily_temp_C >= 17, 17, mean_daily_temp_C),
         mean_daily_temp_F = ifelse(mean_daily_temp_F >= 62.6, 62.6, mean_daily_temp_F))

degree_days_2008_2009_sr_abv_dam <- generate_degree_days(monthly_mean_temperature_2008_2009_abv_dam,
                                                         temperatures_2008_2009_abv_dam, "2008 & 2009 Hec5q", no_spawning_regions_sr)
## 2018 2019
monthly_mean_temperature_2018_2019_abv_dam <- monthly_mean_temperature_2018_2019 |>
  mutate(monthly_mean_temp_c = ifelse(monthly_mean_temp_c >= 17, 17, monthly_mean_temp_c))
temperatures_2018_2019_abv_dam <- temperatures_2018_2019 |>
  mutate(mean_daily_temp_C = ifelse(mean_daily_temp_C >= 17, 17, mean_daily_temp_C),
         mean_daily_temp_F = ifelse(mean_daily_temp_F >= 62.6, 62.6, mean_daily_temp_F))

degree_days_2018_2019_sr_abv_dam <- generate_degree_days(monthly_mean_temperature_2018_2019_abv_dam,
                                                         temperatures_2018_2019_abv_dam, "2008 & 2009 Hec5q", no_spawning_regions_sr)
## run of river
monthly_mean_temperature_run_of_river_abv_dam <- monthly_mean_temperature_run_of_river |>
  mutate(monthly_mean_temp_c = ifelse(monthly_mean_temp_c >= 17, 17, monthly_mean_temp_c))
temperatures_run_of_river_abv_dam <- temperatures_run_of_river |>
  mutate(mean_daily_temp_C = ifelse(mean_daily_temp_C >= 17, 17, mean_daily_temp_C),
         mean_daily_temp_F = ifelse(mean_daily_temp_F >= 62.6, 62.6, mean_daily_temp_F))

degree_days_run_of_river_sr_abv_dam <- generate_degree_days(monthly_mean_temperature_run_of_river_abv_dam,
                                                            temperatures_run_of_river_abv_dam, "2008 & 2009 Hec5q", no_spawning_regions_sr)

degree_days_sr_abv_dam <- list(biop_2008_2009 = degree_days_2008_2009_sr_abv_dam,
                       biop_itp_2018_2019 = degree_days_2018_2019_sr_abv_dam,
                       run_of_river = degree_days_run_of_river_sr_abv_dam)

usethis::use_data(degree_days_sr_abv_dam, overwrite = TRUE)

# FR and  SR Egg temperature effect -----
mean_temperature_effect <- read_csv('data-raw/egg2fry_temp.csv') %>%
  mutate(mean_temp_effect = (Dry + Wet)/2) %>%
  select(watershed = Watershed.full, mean_temp_effect) %>%
  pull(mean_temp_effect)

# spring temperature effect for SJ same as Tuolumne
spring_temperature_effect <- mean_temperature_effect
spring_temperature_effect[31] <- spring_temperature_effect[30]

# WR Egg temperature effect -----
wr_egg_temperature_effect <- rep(0.6466230, 31) # Winter-run value was calibrated.

# Combine into dataframe
egg_temperature_effect <- data.frame(watershed = cvpia_watershed,
                                     fall_run = mean_temperature_effect,
                                     spring_run = spring_temperature_effect,
                                     winter_run = wr_egg_temperature_effect)


usethis::use_data(egg_temperature_effect, overwrite = TRUE)

