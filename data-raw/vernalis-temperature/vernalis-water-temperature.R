library(CDECRetrieve)
library(tidyverse)
library(lubridate)

cdec_datasets("sjr")

sjr_water_temp_data <- cdec_query(station = "sjr", sensor_num = "25", dur_code = "e",
           start_date = "2010-10-21")

