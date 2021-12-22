library(tidyverse)
library(testthat)
library(lubridate)


cvpia_watershed <- DSMflow::watershed_ordering$watershed

#Tests in this script test:

#test dimensionality, test for names (watershed rows), test proportion value <= 1,
#test celisus and f are c and f for its range and type
#update documentation as test develops

#Temperature structure ---------------------------------------------------------
test_that("Temperature dimensions", {
  expect_equal(dim(degree_days), c(31, 12, 22))
  expect_equal(dim(delta_temperature), c(12, 21, 2))
  expect_equal(dim(egg_temperature_effect), c(31, 4))
  expect_equal(dim(migratory_temperature_proportion_over_20), c(31, 12))
  expect_equal(dim(prisoners_point_temperature), c(12, 21))
  expect_equal(dim(stream_temperature), c(31, 12, 21))
  expect_equal(dim(vernalis_temperature), c(12, 21))
#Should test for class types?
  })

#Temperature watershed labels --------------------------------------------------
test_that("Temperature watershed labels are the same as cvpia watershed", {
  expect_equal(row.names(degree_days), cvpia_watershed)
  expect_equal(unlist(egg_temperature_effect['watershed'], use.names= FALSE), cvpia_watershed)
  expect_equal(row.names(migratory_temperature_proportion_over_20), cvpia_watershed)
  expect_equal(row.names(stream_temperature), cvpia_watershed)
})

# DSMtemperature::migratory_temperature_proportion_over_20 ---------------------
migratory_temperature_proportion_over_20_df <- migratory_temperature_proportion_over_20 %>%
  as.data.frame() %>%
  rownames_to_column(var = 'location') %>%
  pivot_longer(!location, names_to = "month", values_to = "migratory_temperature_proportion_over_20")

test_that("proportion less or equal to 1", {
  expect_true(all(migratory_temperature_proportion_over_20_df['migratory_temperature_proportion_over_20'] >= 0))
  expect_true(all(migratory_temperature_proportion_over_20_df['migratory_temperature_proportion_over_20'] <= 1))
})

#DSMtemperature::delta_temperature ---------------------------------------------
delta_temperature_df <- map_df(c("North Delta", "South Delta"), function(i){
  delta_temperature[, , i] %>%
    as.data.frame() %>%
    mutate(location = i) %>%
    rownames_to_column(var = 'month') %>%
    pivot_longer(cols = '1980':'2000',
                 names_to ="year",
                 values_to = 'delta_temperature')}) %>%
  pivot_wider(id_cols = c('month','year'),
              names_from = location,
              values_from = delta_temperature) %>%
    rename(n_dlt_delta_temperature = 'North Delta',
           s_dlt_delta_temperature = 'South Delta')

max_river_celsius <- 50
test_that("The delta temperature values are in celsius", {
  expect_true(all(delta_temperature_df['n_dlt_delta_temperature'] < max_river_celsius))
  expect_true(all(delta_temperature_df['s_dlt_delta_temperature'] < max_river_celsius))
})

#fix null values for 1980 stream temperature
#DSMtemperature::stream_temperature---------------------------------------------
stream_temperature_df <- map_df(c(0:21), function(i){
  stream_temperature[, , i] %>%
    as.data.frame() %>%
    mutate(year = i+1980) %>%
    rownames_to_column(var = 'location')})

stream_temperature_df <- stream_temperature_df %>%
  pivot_longer(cols = !c(location, year),
               names_to = "month",
               values_to = 'stream_temperature')

test_that("The stream temperature values are in celsius",{
  expect_true(all(stream_temperature_df['stream_temperature'] < max_river_celsius))
})

# # DSMtemperature::vernalis_temperature----------------------------------------
vernalis_temperature_df <- vernalis_temperature %>%
  as.data.frame() %>%
  rownames_to_column(var = 'month') %>%
  pivot_longer(cols = !month,
               names_to = "year",
               values_to = "vernalis_temperature")

test_that("The vernalis temperature values are in celsius",{
  expect_true(all(vernalis_temperature_df['vernalis_temperature'] < max_river_celsius))
})

# DSMtemperature::prisoners_point_temperature-----------------------------------
prisoners_point_temperature_df <- prisoners_point_temperature %>%
  as.data.frame() %>%
  rownames_to_column(var = 'month') %>%
  pivot_longer(cols = !month,
               names_to = "year",
               values_to = "prisoners_point_temperature")

test_that("The prisoners point temperature values are in celsius",{
  expect_true(all(prisoners_point_temperature_df['prisoners_point_temperature'] < max_river_celsius))
})


