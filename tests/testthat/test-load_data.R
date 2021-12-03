library(dplyr)
library(testthat)

dataclean <- NOAAdata %>%
  eq_clean_data() %>%
  eq_location_clean()

test_that("Data loading and cleaning is ok", {
  expect_true(class(dataclean$Date) == "Date")
  expect_true(class(dataclean$Longitude) == "numeric")
  expect_true(class(dataclean$Latitude) == "numeric")
})
