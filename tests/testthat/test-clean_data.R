library(dplyr)
library(testthat)

dataclean <- NOAAdata %>%
  eq_clean_data() %>%
  eq_location_clean()

test_that("Clean location", {
  expect_equal(head(dataclean$Location), c("Nisa",
                                                "Turkey",
                                                "Izmir, Efes, Aydin, Manisa, Alasehir, Sart",
                                                "Greece",
                                                "Pakistan" ,
                                                "Kwangju"  ))
})
