#' @title Load the raw dataset from a text file
#'
#' @description This function reads the U.S. National Oceanographic
#' and Atmospheric Administration (NOAA) dataset on earthquakes
#'
#' @param filename (optional) the raw NOAA data to be cleaned
#'
#' @return data.frame containing the NOAA data
#'
#' @examples \dontrun{load_raw_dataset()}
#'
#' @export


load_raw_data <- function(filename ="data/NOAA.txt") {

  dataset <- readr::read_delim(filename, show_col_types = FALSE) # read the raw dataset from a txt file

  names(dataset) <- make.names(names(dataset),unique = TRUE) # to convert columns names without spaces

  return(dataset)

}

#' @title Clean the NOAA dataset
#'
#' @description This function cleans and readies the NOAA dataset.
#'
#' @param raw_data the data.frame to be cleaned
#'
#' @return data.frame containing the cleaned data (lubridate ymd format
#' for the date, latitude and longitude converted to numeric)
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{eq_clean_data(NOAAdata)}
#'
#' @export

eq_clean_data <- function(raw_data) {

  clean_data <- raw_data %>%
    dplyr::filter(Year >0) %>%
    dplyr::mutate(dplyr::across(c("Mo","Dy"), ~ dplyr::if_else(is.na(.), 1, .))) %>%
    dplyr::mutate(Year = stringr::str_pad(Year, 4, "left", pad = 0),
                  dplyr::across(c("Mo","Dy"),as.character)) %>%
    tidyr::unite(Date, Year, Mo, Dy, remove = FALSE) %>%
    dplyr::mutate(Date = lubridate::ymd(Date)) %>%
    dplyr::mutate(dplyr::across(c("Year",
                                  "Mo",
                                  "Dy",
                                  "Longitude",
                                  "Latitude"), as.numeric))


  return(clean_data)

}


#' @title Clean the location string
#'
#' @description Split the location / country field in two fields : one for the country
#' and one for the location
#'
#' @param raw_data the data.frame to be cleaned
#'
#' @importFrom magrittr %>%
#'
#' @return a cleaned data frame with two fields for the country and the location
#'
#' @examples \dontrun{eq_location_clean(NOAAdata)}
#'
#' @export



eq_location_clean <- function(raw_data) {

  clean_data <- raw_data %>%
    tidyr::separate(Location.Name, c("Country", "Location"),": ",
                    remove = FALSE,
                    extra = "merge",
                    fill = "left") %>%
    dplyr::mutate(Location = stringr::str_to_title(Location),
                  Location = stringr::str_trim(Location, "left"))

  return(clean_data)

}

