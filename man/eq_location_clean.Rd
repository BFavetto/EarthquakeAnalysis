% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module1.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Clean the location string}
\usage{
eq_location_clean(raw_data)
}
\arguments{
\item{raw_data}{the data.frame to be cleaned}
}
\value{
a cleaned data frame with two fields for the country and the location
}
\description{
Split the location / country field in two fields : one for the country
and one for the location
}
\examples{
\dontrun{eq_location_clean(NOAAdata)}

}
