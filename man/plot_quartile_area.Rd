% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_quartile_area.R
\name{plot_quartile_area}
\alias{plot_quartile_area}
\title{Function to calculate quartile time series composition for Australian artists and produce a stacked area graph}
\usage{
plot_quartile_area(data = historical_countdowns)
}
\arguments{
\item{data}{The dataframe of Hottest 100 results to analyse}
}
\value{
an object of class ggplot which is a stacked area graph
}
\description{
Function to calculate quartile time series composition for Australian artists and produce a stacked area graph
}
\examples{
\dontrun{
plot_quartile_area(historical_countdowns)
}

}
\author{
Trent Henderson
}
