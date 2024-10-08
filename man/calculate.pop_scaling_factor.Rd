% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculateEMODMetrics.R
\name{calculate.pop_scaling_factor}
\alias{calculate.pop_scaling_factor}
\title{Calculate the scaling factor for a population run on EMOD. Typically when we run EMOD, we scale down our population for the sake of
computation costs. In order to get correct counts of different statuses (i.e., number of people who died from HIV), we need to scale our data.
This function will not scale the data, rather it will add a scaling-factor column to the data that you can multiply any column of interest by.}
\usage{
calculate.pop_scaling_factor(
  data,
  reference_year,
  reference_population,
  age_min_inclusive = 0,
  age_max_inclusive = 200
)
}
\arguments{
\item{data}{A tibble returned from read.simulation.results(..., stratify_columns = c("Year"), aggregate_columns = c("Population")).
More aggregate and stratify columns can be used}

\item{reference_year}{The year that you want to calibrate the total population to}

\item{reference_population}{the actual population of the area being studied at the time of "reference_year"}
}
\value{
A tibble with all original columns found in "data", plus a column for pop_scaling_factor
}
\description{
Calculate the scaling factor for a population run on EMOD. Typically when we run EMOD, we scale down our population for the sake of
computation costs. In order to get correct counts of different statuses (i.e., number of people who died from HIV), we need to scale our data.
This function will not scale the data, rather it will add a scaling-factor column to the data that you can multiply any column of interest by.
}
