% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculateEMODMetrics.R
\name{calculate.bounds.effective_count}
\alias{calculate.bounds.effective_count}
\title{Calculate the 95 percent confidence interval of an estimate given effective counts. This is used to
calculate confidence intervals from EMOD calibration data that comes from a Beta distribution}
\usage{
calculate.bounds.effective_count(mean_vectors, effective_counts)
}
\arguments{
\item{effective_counts}{A vector of the effective number of samples from which the measurement was made}

\item{mean_vector}{A vector of the estimated value of a measurement}
}
\value{
A tibble with columns lb and ub corresponding to upper and lower bounds (95% CI)
}
\description{
Calculate the 95 percent confidence interval of an estimate given effective counts. This is used to
calculate confidence intervals from EMOD calibration data that comes from a Beta distribution
}
