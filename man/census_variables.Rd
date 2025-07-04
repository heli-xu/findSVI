% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/census_variables.R
\docType{data}
\name{census_variables}
\alias{census_variables}
\alias{census_variables_2010}
\alias{census_variables_2011}
\alias{census_variables_2012}
\alias{census_variables_2013}
\alias{census_variables_2014}
\alias{census_variables_2015}
\alias{census_variables_2016}
\alias{census_variables_2017}
\alias{census_variables_2018}
\alias{census_variables_2019}
\alias{census_variables_2020}
\alias{census_variables_2021}
\alias{census_variables_2022}
\alias{census_variables_exp_2010}
\alias{census_variables_exp_2011}
\alias{census_variables_exp_2012}
\alias{census_variables_exp_2013}
\alias{census_variables_exp_2014}
\alias{census_variables_exp_2015}
\alias{census_variables_exp_2016}
\alias{census_variables_exp_2017}
\alias{census_variables_exp_2018}
\alias{census_variables_exp_2019}
\alias{census_variables_exp_2020}
\alias{census_variables_exp_2021}
\alias{census_variables_exp_2022}
\title{List of census variables for SVI calculation}
\format{
\subsection{a list of census variables, grouped into sublists named t0-t5 or}{

t0-t4 (for 2012). t1-t4 represent the 4 themes the corresponding SVI
variables are categorized into:
\itemize{
\item Socioeconomic
\item Household Composition/Disability
\item Minority Status/Language
\item Housing Type/Transportation
}

t0 represents 3 census variables of total counts, and their corresponding
SVI variables are not categorized into any theme. t5 contains the census
variables for SVI "adjunct variables", which are included for reference but
not used in SVI calculation. For 2012, adjunct variables are not included,
as the variable listed in 2014 documentation was not in 2012 Census data,
and there's no adjunct variables in 2010 documentation.

Datasets starting with \code{census_variable_} contains variables corresponding
to \code{variable_e_ep_calculation_} series of tables, where "EP_" (percent)
variables are retrieved directly from Census when available; Datasets
starting with \code{census_variables_exp_} contains variables corresponding to
\code{variable_cal_exp_} tables, where denominators for "EP_" variables are
explicitly defined using census variables (adjunct variables are not modified).
}

An object of class \code{list} of length 5.

An object of class \code{list} of length 5.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 5.

An object of class \code{list} of length 5.

An object of class \code{list} of length 5.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.

An object of class \code{list} of length 6.
}
\source{
CDC/ATSDR SVI Documentation
https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
}
\usage{
census_variables_2010

census_variables_2011

census_variables_2012

census_variables_2013

census_variables_2014

census_variables_2015

census_variables_2016

census_variables_2017

census_variables_2018

census_variables_2019

census_variables_2020

census_variables_2021

census_variables_2022

census_variables_exp_2010

census_variables_exp_2011

census_variables_exp_2012

census_variables_exp_2013

census_variables_exp_2014

census_variables_exp_2015

census_variables_exp_2016

census_variables_exp_2017

census_variables_exp_2018

census_variables_exp_2019

census_variables_exp_2020

census_variables_exp_2021

census_variables_exp_2022
}
\description{
Each of these datasets contains a list of census variable names for a year
between 2012-2022.
}
\keyword{datasets}
