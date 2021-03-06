% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm_functions.R
\name{manual_learn}
\alias{manual_learn}
\title{Manual Learn}
\usage{
manual_learn(raw_data, CallPut, moneyness_breaks = c(0.899, 0.94, 0.97, 1,
  1.03, 1.06, 1.101), maturity_breaks = c(0, 42, 126, 252),
  export_plots = FALSE)
}
\arguments{
\item{raw_data}{The option data set given in the format of \link{uslfin_ds_1}.}

\item{CallPut}{It denotes whether to use the call or put options.}

\item{moneyness_breaks}{Vector of moneyness boundaries.}

\item{moneyness_breaks}{Vector of maturity boundaries.}
}
\description{
Given a set of moneyness and maturity boundaries and a option data set, this function calculates in-sample and out-of-sample clustering errors.
}

