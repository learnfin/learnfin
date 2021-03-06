% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learnfin_data_frame_explanations.R
\docType{data}
\name{learnfin_ds_6}
\alias{learnfin_ds_6}
\title{learnfin Data Set 6}
\format{An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 19613 rows and 12 columns.}
\usage{
learnfin_ds_6
}
\arguments{
\item{date}{Date of the contract (format: YYYY-MM-DD)}

\item{option_symbol}{Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.}

\item{type}{Option type (call or put).}

\item{underlying_price}{Price of the underlying at the time of the option.}

\item{strike_price}{Strike price.}

\item{moneyness}{Moneyness. Relative position of the spot price (S0) to the strike price (K).}

\item{maturity}{Maturity. Time in trading days to expiration.}

\item{market_price}{(Closing) Price of the contract determined by the market.}

\item{model_price}{Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.}

\item{t_or_p}{Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.}

\item{rf_rate}{Annualized risk free rate in percentage.}

\item{dividend_yield}{Annualized dividend yield in percentage.}
}
\description{
This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2011-03-15 to 2011-10-14 with the change point at 2011-09-15 (first date of the prediction data).
}
\keyword{datasets}

