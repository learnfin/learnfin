#'@title learnfin Data Set 1
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2008-09-09 to 2009-04-08 with the change point at 2009-03-09 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_1"

#'@title learnfin Data Set 2
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2009-06-05 to 2010-01-04 with the change point at 2009-12-07 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_2"

#'@title learnfin Data Set 3
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2009-07-20 to 2010-02-19 with the change point at 2010-01-20 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_3"

#'@title learnfin Data Set 4
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2010-04-01 to 2010-10-29 with the change point at 2010-10-01 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_4"

#'@title learnfin Data Set 5
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2011-01-06 to 2011-08-05 with the change point at 2011-07-06 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_5"

#'@title learnfin Data Set 6
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2011-03-15 to 2011-10-14 with the change point at 2011-09-15 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_6"

#'@title learnfin Data Set 7
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2011-11-07 to 2012-06-05 with the change point at 2012-05-07 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_7"

#'@title learnfin Data Set 8
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2012-05-11 to 2012-12-10 with the change point at 2012-11-12 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_8"

#'@title learnfin Data Set 9
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2012-08-06 to 2013-03-01 with the change point at 2013-02-04 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_9"

#'@title learnfin Data Set 10
#'
#'@description
#'This data set consists of European option contracts of S&P 500. Data set is separated into Training and Prediction sets defined in the \code{t_or_p} column. This particular data set consists of contracts from 2013-02-19 to 2013-09-13 with the change point at 2013-08-16 (first date of the prediction data).
#'@param date Date of the contract (format: YYYY-MM-DD)
#'@param option_symbol Unique identifier of the option. First three letters (SPX) defines the symbol of the underlying asset. Next 6 digits denote the expiration date (e.g. 080920 - Sep 20, 2008). Next character defines whether the contract is a (C)all or a (P)ut option. Last digits include the strike price.
#'@param type Option type (call or put).
#'@param underlying_price Price of the underlying at the time of the option.
#'@param strike_price Strike price.
#'@param moneyness Moneyness. Relative position of the spot price (S0) to the strike price (K).
#'@param maturity Maturity. Time in trading days to expiration.
#'@param market_price (Closing) Price of the contract determined by the market.
#'@param model_price Price of the contract determined by the pricing model. By default all model prices are determined by a Black-Scholes model that assumes constant continuous risk-free and dividend rates and volatility of standard deviation of the log-returns with 2 years of lookback period.
#'@param t_or_p Whether the contract should be used in the Training set or Prediction set. Training set should be used to train the clusters (in-sample estimation) and Prediction set should be used to get out-of-sample estimates.
#'@param rf_rate Annualized risk free rate in percentage.
#'@param dividend_yield Annualized dividend yield in percentage.
#'
"learnfin_ds_10"
