# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

####################################################################
#' dryad Dataset: Time series
#'
#' Describe the dataset. Input time series should be daily, weekly or monthly.
#'
#' @family Dataset
#' @docType data
#' @usage data(dt_simulated_weekly)
#' @return data.frame
#' @format An object of class \code{"data.frame"}
#' \describe{
#'   \item{DATE}{Date}
#'   \item{revenue}{Daily total revenue}
#'   \item{tv_S}{Television}
#'   \item{ooh_S}{Out of home}
#'   \item{...}{...}
#' }
#' @examples
#' data(dt_simulated_weekly)
#' head(dt_simulated_weekly)
#' @return Dataframe. Contains simulated dummy dataset to test and run demo.
"dt_simulated_weekly"

# dt_input <- read.csv('data/de_simulated_data.csv')
# save(dt_input, file = "data/dt_input.RData", version = 2)
# dt_simulated_weekly <- as_tibble(dt_simulated_weekly)
# save(dt_simulated_weekly, file = "data/dt_simulated_weekly.RData", version = 2)

####################################################################
#' dryad Dataset: Time series
#'
#' Describe the dataset. When using own holidays, please keep the
#' header \code{c("ds", "holiday", "country", "year")}.
#'
#' @family Dataset
#' @docType data
#' @usage data(dt_prophet_holidays)
#' @return data.frame
#' @format An object of class \code{"data.frame"}
#' \describe{
#'   \item{ds}{Date}
#'   \item{holiday}{Daily total revenue}
#'   \item{country}{Television}
#'   \item{year}{Out of home}
#' }
#' @examples
#' data(dt_prophet_holidays)
#' head(dt_prophet_holidays)
#' @return Dataframe. Contains \code{prophet}'s default holidays by country.
"dt_prophet_holidays"

# dt_prophet_holidays <- as_tibble(dt_prophet_holidays)
# save(dt_prophet_holidays, file = "data/dt_prophet_holidays.RData", version = 2)
