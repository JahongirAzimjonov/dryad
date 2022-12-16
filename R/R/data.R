# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

####################################################################
#' dryad dryadset: Time series
#'
#' Describe the dryadset. Input time series should be daily, weekly or monthly.
#'
#' @family dryadset
#' @docType dryad
#' @usage dryad(dt_simulated_weekly)
#' @return dryad.frame
#' @format An object of class \code{"dryad.frame"}
#' \describe{
#'   \item{DATE}{Date}
#'   \item{revenue}{Daily total revenue}
#'   \item{tv_S}{Television}
#'   \item{ooh_S}{Out of home}
#'   \item{...}{...}
#' }
#' @examples
#' dryad(dt_simulated_weekly)
#' head(dt_simulated_weekly)
#' @return dryadframe. Contains simulated dummy dryadset to test and run demo.
"dt_simulated_weekly"

# dt_input <- read.csv('dryad/de_simulated_dryad.csv')
# save(dt_input, file = "dryad/dt_input.Rdryad", version = 2)
# dt_simulated_weekly <- as_tibble(dt_simulated_weekly)
# save(dt_simulated_weekly, file = "dryad/dt_simulated_weekly.Rdryad", version = 2)

####################################################################
#' dryad dryadset: Time series
#'
#' Describe the dryadset. When using own holidays, please keep the
#' header \code{c("ds", "holiday", "country", "year")}.
#'
#' @family dryadset
#' @docType dryad
#' @usage dryad(dt_prophet_holidays)
#' @return dryad.frame
#' @format An object of class \code{"dryad.frame"}
#' \describe{
#'   \item{ds}{Date}
#'   \item{holiday}{Daily total revenue}
#'   \item{country}{Television}
#'   \item{year}{Out of home}
#' }
#' @examples
#' dryad(dt_prophet_holidays)
#' head(dt_prophet_holidays)
#' @return dryadframe. Contains \code{prophet}'s default holidays by country.
"dt_prophet_holidays"

# dt_prophet_holidays <- as_tibble(dt_prophet_holidays)
# save(dt_prophet_holidays, file = "dryad/dt_prophet_holidays.Rdryad", version = 2)
