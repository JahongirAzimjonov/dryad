# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

####################################################################
#' Budget Allocator
#'
#' \code{dryad_allocator()} function returns a new split of media
#' variable spends that maximizes the total media response.
#'
#' @inheritParams dryad_run
#' @inheritParams dryad_outputs
#' @param dryad_object Character or List. Path of the \code{dryad.RDS} object
#' that contains all previous modeling information or the imported list.
#' @param select_build Integer. Default to the latest model build. \code{select_build = 0}
#' selects the initial model. \code{select_build = 1} selects the first refresh model.
#' @param InputCollect List. Contains all input parameters for the model.
#' Required when \code{dryad_object} is not provided.
#' @param OutputCollect List. Containing all model result.
#' Required when \code{dryad_object} is not provided.
#' @param select_model Character. A model \code{SolID}. When \code{dryad_object}
#' is provided, \code{select_model} defaults to the already selected \code{SolID}. When
#' \code{dryad_object} is not provided, \code{select_model} must be provided with
#' \code{InputCollect} and \code{OutputCollect}, and must be one of
#' \code{OutputCollect$allSolutions}.
#' @param optim_algo Character. Default to \code{"SLSQP_AUGLAG"}, short for "Sequential Least-Squares
#' Quadratic Programming" and "Augmented Lagrangian". Alternatively, "\code{"MMA_AUGLAG"},
#' short for "Methods of Moving Asymptotes". More details see the documentation of
#' NLopt \href{https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/}{here}.
#' @param scenario Character. Accepted options are: \code{"max_historical_response"} or
#' \code{"max_response_expected_spend"}. \code{"max_historical_response"} simulates the scenario
#' "what's the optimal media spend allocation given the same average spend level in history?",
#' while \code{"max_response_expected_spend"} simulates the scenario "what's the optimal media
#' spend allocation of a given future spend level for a given period?"
#' @param expected_spend Numeric. The expected future spend volume. Only applies when
#' \code{scenario = "max_response_expected_spend"}.
#' @param expected_spend_days Integer. The duration of the future spend volume in
#' \code{expected_spend}. Only applies when \code{scenario = "max_response_expected_spend"}.
#' @param channel_constr_low,channel_constr_up Numeric vectors. The lower and upper bounds
#' for each paid media variable when maximizing total media response. For example,
#' \code{channel_constr_low = 0.7} means minimum spend of the variable is 70% of historical
#' average, using non-zero spend values, within \code{date_min} and \code{date_max} date range.
#' Both constrains must be length 1 (same for all values) OR same length and order as
#' \code{paid_media_spends}. It's not recommended to 'exaggerate' upper bounds, especially
#' if the new level is way higher than historical level. Lower bound must be >=0.01,
#' and upper bound should be < 5.
#' @param maxeval Integer. The maximum iteration of the global optimization algorithm.
#' Defaults to 100000.
#' @param constr_mode Character. Options are \code{"eq"} or \code{"ineq"},
#' indicating constraints with equality or inequality.
#' @param date_min,date_max Character/Date. Date range to calculate mean (of non-zero
#' spends) and total spends. Default will consider all dates within modeled window.
#' Length must be 1 for both parameters.
#' @return A list object containing allocator result.
#' @examples
#' \dontrun{
#' # Having InputCollect and OutputCollect results
#' # Set your exported model location
#' dryad_object <- "~/Desktop/mydryad.RDS"
#'
#' # Check media summary for selected model from the simulated data
#' select_model <- "3_10_3"
#' OutputCollect$xDecompAgg[
#'   solID == select_model & !is.na(mean_spend),
#'   .(rn, coef, mean_spend, mean_response, roi_mean,
#'     total_spend,
#'     total_response = xDecompAgg, roi_total, solID
#'   )
#' ]
#'
#' # Run allocator with 'InputCollect' and 'OutputCollect'
#' # with 'scenario = "max_historical_response"'
#' AllocatorCollect <- dryad_allocator(
#'   InputCollect = InputCollect,
#'   OutputCollect = OutputCollect,
#'   select_model = select_model,
#'   scenario = "max_historical_response",
#'   channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7),
#'   channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
#' )
#'
#' # Run allocator with a 'dryad_object' from the second model refresh
#' # with 'scenario = "max_response_expected_spend"'
#' AllocatorCollect <- dryad_allocator(
#'   dryad_object = dryad_object,
#'   select_build = 2,
#'   scenario = "max_response_expected_spend",
#'   channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7),
#'   channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
#'   expected_spend = 100000,
#'   expected_spend_days = 90
#' )
#' }
#' @return List. Contains optimized allocation results and plots.
#' @export
dryad_allocator <- function(dryad_object = NULL,
                            select_build = 0,
                            InputCollect = NULL,
                            OutputCollect = NULL,
                            select_model = NULL,
                            json_file = NULL,
                            optim_algo = "SLSQP_AUGLAG",
                            scenario = "max_historical_response",
                            expected_spend = NULL,
                            expected_spend_days = NULL,
                            channel_constr_low = 0.5,
                            channel_constr_up = 2,
                            maxeval = 100000,
                            constr_mode = "eq",
                            date_min = NULL,
                            date_max = NULL,
                            export = TRUE,
                            quiet = FALSE,
                            ui = FALSE,
                            ...) {
  #####################################
  #### Set local environment

  ### Use previously exported model using json_file
  if (!is.null(json_file)) {
    if (is.null(InputCollect)) InputCollect <- dryad_inputs(json_file = json_file, ...)
    if (is.null(OutputCollect)) {
      OutputCollect <- dryad_run(
        json_file = json_file, plot_folder = dryad_object, ...
      )
    }
    if (is.null(select_model)) select_model <- OutputCollect$selectID
  }

  ## Collect inputs
  if (!is.null(dryad_object) && (is.null(InputCollect) && is.null(OutputCollect))) {
    if ("dryad_exported" %in% class(dryad_object)) {
      imported <- dryad_object
      dryad_object <- imported$dryad_object
    } else {
      imported <- dryad_load(dryad_object, select_build, quiet)
    }
    InputCollect <- imported$InputCollect
    OutputCollect <- imported$OutputCollect
    select_model <- imported$select_model
  } else if (any(is.null(InputCollect), is.null(OutputCollect), is.null(select_model))) {
    stop("When 'dryad_object' is not provided, then InputCollect, OutputCollect, select_model must be provided")
  }

  message(paste(">>> Running budget allocator for model ID", select_model, "..."))

  ## Set local data & params values
  if (TRUE) {
    dt_mod <- InputCollect$dt_mod
    paid_media_vars <- InputCollect$paid_media_vars
    paid_media_spends <- InputCollect$paid_media_spends
    startRW <- InputCollect$rollingWindowStartWhich
    endRW <- InputCollect$rollingWindowEndWhich
    adstock <- InputCollect$adstock
    media_order <- order(paid_media_spends)
    mediaVarSorted <- paid_media_vars[media_order]
    mediaSpendSorted <- paid_media_spends[media_order]
  }

  ## Check inputs and parameters
  check_allocator(
    OutputCollect, select_model, paid_media_spends, scenario,
    channel_constr_low, channel_constr_up,
    expected_spend, expected_spend_days, constr_mode
  )

  # Channels contrains
  # channel_constr_low <- rep(0.8, length(paid_media_spends))
  # channel_constr_up <- rep(1.2, length(paid_media_spends))
  if (length(channel_constr_low) == 1) {
    channel_constr_low <- rep(channel_constr_low, length(paid_media_spends))
  }
  if (length(channel_constr_up) == 1) {
    channel_constr_up <- rep(channel_constr_up, length(paid_media_spends))
  }
  names(channel_constr_low) <- paid_media_spends
  names(channel_constr_up) <- paid_media_spends
  channel_constr_low <- channel_constr_low[media_order]
  channel_constr_up <- channel_constr_up[media_order]

  # Hyper-parameters and results
  dt_hyppar <- filter(OutputCollect$resultHypParam, .data$solID == select_model)
  dt_bestCoef <- filter(OutputCollect$xDecompAgg, .data$solID == select_model, .data$rn %in% paid_media_spends)

  ## Sort table and get filter for channels mmm coef reduced to 0
  dt_coef <- select(dt_bestCoef, .data$rn, .data$coef)
  get_rn_order <- order(dt_bestCoef$rn)
  dt_coefSorted <- dt_coef[get_rn_order, ]
  dt_bestCoef <- dt_bestCoef[get_rn_order, ]
  coefSelectorSorted <- dt_coefSorted$coef > 0
  names(coefSelectorSorted) <- dt_coefSorted$rn

  ## Filter and sort all variables by name that is essential for the apply function later
  if (!all(coefSelectorSorted)) {
    chn_coef0 <- setdiff(names(coefSelectorSorted), mediaSpendSorted[coefSelectorSorted])
    message("Excluded in optimiser because their coefficients are 0: ", paste(chn_coef0, collapse = ", "))
  } else {
    chn_coef0 <- "None"
  }
  mediaSpendSortedFiltered <- mediaSpendSorted[coefSelectorSorted]
  dt_hyppar <- select(dt_hyppar, hyper_names(adstock, mediaSpendSortedFiltered)) %>%
    select(sort(colnames(.)))
  dt_bestCoef <- dt_bestCoef[dt_bestCoef$rn %in% mediaSpendSortedFiltered, ]
  channelConstrLowSorted <- channel_constr_low[mediaSpendSortedFiltered]
  channelConstrUpSorted <- channel_constr_up[mediaSpendSortedFiltered]

  ## Get adstock parameters for each channel
  getAdstockHypPar <- get_adstock_params(InputCollect, dt_hyppar)

  ## Get hill parameters for each channel
  hills <- get_hill_params(
    InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model
  )
  alphas <- hills$alphas
  gammaTrans <- hills$gammaTrans
  coefsFiltered <- hills$coefsFiltered

  # Spend values based on date range set
  dt_optimCost <- slice(dt_mod, startRW:endRW)
  check_daterange(date_min, date_max, dt_optimCost$ds)
  if (is.null(date_min)) date_min <- min(dt_optimCost$ds)
  if (is.null(date_max)) date_max <- max(dt_optimCost$ds)
  if (date_min < min(dt_optimCost$ds)) date_min <- min(dt_optimCost$ds)
  if (date_max > max(dt_optimCost$ds)) date_max <- max(dt_optimCost$ds)
  histFiltered <- filter(dt_optimCost, .data$ds >= date_min & .data$ds <= date_max)
  nPeriod <- nrow(histFiltered)
  message(sprintf("Date Window: %s:%s (%s %ss)", date_min, date_max, nPeriod, InputCollect$intervalType))

  histSpendB <- select(histFiltered, any_of(mediaSpendSortedFiltered))
  histSpendTotal <- sum(histSpendB)
  histSpend <- unlist(summarise_all(select(histFiltered, any_of(mediaSpendSortedFiltered)), sum))
  histSpendUnit <- unlist(summarise_all(histSpendB, function(x) sum(x) / sum(x > 0)))
  histSpendUnit[is.nan(histSpendUnit)] <- 0
  histSpendUnitTotal <- sum(histSpendUnit, na.rm = TRUE)
  histSpendShare <- histSpendUnit / histSpendUnitTotal

  # Response values based on date range -> mean spend
  noSpendMedia <- histResponseUnitModel <- NULL
  for (i in seq_along(mediaSpendSortedFiltered)) {
    if (histSpendUnit[i] > 0) {
      val <- dryad_response(
        json_file = json_file,
        dryad_object = dryad_object,
        select_build = select_build,
        media_metric = mediaSpendSortedFiltered[i],
        select_model = select_model,
        metric_value = histSpendUnit[i],
        dt_hyppar = OutputCollect$resultHypParam,
        dt_coef = OutputCollect$xDecompAgg,
        InputCollect = InputCollect,
        OutputCollect = OutputCollect,
        quiet = quiet
      )$response
    } else {
      val <- 0
      noSpendMedia <- c(noSpendMedia, mediaSpendSortedFiltered[i])
    }
    histResponseUnitModel <- c(histResponseUnitModel, val)
  }
  names(histResponseUnitModel) <- mediaSpendSortedFiltered
  if (!is.null(noSpendMedia) && !quiet) {
    message("Media variables with 0 spending during this date window: ", v2t(noSpendMedia))
  }

  ## Build constraints function with scenarios
  if ("max_historical_response" %in% scenario) {
    expected_spend <- histSpendTotal
    expSpendUnitTotal <- histSpendUnitTotal
  } else {
    expSpendUnitTotal <- expected_spend / (expected_spend_days / InputCollect$dayInterval)
  }

  # Gather all values that will be used internally on optim (nloptr)
  eval_list <- list(
    coefsFiltered = coefsFiltered,
    alphas = alphas,
    gammaTrans = gammaTrans,
    mediaSpendSortedFiltered = mediaSpendSortedFiltered,
    expSpendUnitTotal = expSpendUnitTotal
  )
  # So we can implicitly use these values within eval_f()
  options("DRYAD_TEMP" = eval_list)

  # eval_f(c(1,1))
  # $objective
  # [1] -0.02318446
  # $gradient
  # [1] -1.923670e-06 -8.148831e-06 -3.163465e-02 -3.553371e-05
  # $objective.channel
  # [1] -6.590166e-07 -3.087475e-06 -2.316821e-02 -1.250144e-05

  ## Set initial values and bounds
  x0 <- lb <- histSpendUnit * channelConstrLowSorted
  ub <- histSpendUnit * channelConstrUpSorted

  ## Set optim options
  if (optim_algo == "MMA_AUGLAG") {
    local_opts <- list(
      "algorithm" = "NLOPT_LD_MMA",
      "xtol_rel" = 1.0e-10
    )
  } else if (optim_algo == "SLSQP_AUGLAG") {
    local_opts <- list(
      "algorithm" = "NLOPT_LD_SLSQP",
      "xtol_rel" = 1.0e-10
    )
  }

  ## Run optim
  nlsMod <- nloptr::nloptr(
    x0 = x0,
    eval_f = eval_f,
    eval_g_eq = if (constr_mode == "eq") eval_g_eq else NULL,
    eval_g_ineq = if (constr_mode == "ineq") eval_g_ineq else NULL,
    lb = lb, ub = ub,
    opts = list(
      "algorithm" = "NLOPT_LD_AUGLAG",
      "xtol_rel" = 1.0e-10,
      "maxeval" = maxeval,
      "local_opts" = local_opts
    )
  )

  ## Collect output
  dt_optimOut <- data.frame(
    solID = select_model,
    dep_var_type = InputCollect$dep_var_type,
    channels = mediaSpendSortedFiltered,
    date_min = date_min,
    date_max = date_max,
    periods = sprintf("%s %ss", nPeriod, InputCollect$intervalType),
    constr_low = channelConstrLowSorted,
    constr_up = channelConstrUpSorted,
    # Initial
    histSpend = histSpend,
    histSpendTotal = histSpendTotal,
    initSpendUnitTotal = histSpendUnitTotal,
    initSpendUnit = histSpendUnit,
    initSpendShare = histSpendShare,
    initResponseUnit = histResponseUnitModel,
    initResponseUnitTotal = sum(histResponseUnitModel),
    initRoiUnit = histResponseUnitModel / histSpendUnit,
    # Expected
    expSpendTotal = expected_spend,
    expSpendUnitTotal = expSpendUnitTotal,
    expSpendUnitDelta = expSpendUnitTotal / histSpendUnitTotal - 1,
    # Optimized
    optmSpendUnit = nlsMod$solution,
    optmSpendUnitDelta = (nlsMod$solution / histSpendUnit - 1),
    optmSpendUnitTotal = sum(nlsMod$solution),
    optmSpendUnitTotalDelta = sum(nlsMod$solution) / histSpendUnitTotal - 1,
    optmSpendShareUnit = nlsMod$solution / sum(nlsMod$solution),
    optmResponseUnit = -eval_f(nlsMod$solution)[["objective.channel"]],
    optmResponseUnitTotal = sum(-eval_f(nlsMod$solution)[["objective.channel"]]),
    optmRoiUnit = -eval_f(nlsMod$solution)[["objective.channel"]] / nlsMod$solution,
    optmResponseUnitLift = (-eval_f(nlsMod$solution)[["objective.channel"]] / histResponseUnitModel) - 1
  ) %>%
    mutate(optmResponseUnitTotalLift = (.data$optmResponseUnitTotal / .data$initResponseUnitTotal) - 1)
  .Options$DRYAD_TEMP <- NULL # Clean auxiliary method

  ## Plot allocator results
  plots <- allocation_plots(InputCollect, OutputCollect, dt_optimOut, select_model, scenario, export, quiet)

  ## Export results into CSV
  if (export) {
    export_dt_optimOut <- dt_optimOut
    if (InputCollect$dep_var_type == "conversion") {
      colnames(export_dt_optimOut) <- gsub("Roi", "CPA", colnames(export_dt_optimOut))
    }
    write.csv(export_dt_optimOut, paste0(OutputCollect$plot_folder, select_model, "_reallocated.csv"))
  }

  output <- list(
    dt_optimOut = dt_optimOut,
    nlsMod = nlsMod,
    plots = plots,
    scenario = scenario,
    expected_spend = expected_spend,
    expected_spend_days = expected_spend_days,
    skipped = chn_coef0,
    no_spend = noSpendMedia,
    ui = if (ui) plots else NULL
  )

  class(output) <- c("dryad_allocator", class(output))
  return(output)
}

#' @rdname dryad_allocator
#' @aliases dryad_allocator
#' @param x \code{dryad_allocator()} output.
#' @export
print.dryad_allocator <- function(x, ...) {
  temp <- x$dt_optimOut[!is.nan(x$dt_optimOut$optmRoiUnit), ]
  print(glued(
    "
Model ID: {x$dt_optimOut$solID[1]}
Scenario: {scenario}
Dep. Variable Type: {temp$dep_var_type[1]}
Media Skipped (coef = 0): {paste0(x$skipped, collapse = ',')} {no_spend}
Relative Spend Increase: {spend_increase_p}% ({spend_increase}{scenario_plus})
Total Response Increase (Optimized): {signif(100 * x$dt_optimOut$optmResponseUnitTotalLift[1], 3)}%
Window: {x$dt_optimOut$date_min[1]}:{x$dt_optimOut$date_max[1]} ({x$dt_optimOut$periods[1]})

Allocation Summary:
  {summary}
",
    scenario = ifelse(
      x$scenario == "max_historical_response",
      "Maximum Historical Response",
      "Maximum Response with Expected Spend"
    ),
    no_spend = ifelse(!is.null(x$no_spend), paste("| (spend = 0):", v2t(x$no_spend, quotes = FALSE)), ""),
    spend_increase_p = signif(100 * x$dt_optimOut$expSpendUnitDelta[1], 3),
    spend_increase = formatNum(
      sum(x$dt_optimOut$optmSpendUnitTotal) - sum(x$dt_optimOut$initSpendUnitTotal),
      abbr = TRUE, sign = TRUE
    ),
    scenario_plus = ifelse(
      x$scenario == "max_response_expected_spend",
      sprintf(" in %s days", x$expected_spend_days), ""
    ),
    summary = paste(sprintf(
      "
- %s:
  Optimizable Range (bounds): [%s%%, %s%%]
  Mean Spend Share (avg): %s%% -> Optimized = %s%%
  Mean Response: %s -> Optimized = %s
  Mean Spend (per time unit): %s -> Optimized = %s [Delta = %s%%]",
      temp$channels,
      100 * temp$constr_low - 100,
      100 * temp$constr_up - 100,
      signif(100 * temp$initSpendShare, 3),
      signif(100 * temp$optmSpendShareUnit, 3),
      formatNum(temp$initResponseUnit, 0),
      formatNum(temp$optmResponseUnit, 0),
      formatNum(temp$initSpendUnit, 3, abbr = TRUE),
      formatNum(temp$optmSpendUnit, 3, abbr = TRUE),
      formatNum(100 * temp$optmSpendUnitDelta, signif = 2)
    ), collapse = "\n  ")
  ))
}

#' @rdname dryad_allocator
#' @aliases dryad_allocator
#' @param x \code{dryad_allocator()} output.
#' @export
plot.dryad_allocator <- function(x, ...) plot(x$plots$plots, ...)

eval_f <- function(X) {
  # eval_list <- get("eval_list", pos = as.environment(-1))
  eval_list <- getOption("DRYAD_TEMP")
  # mm_lm_coefs <- eval_list[["mm_lm_coefs"]]
  coefsFiltered <- eval_list[["coefsFiltered"]]
  alphas <- eval_list[["alphas"]]
  gammaTrans <- eval_list[["gammaTrans"]]
  mediaSpendSortedFiltered <- eval_list[["mediaSpendSortedFiltered"]]
  # exposure_selectorSortedFiltered <- eval_list[["exposure_selectorSortedFiltered"]]
  # vmaxVec <- eval_list[["vmaxVec"]]
  # kmVec <- eval_list[["kmVec"]]

  fx_objective <- function(x, coeff, alpha, gammaTran
                           # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }

    # Adstock scales
    xAdstocked <- x
    # Hill transformation
    xOut <- coeff * sum((1 + gammaTran**alpha / xAdstocked**alpha)**-1)
    xOut
    return(xOut)
  }

  objective <- -sum(mapply(
    fx_objective,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  ))

  # https://www.derivative-calculator.net/ on the objective function 1/(1+gamma^alpha / x^alpha)
  fx_gradient <- function(x, coeff, alpha, gammaTran
                          # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }

    # Adstock scales
    xAdstocked <- x
    xOut <- -coeff * sum((alpha * (gammaTran**alpha) * (xAdstocked**(alpha - 1))) / (xAdstocked**alpha + gammaTran**alpha)**2)
    return(xOut)
  }

  gradient <- c(mapply(
    fx_gradient,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  ))

  fx_objective.chanel <- function(x, coeff, alpha, gammaTran
                                  # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }

    # Adstock scales
    xAdstocked <- x
    xOut <- -coeff * sum((1 + gammaTran**alpha / xAdstocked**alpha)**-1)
    return(xOut)
  }

  objective.channel <- mapply(
    fx_objective.chanel,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  )

  optm <- list(objective = objective, gradient = gradient, objective.channel = objective.channel)
  return(optm)
}

eval_g_eq <- function(X) {
  eval_list <- getOption("DRYAD_TEMP")
  constr <- sum(X) - eval_list$expSpendUnitTotal
  grad <- rep(1, length(X))
  return(list(
    "constraints" = constr,
    "jacobian" = grad
  ))
}

eval_g_ineq <- function(X) {
  eval_list <- getOption("DRYAD_TEMP")
  constr <- sum(X) - eval_list$expSpendUnitTotal
  grad <- rep(1, length(X))
  return(list(
    "constraints" = constr,
    "jacobian" = grad
  ))
}

get_adstock_params <- function(InputCollect, dt_hyppar) {
  if (InputCollect$adstock == "geometric") {
    getAdstockHypPar <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_thetas"))))
  } else if (InputCollect$adstock %in% c("weibull_cdf", "weibull_pdf")) {
    getAdstockHypPar <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_shapes|.*_scales"))))
  }
  return(getAdstockHypPar)
}

get_hill_params <- function(InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model) {
  hillHypParVec <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_alphas|.*_gammas"))))
  alphas <- hillHypParVec[str_which(names(hillHypParVec), "_alphas")]
  gammas <- hillHypParVec[str_which(names(hillHypParVec), "_gammas")]
  startRW <- InputCollect$rollingWindowStartWhich
  endRW <- InputCollect$rollingWindowEndWhich
  chnAdstocked <- filter(
    OutputCollect$mediaVecCollect,
    .data$type == "adstockedMedia",
    .data$solID == select_model
  ) %>%
    select(all_of(mediaSpendSortedFiltered)) %>%
    slice(startRW:endRW)
  gammaTrans <- mapply(function(gamma, x) {
    round(quantile(seq(range(x)[1], range(x)[2], length.out = 100), gamma), 4)
  }, gamma = gammas, x = chnAdstocked)
  names(gammaTrans) <- names(gammas)
  coefs <- dt_coef$coef
  names(coefs) <- dt_coef$rn
  coefsFiltered <- coefs[mediaSpendSortedFiltered]
  return(list(
    alphas = alphas,
    gammaTrans = gammaTrans,
    coefsFiltered = coefsFiltered
  ))
}
