# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

####################################################################
#' Import and Export dryad JSON files
#'
#' \code{dryad_write()} generates a JSON file with all the information
#' required to replicate a single dryad model.
#'
#' @inheritParams dryad_outputs
#' @param InputCollect \code{dryad_inputs()} output.
#' @param select_model Character. Which model ID do you want to export
#' into the JSON file?
#' @param dir Character. Existing directory to export JSON file to.
#' @param ... Additional parameters.
#' @examples
#' \dontrun{
#' InputCollectJSON <- dryad_inputs(
#'   dt_input = dryad::dt_simulated_weekly,
#'   dt_holidays = dryad::dt_prophet_holidays,
#'   json_file = "~/Desktop/dryadModel-1_29_12.json"
#' )
#' print(InputCollectJSON)
#' }
#' @return (invisible) List. Contains all inputs and outputs of exported model.
#' Class: \code{dryad_write}.
#' @export
dryad_write <- function(InputCollect,
                        OutputCollect = NULL,
                        select_model = NULL,
                        dir = OutputCollect$plot_folder,
                        export = TRUE,
                        quiet = FALSE, ...) {
  # Checks
  stopifnot(inherits(InputCollect, "dryad_inputs"))
  if (!is.null(OutputCollect)) {
    stopifnot(inherits(OutputCollect, "dryad_outputs"))
    stopifnot(select_model %in% OutputCollect$allSolutions)
    if (is.null(select_model) && length(OutputCollect$allSolutions == 1)) {
      select_model <- OutputCollect$allSolutions
    }
  }
  if (is.null(dir)) dir <- getwd()

  # InputCollect JSON
  ret <- list()
  skip <- which(unlist(lapply(InputCollect, function(x) is.list(x) | is.null(x))))
  skip <- skip[!names(skip) %in% c("calibration_input", "hyperparameters", "custom_params")]
  ret[["InputCollect"]] <- inputs <- InputCollect[-skip]
  # toJSON(inputs, pretty = TRUE)

  # ExportedModel JSON
  if (!is.null(OutputCollect)) {
    outputs <- list()
    outputs$select_model <- select_model
    outputs$ts_validation <- OutputCollect$OutputModels$ts_validation
    outputs$summary <- filter(OutputCollect$xDecompAgg, .data$solID == select_model) %>%
      mutate(
        metric = ifelse(InputCollect$dep_var_type == "revenue", "ROI", "CPA"),
        performance = ifelse(.data$metric == "ROI", .data$roi_total, .data$cpa_total)
      ) %>%
      select(
        variable = .data$rn, coef = .data$coef,
        decompPer = .data$xDecompPerc, decompAgg = .data$xDecompAggRF,
        .data$performance, .data$mean_response, .data$mean_spend,
        contains("boot_mean"), contains("ci_")
      )
    outputs$errors <- filter(OutputCollect$resultHypParam, .data$solID == select_model) %>%
      select(starts_with("rsq_"), starts_with("nrmse"), .data$decomp.rssd, .data$mape)
    outputs$hyper_values <- OutputCollect$resultHypParam %>%
      filter(.data$solID == select_model) %>%
      select(contains(hyps_name), dplyr::ends_with("_penalty"), any_of(other_hyps)) %>%
      select(order(colnames(.))) %>%
      as.list()
    outputs$hyper_updated <- OutputCollect$hyper_updated
    skip <- which(unlist(lapply(OutputCollect, function(x) is.list(x) | is.null(x))))
    skip <- c(skip, which(names(OutputCollect) %in% "allSolutions"))
    outputs <- append(outputs, OutputCollect[-skip])
    ret[["ExportedModel"]] <- outputs
    # toJSON(outputs, pretty = TRUE)
  } else {
    select_model <- "inputs"
  }

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  filename <- sprintf("%s/dryadModel-%s.json", dir, select_model)
  filename <- gsub("//", "/", filename)
  class(ret) <- c("dryad_write", class(ret))
  attr(ret, "json_file") <- filename
  if (export) {
    if (!quiet) message(sprintf(">> Exported model %s as %s", select_model, filename))
    write_json(ret, filename, pretty = TRUE)
  }
  return(invisible(ret))
}


#' @rdname dryad_write
#' @aliases dryad_write
#' @param x \code{dryad_read()} or \code{dryad_write()} output.
#' @export
print.dryad_write <- function(x, ...) {
  val <- isTRUE(x$ExportedModel$ts_validation)
  print(glued(
    "
   Exported directory: {x$ExportedModel$plot_folder}
   Exported model: {x$ExportedModel$select_model}
   Window: {start} to {end} ({periods} {type}s)
   Time Series Validation: {val} (train size = {val_detail})",
    start = x$InputCollect$window_start,
    end = x$InputCollect$window_end,
    periods = x$InputCollect$rollingWindowLength,
    type = x$InputCollect$intervalType,
    val_detail = formatNum(100 * x$ExportedModel$hyper_values$train_size, 2, pos = "%")
  ))
  errors <- x$ExportedModel$errors
  print(glued(
    "\n\nModel's Performance and Errors:\n    {errors}",
    errors = paste(
      sprintf(
        "Adj.R2 (%s): %s",
        ifelse(!val, "train", "test"),
        ifelse(!val, signif(errors$rsq_train, 4), signif(errors$rsq_test, 4))
      ),
      "| NRMSE =", signif(errors$nrmse, 4),
      "| DECOMP.RSSD =", signif(errors$decomp.rssd, 4),
      "| MAPE =", signif(errors$mape, 4)
    )
  ))

  print(glued("\n\nSummary Values on Selected Model:"))

  print(x$ExportedModel$summary %>%
    select(-contains("boot"), -contains("ci_")) %>%
    dplyr::rename_at("performance", list(~ ifelse(InputCollect$dep_var_type == "revenue", "ROI", "CPA"))) %>%
    mutate(decompPer = formatNum(100 * .data$decompPer, pos = "%")) %>%
    dplyr::mutate_if(is.numeric, function(x) formatNum(x, 4, abbr = TRUE)) %>%
    replace(., . == "NA", "-") %>% as.data.frame())

  print(glued(
    "\n\nHyper-parameters:\n    Adstock: {x$InputCollect$adstock}"
  ))

  # Nice and tidy table format for hyper-parameters
  hyps_name <- c(hyps_name, "penalty")
  regex <- paste(paste0("_", hyps_name), collapse = "|")
  hyper_df <- as.data.frame(x$ExportedModel$hyper_values) %>%
    select(-contains("lambda"), -any_of(other_hyps)) %>%
    tidyr::gather() %>%
    tidyr::separate(.data$key,
      into = c("channel", "none"),
      sep = regex, remove = FALSE
    ) %>%
    mutate(hyperparameter = gsub("^.*_", "", .data$key)) %>%
    select(.data$channel, .data$hyperparameter, .data$value) %>%
    tidyr::spread(key = "hyperparameter", value = "value")
  print(hyper_df)
}


#' @rdname dryad_write
#' @aliases dryad_write
#' @param json_file Character. JSON file name to read and import as list.
#' @param step Integer. 1 for import only and 2 for import and ouput.
#' @export
dryad_read <- function(json_file = NULL, step = 1, quiet = FALSE, ...) {
  if (!is.null(json_file)) {
    if (inherits(json_file, "character")) {
      if (lares::right(tolower(json_file), 4) != "json") {
        stop("JSON file must be a valid .json file")
      }
      if (!file.exists(json_file)) {
        stop("JSON file can't be imported: ", json_file)
      }
      json <- read_json(json_file, simplifyVector = TRUE)
      json$InputCollect <- json$InputCollect[lapply(json$InputCollect, length) > 0]
      if (!"InputCollect" %in% names(json) && step == 1) {
        stop("JSON file must contain InputCollect element")
      }
      if (!"ExportedModel" %in% names(json) && step == 2) {
        stop("JSON file must contain ExportedModel element")
      }
      if (!quiet) message("Imported JSON file succesfully: ", json_file)
      class(json) <- c("dryad_read", class(json))
      return(json)
    }
  }
  return(json_file)
}

#' @rdname dryad_write
#' @aliases dryad_write
#' @export
print.dryad_read <- function(x, ...) {
  a <- x$InputCollect
  print(glued(
    "
############ InputCollect ############

Date: {a$date_var}
Dependent: {a$dep_var} [{a$dep_var_type}]
Paid Media: {paste(a$paid_media_vars, collapse = ', ')}
Paid Media Spend: {paste(a$paid_media_spends, collapse = ', ')}
Context: {paste(a$context_vars, collapse = ', ')}
Organic: {paste(a$organic_vars, collapse = ', ')}
Prophet (Auto-generated): {prophet}
Unused variables: {unused}
Model Window: {windows} ({a$rollingWindowEndWhich - a$rollingWindowStartWhich + 1} {a$intervalType}s)
With Calibration: {!is.null(a$calibration_input)}
Custom parameters: {custom_params}

Adstock: {a$adstock}
{hyps}
",
    windows = paste(a$window_start, a$window_end, sep = ":"),
    custom_params = if (length(a$custom_params) > 0) paste("\n", flatten_hyps(a$custom_params)) else "None",
    prophet = if (!is.null(a$prophet_vars)) {
      sprintf("%s on %s", paste(a$prophet_vars, collapse = ", "), a$prophet_country)
    } else {
      "\033[0;31mDeactivated\033[0m"
    },
    unused = if (length(a$unused_vars) > 0) {
      paste(a$unused_vars, collapse = ", ")
    } else {
      "None"
    },
    hyps = glued(
      "Hyper-parameters ranges:\n{flatten_hyps(a$hyperparameters)}"
    )
  ))

  if (!is.null(x$ExportedModel)) {
    temp <- x
    class(temp) <- "dryad_write"
    print(glued("\n\n############ Exported Model ############\n"))
    print(temp)
  }
  return(invisible(x))
}

#' @rdname dryad_write
#' @aliases dryad_write
#' @export
dryad_recreate <- function(json_file, quiet = FALSE, ...) {
  json <- dryad_read(json_file, quiet = TRUE)
  message(">>> Recreating model ", json$ExportedModel$select_model)
  InputCollect <- dryad_inputs(
    json_file = json_file,
    quiet = quiet,
    ...
  )
  OutputCollect <- dryad_run(
    InputCollect = InputCollect,
    json_file = json_file,
    export = FALSE,
    quiet = quiet,
    ...
  )
  return(invisible(list(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect
  )))
}

# Import the whole chain any refresh model to init
dryad_chain <- function(json_file) {
  json_data <- dryad_read(json_file, quiet = TRUE)
  ids <- c(json_data$InputCollect$refreshChain, json_data$ExportedModel$select_model)
  plot_folder <- json_data$ExportedModel$plot_folder
  temp <- stringr::str_split(plot_folder, "/")[[1]]
  chain <- temp[startsWith(temp, "dryad_")]
  if (length(chain) == 0) chain <- tail(temp[temp != ""], 1)
  base_dir <- gsub(sprintf("\\/%s.*", chain[1]), "", plot_folder)
  chainData <- list()
  for (i in rev(seq_along(chain))) {
    if (i == length(chain)) {
      json_new <- json_data
    } else {
      file <- paste0("dryadModel-", json_new$InputCollect$refreshSourceID, ".json")
      filename <- paste(c(base_dir, chain[1:i], file), collapse = "/")
      json_new <- dryad_read(filename, quiet = TRUE)
    }
    chainData[[json_new$ExportedModel$select_model]] <- json_new
  }
  chainData <- chainData[rev(seq_along(chain))]
  dirs <- unlist(lapply(chainData, function(x) x$ExportedModel$plot_folder))
  json_files <- paste0(dirs, "dryadModel-", names(dirs), ".json")
  attr(chainData, "json_files") <- json_files
  attr(chainData, "chain") <- ids # names(chainData)
  if (length(ids) != length(names(chainData))) {
    warning("Can't replicate chain-like results if you don't follow dryad's chain structure")
  }
  return(invisible(chainData))
}
