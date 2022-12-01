# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

####################################################################
#' Export dryad Model to Local File [DEPRECATED]
#'
#' Use \code{dryad_save()} to select and save as .RDS file the initial model.
#'
#' @inheritParams dryad_allocator
#' @return (Invisible) list with filename and summary. Class: \code{dryad_save}.
#' @export
dryad_save <- function(InputCollect,
                       OutputCollect,
                       dryad_object = NULL,
                       select_model = NULL,
                       quiet = FALSE) {
  check_dryad_name(dryad_object, quiet)
  if (is.null(select_model)) select_model <- OutputCollect[["selectID"]]
  if (!select_model %in% OutputCollect$allSolutions) {
    stop(paste0("Input 'select_model' must be one of these values: ", paste(
      OutputCollect$allSolutions,
      collapse = ", "
    )))
  }

  # Export as JSON file
  json <- dryad_write(InputCollect, OutputCollect, select_model)

  summary <- filter(OutputCollect$xDecompAgg, .data$solID == select_model) %>%
    select(
      variable = .data$rn, .data$coef, decomp = .data$xDecompPerc,
      .data$total_spend, mean_non0_spend = .data$mean_spend
    )

  # Nice and tidy table format for hyper-parameters
  hyps_name <- c("thetas", "shapes", "scales", "alphas", "gammas")
  regex <- paste(paste0("_", hyps_name), collapse = "|")
  hyps <- filter(OutputCollect$resultHypParam, .data$solID == select_model) %>%
    select(contains(hyps_name)) %>%
    tidyr::gather() %>%
    tidyr::separate(.data$key,
      into = c("channel", "none"),
      sep = regex, remove = FALSE
    ) %>%
    mutate(hyperparameter = gsub("^.*_", "", .data$key)) %>%
    select(.data$channel, .data$hyperparameter, .data$value) %>%
    tidyr::spread(key = "hyperparameter", value = "value")

  values <- OutputCollect[!unlist(lapply(OutputCollect, is.list))]
  values <- values[!names(values) %in% c("allSolutions", "hyper_fixed", "plot_folder")]

  output <- list(
    dryad_object = dryad_object,
    select_model = select_model,
    summary = summary,
    errors = json$ExportedModel$errors,
    hyper_df = hyps,
    hyper_values = json$ExportedModel$hyper_values,
    hyper_updated = OutputCollect$hyper_updated,
    window = c(InputCollect$window_start, InputCollect$window_end),
    periods = InputCollect$rollingWindowLength,
    interval = InputCollect$intervalType,
    adstock = InputCollect$adstock,
    plot = dryad_onepagers(InputCollect, OutputCollect,
      select_model,
      quiet = TRUE, export = FALSE
    )
  )
  output <- append(output, values)
  if (InputCollect$dep_var_type == "conversion") {
    colnames(output$summary) <- gsub("roi_", "cpa_", colnames(output$summary))
  }
  class(output) <- c("dryad_save", class(output))

  if (!is.null(dryad_object)) {
    if (file.exists(dryad_object)) {
      if (!quiet) {
        answer <- askYesNo(paste0(dryad_object, " already exists. Are you certain to overwrite it?"))
      } else {
        answer <- TRUE
      }
      if (answer == FALSE || is.na(answer)) {
        message("Stopped export to avoid overwriting")
        return(invisible(output))
      }
    }
  }

  OutputCollect$resultHypParam <- OutputCollect$resultHypParam[
    OutputCollect$resultHypParam$solID == select_model,
  ]
  OutputCollect$xDecompAgg <- OutputCollect$xDecompAgg[
    OutputCollect$resultHypParam$solID == select_model,
  ]
  OutputCollect$mediaVecCollect <- OutputCollect$mediaVecCollect[
    OutputCollect$resultHypParam$solID == select_model,
  ]
  OutputCollect$xDecompVecCollect <- OutputCollect$xDecompVecCollect[
    OutputCollect$resultHypParam$solID == select_model,
  ]
  OutputCollect$selectID <- select_model

  InputCollect$refreshCounter <- 0
  listInit <- list(InputCollect = InputCollect, OutputCollect = OutputCollect)
  dryad <- list(listInit = listInit)

  class(dryad) <- c("dryad_exported", class(dryad))
  if (!is.null(dryad_object)) {
    saveRDS(dryad, file = dryad_object)
    if (!quiet) message("Exported results: ", dryad_object)
  }
  return(invisible(output))
}

#' @rdname dryad_save
#' @aliases dryad_save
#' @param x \code{dryad_save()} output.
#' @export
print.dryad_save <- function(x, ...) {
  print(glued(
    "
  Exported file: {x$dryad_object}
  Exported model: {x$select_model}
  Window: {x$window[1]} to {x$window[2]} ({x$periods} {x$interval}s)"
  ))

  print(glued(
    "\n\nModel's Performance and Errors:\n    {errors}",
    errors = paste(
      "R2 (train):", signif(x$errors$rsq_train, 4),
      "| NRMSE =", signif(x$errors$nrmse, 4),
      "| DECOMP.RSSD =", signif(x$errors$decomp.rssd, 4),
      "| MAPE =", signif(x$errors$mape, 4)
    )
  ))

  print(glued("\n\nSummary Values on Selected Model:"))

  print(x$summary %>%
    mutate(decomp = formatNum(100 * .data$decomp, pos = "%")) %>%
    dplyr::mutate_if(is.numeric, function(x) formatNum(x, 4, abbr = TRUE)) %>%
    replace(., . == "NA", "-") %>% as.data.frame())

  print(glued(
    "\n\nHyper-parameters for channel transformations:\n    Adstock: {x$adstock}"
  ))

  print(as.data.frame(x$hyper_df))
}

#' @rdname dryad_save
#' @aliases dryad_save
#' @param x \code{dryad_save()} output.
#' @export
plot.dryad_save <- function(x, ...) plot(x$plot[[1]], ...)

#' @rdname dryad_save
#' @aliases dryad_save
#' @return (Invisible) list with imported results
#' @export
dryad_load <- function(dryad_object, select_build = NULL, quiet = FALSE) {
  if ("dryad_exported" %in% class(dryad_object) || is.list(dryad_object)) {
    dryad <- dryad_object
    objectPath <- dryad$listInit$OutputCollect$plot_folder
    dryad_object <- paste0(objectPath, "/dryadMMM_", dryad$listInit$OutputCollect$selectID, ".RDS")
    if (!dir.exists(objectPath)) {
      stop("Directory does not exist or is somewhere else. Check: ", objectPath)
    }
  } else {
    if (!"character" %in% class(dryad_object)) {
      stop("Input 'dryad_object' must be a character input or 'dryad_exported' object")
    }
    check_dryad_name(dryad_object, quiet)
    dryad <- readRDS(dryad_object)
    objectPath <- dirname(dryad_object)
  }
  select_build_all <- 0:(length(dryad) - 1)
  if (is.null(select_build)) {
    select_build <- max(select_build_all)
    if (!quiet) {
      message(
        ">>> Loaded Model: ",
        ifelse(select_build == 0, "Initial model", paste0("Refresh model #", select_build))
      )
    }
  }
  if (!(select_build %in% select_build_all) || length(select_build) != 1) {
    stop("Input 'select_build' must be one value of ", paste(select_build_all, collapse = ", "))
  }
  listName <- ifelse(select_build == 0, "listInit", paste0("listRefresh", select_build))
  InputCollect <- dryad[[listName]][["InputCollect"]]
  OutputCollect <- dryad[[listName]][["OutputCollect"]]
  select_model <- OutputCollect$selectID
  output <- list(
    dryad = dryad,
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    select_model = select_model,
    objectPath = objectPath,
    dryad_object = dryad_object
  )
  return(invisible(output))
}
