# remotes::install_github("hemicontinuous/dryad/R")
library(dryad)
packageVersion("dryad")
library(readxl)
Sys.setenv(R_FUTURE_FORK_ENABLE = "true") ## Force multicore 
options(future.fork.enable = TRUE)
setwd("C:/Users/charl/Documents/dryad") # set WD

mydata <- read_excel("demo_data_1.xlsx", sheet = "data")

data("dt_prophet_holidays") # holiday frame
head(dt_prophet_holidays)
dryad_object <- "~/dryad" # Directory where you want to export results to (will create new folders)

################################################################

InputCollect <- dryad_inputs(
  dt_input = mydata,
  dt_holidays = dt_prophet_holidays,
  date_var = "DATE", # date format must be "2020-01-01"
  dep_var = "revenue", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "holiday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "UK", # input one country. dt_prophet_holidays includes 59 countries by default
  context_vars = c("competitor_sales"), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("tv_spend", "facebook_spend", "social_spend", "search_spend"), # mandatory input
  paid_media_vars = c("tv_imp",   "facebook_imp",   "social_imp",   "search_imp"), # mandatory.
  organic_vars = "newsletter", # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2015-11-30",
  window_end = "2019-10-21",
  adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
)
# print(InputCollect)

hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

################################################################

plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

################################################################

## 4. Set individual hyperparameter bounds. They either contain two values e.g. c(0, 0.5),
# or only one value, in which case you'd "fix" that hyperparameter.

# Run hyper_limits() to check maximum upper and lower bounds by range
# Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
   tv_spend_alphas = c(0.5, 3)
  ,tv_spend_gammas = c(0.3, 1)
  ,tv_spend_thetas = c(0.3, 0.8)
  
  ,facebook_spend_alphas = c(0.5, 3)
  ,facebook_spend_gammas = c(0.3, 1)
  ,facebook_spend_thetas = c(0, 0.3)
  
  ,social_spend_alphas = c(0.5, 3)
  ,social_spend_gammas = c(0.3, 1)
  ,social_spend_thetas = c(0, 0.3)
  
  ,search_spend_alphas = c(0.5, 3)
  ,search_spend_gammas = c(0.3, 1)
  ,search_spend_thetas = c(0, 0.3)
  
  ,newsletter_alphas = c(0.5, 3)
  ,newsletter_gammas = c(0.3, 1)
  ,newsletter_thetas = c(0.1, 0.4)
  
### new feature, uncomment below line if using legacy version
  ,train_size = c(0.5, 0.8)
  
)

InputCollect <- dryad_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
# uncomment next line to print results
# print(InputCollect)

################################################################

#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}

################################################################
#### Build initial model

## Run all trials and iterations. Use ?dryad_run to check parameter definition
OutputModels <- dryad_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to max available - 1
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)

# uncomment next line to print results
# print(OutputModels)

## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?dryad_outputs
OutputCollect <- dryad_outputs(
  InputCollect, OutputModels,
  pareto_fronts = 3, # automatically pick how many pareto-fronts to fill min_candidates
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?dryad_clusters
  plot_pareto = TRUE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = dryad_object, # path for plots export
  export = TRUE # this will create files locally
)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?dryad_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

# uncomment next line to print results
# print(OutputCollect)

### new feature: DIAGNOSTICS
run_diagnostics(mydata)
