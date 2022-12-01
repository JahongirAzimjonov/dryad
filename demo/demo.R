library(dryad)
library(readxl)
Sys.setenv(R_FUTURE_FORK_ENABLE = "true") ## Force multicore 
options(future.fork.enable = TRUE)
setwd("C:/Users/charl/Documents/dryad") # set WD

# data("dt_simulated_weekly")
# head(dt_simulated_weekly)

mydata <- read_excel("demo_data_Nov22.xlsx", sheet = "data")

data("dt_prophet_holidays") # holiday frame
head(dt_prophet_holidays)
dryad_object <- "dryad" # Directory where you want to export results to (will create new folders)

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
  paid_media_vars = c("tv_imp", "facebook_imp", "social_imp", "search_imp"), # mandatory.
  organic_vars = "newsletter", # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2015-11-30",
  window_end = "2019-10-21",
  adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
)
# print(InputCollect)

hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

hyperparameters <- list(
  tv_spend_alphas = c(0.5, 3),
  tv_spend_gammas = c(0.3, 1),
  tv_spend_thetas = c(0.3, 0.8),
  
  facebook_spend_alphas = c(0.5, 3),
  facebook_spend_gammas = c(0.3, 1),
  facebook_spend_thetas = c(0, 0.3),
  
  social_spend_alphas = c(0.5, 3),
  social_spend_gammas = c(0.3, 1),
  social_spend_thetas = c(0, 0.3),
  
  search_spend_alphas = c(0.5, 3),
  search_spend_gammas = c(0.3, 1),
  search_spend_thetas = c(0, 0.3),
  
  newsletter_alphas = c(0.5, 3),
  newsletter_gammas = c(0.3, 1),
  newsletter_thetas = c(0.1, 0.4)
)

InputCollect <- dryad_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
# print(InputCollect)

#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  InputCollect$modNLS$plots$facebook_spend
  InputCollect$modNLS$plots$search_spend
}

## Run all trials and iterations. Use ?dryad_run to check parameter definition
OutputModels <- dryad_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to max available - 1
  iterations = 500, # 2000 recommended for the dummy dataset with no calibration
  trials = 1, # 5 recommended for the dummy dataset
  add_penalty_factor = FALSE, # Experimental feature. Use with caution.
  outputs = FALSE # outputs = FALSE disables direct model output - dryad_outputs()
)
# print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
OutputModels$convergence$moo_cloud_plot
## Calculate Pareto optimality, cluster and export results and plots. 

OutputCollect <- dryad_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = FALSE, # Set to TRUE to cluster similar models by ROAS. 
  plot_pareto = FALSE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = dryad_object, # path for plots export
  export = TRUE # this will create files locally
)

# check convergence rules ?dryad_converge
OutputModels$convergence$moo_distrb_plot
# print(OutputCollect)

run_diagnostics(mydata)

 
