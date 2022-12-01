devtools::load_all()

# debug dryad_input
args(dryad_inputs)
dt_input = dt_simulated_weekly
dt_holidays = dt_prophet_holidays
date_var = "DATE"
dep_var = "revenue"
dep_var_type = "revenue"
prophet_vars = c("trend", "season", "holiday")
prophet_signs = c("default","default", "default")
prophet_country = "DE"
context_vars = c("competitor_sales_B", "events")
context_signs = c("default", "default")
paid_media_vars = c("tv_S", "ooh_S", 	"print_S", "facebook_I", "search_clicks_P")
paid_media_signs = c("positive", "positive", "positive", "positive", "positive")
paid_media_spends = c("tv_S", "ooh_S",	"print_S", "facebook_S", "search_S")
organic_vars = "newsletter"
organic_signs = "positive"
factor_vars = "events"
cores = 8
window_start = "2016-11-23"
window_end = "2018-08-22"
adstock = "geometric"
intercept_sign = "non_negative"
nevergrad_algo = "TwoPointsDE"
iterations = 100
trials = 2
calibration_input = NULL
json_file = NULL
InputCollect = NULL
#hyperparameters = NULL

## debug dryad_run
args(dryad_run)
calibration_constraint = 0.1
lambda_control = 1
refresh = FALSE
dt_hyper_fixed = NULL
seed = 123
outputs = FALSE
quiet = FALSE
add_penalty_factor = FALSE
cores = 6
iterations = 500
trials = 1
intercept_sign = "non_negative"
nevergrad_algo = "TwoPointsDE"
json_file = NULL

## debug dryad_train
args(dryad_train)
#hyper_collect
add_penalty_factor = FALSE
dt_hyper_fixed = NULL
lambda_control = 1
refresh = FALSE
seed = 123
quiet = FALSE


## debug dryad_mmm
args(dryad_mmm)
#InputCollect
#hyper_collect
add_penalty_factor = FALSE
iterations = 500
lambda.n = 100
lambda_control = 1
refresh = FALSE
seed = 123L
quiet = FALSE

## debug model_decomp
coefs = mod_out$coefs
y_pred = mod_out$y_pred

## debug dryad_calibrate
# calibration_input = calibration_input
df_raw = dt_mod
#hypParamSam = hypParamSam
wind_start = rollingWindowStartWhich
wind_end = rollingWindowEndWhich
dayInterval = InputCollect$dayInterval
dt_modAdstocked = dt_modAdstocked
#adstock = adstock
xDecompVec = decompCollect$xDecompVec
coefs = decompCollect$coefsOutCat


## debug dryad_outputs
args(dryad_outputs)
#InputCollect
#OutputModels
pareto_fronts = "auto"
calibration_constraint = 0.1
plot_folder = dryad_object
plot_folder_sub = NULL
plot_pareto = TRUE
csv_out = "pareto"
clusters = TRUE
select_model = "clusters"
ui = FALSE
export = TRUE
quiet = FALSE

## debug dryad_pareto
args(dryad_pareto)

#InputCollect
#OutputModels
pareto_fronts = "auto"
calibration_constraint = 0.1
quiet = FALSE

## debug dryad_clusters
input = OutputCollect
dep_var_type = InputCollect$dep_var_type
all_media = NULL
k = "auto"
limit = 1
weights = rep(1, 3)
dim_red = "PCA"
quiet = FALSE
export = TRUE

## debug dryad_response
args(dryad_response)
dryad_object = NULL
select_build = NULL
media_metric = decompSpendDistPar$rn[4]
select_model = decompSpendDistPar[4, solID]
metric_value = decompSpendDistPar[4, mean_spend]
dt_hyppar = resultHypParamPar
dt_coef = xDecompAggPar


## debug dryad_refresh
args(dryad_refresh)
dryad_object = NULL
# json_file
# dryad_object
dt_input = dt_simulated_weekly
dt_holidays = dt_prophet_holidays
refresh_steps = 3
refresh_mode = "manual" # "auto", "manual"
refresh_iters = 100
refresh_trials = 2
plot_pareto = TRUE
plot_folder = NULL
version_prompt = FALSE
export = TRUE
#calibration_input



## debug dryad_allocator
args(dryad_allocator)
# InputCollect = NULL
# OutputCollect = NULL
# select_model = NULL
dryad_object = NULL
select_build = NULL
optim_algo = "SLSQP_AUGLAG"
scenario = "max_historical_response"
expected_spend = NULL
expected_spend_days = NULL
channel_constr_low = rep(0.5, 5)
channel_constr_up = rep(2, 5)
maxeval = 100000
constr_mode = "eq"
ui = FALSE

## debug adstock_weibull
x = 1:120
shape = 1
scale = 0.5
windlen = NULL
type = "cdf"


## debug allocator
#InputCollect = InputCollect
#OutputCollect = OutputCollect
#select_model = select_model
scenario = "max_historical_response"
channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7)
channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
dryad_object = NULL
select_build = NULL
optim_algo = "SLSQP_AUGLAG"
scenario = "max_historical_response"
expected_spend = NULL
expected_spend_days = NULL
maxeval = 100000
constr_mode = "eq"
ui = FALSE

## debug dryad_refresh
args(dryad_refresh)
#dryad_object
plot_folder_sub = NULL
dt_input = dt_simulated_weekly
dt_holidays = dt_prophet_holidays
refresh_steps = 4
refresh_mode = "manual"
refresh_iters = 1000
refresh_trials = 1
plot_pareto = TRUE

## debug dryad_clusters
args(dryad_clusters)
input = OutputCollect
all_media = NULL
k = "auto"
limit = 1
weights = rep(1, 3)
dim_red = "PCA"
quiet = FALSE
export = FALSE

