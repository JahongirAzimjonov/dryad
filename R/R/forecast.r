prophet_forecast_function <- function(depvar, csv_output_dir){
  
 library(prophet)
 #library(magrittr)
  
  df1 <- InputCollect$dep_var
  
  f1 <- prophet(df1, daily.seasonality = FALSE, weekly.seasonality = TRUE, yearly.seasonality = TRUE)
  forecast <- make_future_dataframe(f1, periods = 180)
  # 180 day ahead
  df1fc <- predict (f1, forecast)
  tail(df1fc[c("ds", "yhat", "yhat_lower", "yhat_upper")])
  
  plot(f1, df1fc, xlab = "Years", ylab = "depVar")
  title("Forecast")
  
  prophet_plot_components(f1, df1fc, uncertainty = TRUE)
  
  forecast_excel_output_file_path <- paste(csv_output_dir, "/forecast-on-depvar.xlsx",sep="")
  wb_forecast <- createWorkbook()
  forecast_sheet_name = "Forecast on depVar"
  addWorksheet(wb = wb_forecast, sheetName = forecast_sheet_name)
  
    writeDataTable(wb = wb_forecast, sheet = forecast_sheet_name,
                 x = df_forecast, xy = c("A", 1), rowNames = FALSE,
                 tableStyle = "TableStyleLight9", withFilter = FALSE
  )
  saveWorkbook(wb = wb_forecast, forecast_excel_output_file_path, overwrite = TRUE) 
  print("The forecast results have been written to '.xlsx' file successfully!")
}

 run_week_forecast <- function(mydata){

 colnames(mydata)
 my_depvar_name <- InputCollect$dep_var
 # my_depvar_name
 my_depvar <- mydata[[my_depvar_name]]
  
 diag_res_output_dir <- paste0(OutputCollect$plot_folder,"Forecast")
 diag_res_output_dir
  
 dirs_for_csv = "csv-data"
 csv_output_dir <- create_csv_output_dir(diag_res_output_dir, dirs_for_csv)
 csv_output_dir
 # Create sub-main directories for saving results into pdf files
 dirs_for_plt = "plot-data"
 plt_output_dir <- create_plt_output_dir(diag_res_output_dir, dirs_for_plt)
 plt_output_dir
  
 forecast_dryad <- prophet_forecast_function(my_depvar, csv_output_dir)
 forecast_dryad
 
 }