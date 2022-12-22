# Package names
packages <- c("ggplot2", "readxl", "tidyverse", "pacman", "forecast", "fpp2", "tools", "RColorBrewer", "reshape")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
# pacman::p_load(ggplot2, tidyr, dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(forecast)
library(fpp2)
library(tools)
library(prophet)
library(dplyr)
library(reshape)

write_forecasting_res_to_excel_file <- function(excel_output_dir, excel_file_name, forcasted_df){
  forcast_excel_output_file_path <- paste(excel_output_dir, '/', excel_file_name, '.xlsx', sep="")
  
  wb_df <- createWorkbook()
  
  mf_cols <- colnames(forecasted_df)
  for (mf_col_name in mf_cols) {
    # print(mf_col_name)
    mf_forecast_df <- as.data.frame(forecasted_df[[mf_col_name]])
    colnames(mf_forecast_df)[1] <- mf_col_name
    # print(mf_forecast_df)
    
    addWorksheet(wb = wb_df, sheetName = mf_col_name)
    
    writeDataTable(wb = wb_df, sheet = mf_col_name,
                   x = mf_forecast_df, xy = c("A", 1), rowNames = FALSE,
                   tableStyle = "TableStyleLight9", withFilter = FALSE
    )
  }
  ## Save workbook into the csv file
  saveWorkbook(wb = wb_df, forcast_excel_output_file_path, overwrite = TRUE)
  print("The results have been written to '.xlsx' file successfully!")
}

############################################################################################################
run_forecasting <- function(mydata, testLenInWeek){
  # This variables are used for just conducting some diagnostics tests on dep and indep vars 
  # The index of the depvar_name in the main dataframe is determined manually by checking df colnames as below
  colnames(mydata)
  # In our case it is at the third index of df column names
  # my_depvar_name <- colnames(mydata)[3]
  my_depvar_name <- InputCollect$dep_var
  varOfInterest = my_depvar_name
  my_depvar <- mydata[[my_depvar_name]]
  
  # Creating main directory inside dryad results folder. In my case, it is in the Desktop folder.
  forecast_res_output_dir <- paste0(OutputCollect$plot_folder,"Forecasting")
  forecast_res_output_dir
  
  # Create sub-main directories for saving results into csv and excel files
  dirs_for_csv = "csv-data"
  csv_output_dir <- create_csv_output_dir(forecast_res_output_dir, dirs_for_csv)
  csv_output_dir
  
  # Create sub-main directories for saving results into pdf files
  dirs_for_plt = "plot-data"
  plt_output_dir <- create_plt_output_dir(forecast_res_output_dir, dirs_for_plt)
  plt_output_dir

  makeForecasting <- makeNStepForcast(data, testLenInWeek, varOfInterest)
  # makeForecasting
  forecasted_df <- makeForecasting$forecastedRes
  write_forecasting_res_to_excel_file(excel_output_dir = csv_output_dir, excel_file_name = "ForecastingResults", forecasted_df)
  
  forecasted_plt <- makeForecasting$plottedRes
  ggsave(file=paste0(plt_output_dir,'/ForecastingResults.png'), width=25, height=13, dpi=300, device = "png")
}

makeNStepForcast <- function(exceldata, testLenthsInWeeks, variableOfInterest){
  dfdata = data.frame(exceldata)
  
  df = data.frame(ds=dfdata$DATE, y=dfdata[,c(variableOfInterest)])
  trainLenthsInWeeks <- nrow(df) - testLenthsInWeeks
  
  df_train <- df[1:(nrow(df) - testLenthsInWeeks), ]
  # print(paste("df_train:\n", df_train))
  df_test <- df[(nrow(df) - testLenthsInWeeks + 1):nrow(df), ]
  # print(paste("df_test:\n", df_test))
  
  model <- prophet(df_train, interval.width = 0.8)
  future <- make_future_dataframe(model, periods=testLenthsInWeeks, freq = 'week')
  
  forcast <- predict(model, future)
  # print(forcast)
  
  forcast_df <- tibble(ds = as.Date(forcast$ds),
                       observation = c(head(df$y,-testLenthsInWeeks), rep(NA, testLenthsInWeeks)),
                       fitted = c(head(forcast$yhat,-testLenthsInWeeks), rep(NA, testLenthsInWeeks)),
                       forecast = c(rep(NA, trainLenthsInWeeks), tail(forcast$yhat,testLenthsInWeeks)),
                       lo.80 = c(rep(NA, trainLenthsInWeeks), tail(forcast$yhat_lower,testLenthsInWeeks)),
                       up.80 = c(rep(NA, trainLenthsInWeeks), tail(forcast$yhat_upper,testLenthsInWeeks)),)
  
  # print(paste("Forecast data frame:\n ", forcast_df))
  
  line.cols = c("black", "darkcyan", "goldenrod1")
  shade.cols = brewer.pal(3, "PuBuGn")
  date.breaks <- "6 months"
  date.format = "%b-%y"
  
  p <- ggplot(forcast_df, aes(x=ds,y=observation,colour = "Training",group = 1))+
    geom_line(aes(colour = "Training")) +
    geom_line(forcast_df, mapping=aes(x=ds, y=fitted, colour = "Fitted", group = 1, size=4), linewidth = 0.75) +
    geom_ribbon(forcast_df, mapping=aes(ds, ymin = lo.80, ymax = up.80, colour='Uncertainty', fill = "80%", group=1)) +
    geom_line(forcast_df, mapping=aes(ds, forecast, colour = "Forecast", group = 1,), linewidth = 0.75) +
    scale_x_date(breaks = seq(forcast_df$ds[1],forcast_df$ds[length(forcast_df$ds)], by = date.breaks), 
                 date_labels = date.format) +
    scale_colour_manual(name = "Model Data", values = c("Training" = line.cols[1],
                                   "Fitted" = line.cols[2],
                                   "Forecast" = line.cols[3],
                                   "Uncertainty" = shade.cols[2]),
                        breaks = c("Training", "Fitted", "Forecast","Uncertainty")) +
    # guides(colour = guide_legend(order = 1)) +
    theme(legend.position = 'bottom')
    labs(title = paste0('forcasting ', variableOfInterest),
       #subtitle = 'sub.title',
       #caption = 'caption',
       x = 'Production Months (Every 6 months)',
       y = variableOfInterest) +
    theme.fxdat
  
  returned_results <- list()
  returned_results$plottedRes <- p
  returned_results$forecastedRes <- forcast_df
  # return(p)
  return(returned_results)
}

theme.fxdat <- theme_gdocs() +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "grey50"),
        axis.title.y = element_text(face = "bold", color = "gray30"),
        axis.title.x = element_text(face = "bold", color = "gray30", vjust = -1),
        panel.background = element_rect(fill = "grey95", colour = "grey75"),
        panel.border = element_rect(colour = "grey75"),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.minor.y = element_line(colour = "white", linetype = "dotted"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.minor.x = element_line(colour = "white", linetype = "dotted"),
        strip.background = element_rect(size = 1, fill = "white", colour = "grey75"),
        strip.text.y = element_text(face = "bold"),
        axis.line = element_line(colour = "grey75"))