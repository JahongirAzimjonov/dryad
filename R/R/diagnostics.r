## check for missing packages and install them
list.of.packages <- c("ggplot2", "goftest","nortest", "openxlsx", "readxl",
                      "RTransferEntropy", "ggpubr","purrr",
                      "tseries", "fastmatrix", "genridge")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
set.seed(123)
library(ggplot2)
library(nortest)
library(openxlsx)
library(readxl)
library(RTransferEntropy)
library(ggpubr)
library(purrr)
library(tseries)
library(fastmatrix)
library(genridge)
# library(car)
# library(goftest)
# install.packages("fastmatrix")
# install.packages("genridge")
# install.packages("ggpubr")
# install.packages("nortest")

################################################################

create_csv_output_dir <- function(result_output_dir, sub_dir_csv){
  print(result_output_dir)
  if (!dir.exists(result_output_dir)){
    dir.create(result_output_dir)
    print("Main directory for saving csv files created successfully!")
  }
  else {
    print("Main directory for saving csv files already exists!")
  }
  
  csv_out_dir <- file.path(result_output_dir, sub_dir_csv)
  print(csv_out_dir)
  
  if (!dir.exists(csv_out_dir)){
    dir.create(csv_out_dir)
    print("Sub-main directory for saving csv files created successfully!")
  }
  else {
    print("Sub-main directory for saving csv files already exists!")
  }
  return(csv_out_dir)
  }

create_plt_output_dir <- function(result_output_dir, sub_dir_plt){
  print(result_output_dir)
  if (!dir.exists(result_output_dir)){
    dir.create(result_output_dir)
    print("Main directory for saving plot files created successfully!")
  }
  else {
    print("Main directory for saving plot files already exists!")
  }
  
  plt_out_dir <- file.path(result_output_dir, sub_dir_plt)
  print(plt_out_dir)
  
  if (!dir.exists(plt_out_dir)){
    dir.create(plt_out_dir)
    print("Sub-main directory for saving plot files created successfully!")
  }
  else {
    print("Sub-main directory for saving plot files already exists!")
  }
  return(plt_out_dir)
}

# With nortest package. Note: Performs the Cramer-von Mises test for the composite hypothesis of normality
cvm_test_for_normality <- function(depvar, depvar_name, csv_output_dir){
library(nortest)
  cramer_testNortest <- cvm.test(depvar) ####  Null hypothesis: Normal distribution
  print(cramer_testNortest)
  
  Method <- c(cramer_testNortest$method)
  P_Value <- c(format(cramer_testNortest$p.value, scientific = FALSE))
  Statistic <- c(cramer_testNortest$statistic)
  DepVar <- c(depvar_name)
  df_cvm <- data.frame(Method, P_Value, Statistic, DepVar)
  # print (df_cvm)
  cvm_csv_file_path <- paste(csv_output_dir, '/cramer-von-mises-normtest.csv', sep="")
  # print(cvm_csv_file_path)
  write.csv(df_cvm, file = cvm_csv_file_path)
  print("The results of the Cramer-von_Mises test on depvar for normality, have been written successfully!")
}

################################################################################

# 2. Transfer entropy
# define set of x of dimension n i.e. x = {spend variable x_1, spend variable x_2, ..., spend variable x_n}, then define y = depVar
#  [calculate] X->Y transfer entropy
#  [calculate] Y->X transfer entropy
#  separate TE test for each spend variable
transfer_entropy_test <- function(entropy_method="Shannon", mydata, paid_media_spends, depvar, 
                                  depvar_name, csv_output_dir, plt_output_dir){
 

 te_excel_output_file_path <- paste(csv_output_dir, '/transfer_entropy.xlsx', sep="")
    
	 library(openxlsx)
	 library(RTransferEntropy)
	 
	wb_te <- createWorkbook()
  for(pms_tr_en in paid_media_spends){
    addWorksheet(wb = wb_te, sheetName = pms_tr_en)
    x <- as.data.frame(mydata)[, pms_tr_en]
    te_results <- transfer_entropy(x = x, 
                                   y = depvar, 
                                   entropy = entropy_method,
                                   # entropy = 'Shannon'
                                   )
    te_csv_output_file_path <- paste0(csv_output_dir, "/TE-", pms_tr_en ,".csv")
    print(te_results, file = te_csv_output_file_path)
    ## We need the elements of te_results data frame since we are going to write it into to a .csv file.
    # Get the coefficients of the results of the transfer entropy function 
    coef_te <- te_results$coef
    
    df_tren <- data.frame(
      Method = c(te_results$entropy, te_results$entropy),
      Variables = c("X->Y","Y->X"),
      Tran.Entr = c(coef_te["X->Y","te"],      coef_te["Y->X","te"]),
      Effect.TE = c(coef_te["X->Y","ete"],     coef_te["Y->X","ete"]),
      Stand.Err. =  c(coef_te["X->Y","se"],      coef_te["Y->X","se"]),
      P_Value =   c(coef_te["X->Y","p-value"], coef_te["Y->X","p-value"]),
      Num.Obs. = c(te_results$nobs, te_results$nobs)
    )
    # print(df_tren)
  
    writeDataTable(wb = wb_te, sheet = pms_tr_en,
                 x = df_tren, xy = c("A", 1), rowNames = FALSE,
                 tableStyle = "TableStyleLight9", withFilter = FALSE
                 )
  }
  ## Save workbook into the csv file
  saveWorkbook(wb = wb_te, te_excel_output_file_path, overwrite = TRUE)
  print("The transfer entropy results have been written to '.xlsx' file successfully!")
  
  # The precise value of the transfer entropy is influenced by the choice of the quantiles.
  # To illustrate the effect, we re-estimate Shannon transfer entropy for a selection of quantiles.
  # The following graph reports the results for increasing tail bins (and, thus, a shrinking central bin).
  # Create a 2 x 5 plotting matrix
  # The next 10 plots created will be plotted next to each other
  te_quantile_plot_list <- vector("list")
  te_plt_ID <- 1
  for (pms_te in paid_media_spends) {
    # X -> Y transfer entropy
    dfXY <- data.frame(q1 = 5:25, q2 = 95:75)
  
    dfXY$ete <- apply(
      dfXY, 1,
      function(el) calc_ete(x = mydata[[pms_te]], y = depvar, quantiles = c(el[["q1"]], el[["q2"]]))
    )
  
    dfXY$quantiles <- factor(sprintf("(%02.f, %02.f)", dfXY$q1, dfXY$q2))
    
    te_quantile_plot_list[[te_plt_ID]] <- local({
      # te_plt_ID <- te_plt_ID + 1
      ggp_teXY <- ggplot(dfXY, aes(x = quantiles, y = ete, group=1)) +
        ggtitle(paste0(pms_te, " - ", depvar_name)) +
        geom_line(color="darkgrey") +
        geom_point() +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = "Quantiles", y = "ETE, [X->Y]")
    })
    te_plt_ID <- te_plt_ID + 1
    
    #****************************************************************************#
  
    ## Y -> X transfer entropy
    dfYX <- data.frame(q1 = 5:25, q2 = 95:75)
  
    dfYX$ete <- apply(
      dfYX, 1,
      function(el) calc_ete(x = depvar, y = mydata[[pms_te]], quantiles = c(el[["q1"]], el[["q2"]]))
    )
  
    dfYX$quantiles <- factor(sprintf("(%02.f, %02.f)", dfYX$q1, dfYX$q2))
      
    te_quantile_plot_list[[te_plt_ID]] <- local({
      # te_plt_ID <- te_plt_ID + 1
      ggp_teYX <- ggplot(dfYX, aes(x = quantiles, y = ete, group=1)) +
        ggtitle(paste0("Revenue-", pms_te)) +
        geom_line(color="darkgrey") +
        geom_point() +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = "Quantiles", y = "ETE, [Y->X]")
      })
    te_plt_ID <- te_plt_ID + 1 
  }
  library(ggpubr)
  figure_arrangeTE <- ggarrange(plotlist=te_quantile_plot_list,
                      # labels = c("A", "B", "C"),
                      ncol = 5, nrow = 2)
  figure_annotateTE <- annotate_figure(figure_arrangeTE,
                  top = text_grob("The relationship between 'Effect.Tr.En.' and 'selection of quantiles'", color = "black", face = "bold", size = 14)
                  )
  figure_annotateTE
  ggsave(file=paste0(plt_output_dir, "/eff_te_main_figure.pdf"), width=20, height=8, dpi=300)
  ggsave(file=paste0(plt_output_dir, "/eff_te_main_figure.png"), width=20, height=8, dpi=300)
  print("The 'transfer entropy vs quantiles' results have been drawn to '.pdf' file successfully!")
  # dev.off()
}

################################################################################

# 3. KPSS Test for Trend Stationarity
# perform KPSS test on depVar
# H0: The time series is trend stationary.
# HA: The time series is not trend stationary.
# Example: The p-value is 0.1. Since this value is not less than .05, we fail to reject the null hypothesis of the KPSS test.
# This means we can assume that the time series is trend stationary.
# Example 2: The p-value is 0.04751. Since this value is less than .05, we reject the null hypothesis of the KPSS test.
# This means the time series is NOT trend stationary.

kpss_test_for_trend_stationary <- function(depvar, csv_output_dir){
  library(tseries)
  kps <- kpss.test(depvar, null = c("Level", "Trend"), lshort = TRUE)
  # kpss_csv_output_file_path <- paste(csv_output_dir, "/kpss-depvar.xlsx",sep="")
  kpss_excel_output_file_path <- paste(csv_output_dir, "/kpss-test-on-depvar.xlsx",sep="")
  wb_kpss <- createWorkbook()
  kpss_sheet_name = "KPSS test on depVar"
  addWorksheet(wb = wb_kpss, sheetName = kpss_sheet_name)
  # Create a data frame object to keep kpss output
  df_kpss = data.frame(
    Statistic = c(kps$statistic),
    Parameter = c(kps$parameter),
    P_Value = c(kps$p.value),
    Method = c(kps$method),
    Data_Name = c(kps$data.name)
  )
  print(df_kpss)
  
  writeDataTable(wb = wb_kpss, sheet = kpss_sheet_name,
                 x = df_kpss, xy = c("A", 1), rowNames = FALSE,
                 tableStyle = "TableStyleLight9", withFilter = FALSE
  )
  saveWorkbook(wb = wb_kpss, kpss_excel_output_file_path, overwrite = TRUE) 
  print("The 'KPSS Test for Trend Stationarity' results have been written to '.xlsx' file successfully!")
}

################################################################################

# Examples VIF for Ridge Regression
# 4. VIF Ridge

vif_ridge_regression <- function(depvar, depvar_name, csv_output_dir, mydata, paid_media_spends, plt_output_dir){
  # myfm <- as.formula(paste(colnames(data)[1], "~", var))
  library(genridge)
  pms_formula <- paste(depvar_name, "~")
  for (i in seq_along(paid_media_spends)) {
    
    fm <- paid_media_spends[i]
    
    if (i >= 1 && i < length(paid_media_spends)) {
      
      pms_formula <- paste(pms_formula, fm, "+")
      
    }
    
    if (i == length(paid_media_spends)) {
      
      pms_formula <- paste(pms_formula, fm)
      
    }
    
  }
  fm <- as.formula(pms_formula)
  fm
  
  lmod <- lm(formula = fm, data = mydata)
  lmod
  vif_output <- vif(lmod)
  vif_output
  library(genridge)
  vif_excel_output_file_path <- paste(csv_output_dir, "/vif-test-on-paid-media-spends.xlsx", sep="")
  wb_vif <- createWorkbook()
  vif_sheet_name = "Variance Inflation Factors"
  addWorksheet(wb = wb_vif, sheetName = vif_sheet_name)
  # Create a data frame object to keep kpss output
  
  df_vif <- as.data.frame(vif_output)
  df_vif
  
    print(df_vif)
  
  writeDataTable(wb = wb_vif, sheet = vif_sheet_name,
                 x = df_vif, xy = c("A", 1), rowNames = FALSE,
                 tableStyle = "TableStyleLight9", withFilter = FALSE
  )
  saveWorkbook(wb = wb_vif, vif_excel_output_file_path, overwrite = TRUE)
  print("The 'VIF for Ridge Regression' results have been written to '.xlsx' file successfully!")
  
  # Calculating the relationship between "the Variance Inflation Factor" and "Degrees of freedom and Ridge Constant (k)"
  mydt.y <- as.data.frame(mydata[,depvar_name])[,]
  # longley.y <- longley[, "Employed"]
  mydt.X <- data.matrix(mydata[paid_media_spends]) 
  # longley.X <- data.matrix(longley[, c(2:6,1)])
  
  lambda <- c(0, 0.005, 0.01, 0.02, 0.04, 0.08)
  lridge <- ridge(mydt.y, mydt.X, lambda=lambda)
  coef(lridge)
  
  vridge <- vif(lridge)
  vridge
  
  # plot VIFs
  pch <- c(15:18, 7, 9)
  clr <- c("black", rainbow(10, start=.6, end=.1))
  
  png(file=paste0(plt_output_dir,"/ridge_const_vif.png"))
  vif_ridge_const_mt_plt <- matplot(rownames(vridge), vridge, type='b', 
                                    xlab='Ridge constant (k)', ylab="Variance Inflation", 
                                    xlim=c(0, 0.091),  
                                    col=clr, pch=pch, cex=1.2) + 
    text(0.095, vridge[1,], colnames(vridge), pos=2)
  dev.off()
  
  png(file=paste0(plt_output_dir,"/degrees_of_freedom_vif.png"))
  matplot(lridge$df, vridge, type='b', 
          xlab='Degrees of freedom', ylab="Variance Inflation", 
          xlim=c(4.9980, 5.00025),
          col=clr, pch=pch, cex=1.2)+
    text(5.00033, vridge[1,], colnames(vridge), pos=2)
  dev.off()
  
  # more useful to plot VIF on the sqrt scale
  png(file=paste0(plt_output_dir,"/sq_ridge_const_vif.png"))
  matplot(rownames(vridge), sqrt(vridge), type='b', 
          xlab='Ridge constant (k)', ylab=expression(sqrt(VIF)), 
          xlim=c(-0.011, 0.08), 
          col=clr, pch=pch, cex=1.2, cex.lab=1.25)+
    text(-0.015, sqrt(vridge[1,]), colnames(vridge), pos=4, cex=1.2)
  dev.off()
  
  png(file=paste0(plt_output_dir,"/sq_degrees_of_freedom_vif.png"))
  matplot(lridge$df, sqrt(vridge), type='b', 
          xlab='Degrees of freedom', ylab=expression(sqrt(VIF)), 
          xlim=c(4.9980, 5.0003),
          col=clr, pch=pch, cex=1.2, cex.lab=1.25)+
    text(5.00039, sqrt(vridge[1,]), colnames(vridge), pos=2, cex=1.2)
  print("The 'VIF for Ridge Regression' results have been drawn to '.png' files successfully!")
  dev.off()
}

################################################################################

# 5. QQ Plot
# paid_media_colors = rainbow(length(paid_media_spends))
qqplot_figures <- function(plt_output_dir, paid_media_spends, paid_media_colors){
  pdf(paste(plt_output_dir, "/Q-Q_paid_media_spends.pdf", sep=""), onefile = TRUE)
  for (pms_id in seq_along(paid_media_spends)){
library(ggpubr)
library(ggplot2)
   pms <- paid_media_spends[pms_id]
    ggqqp <- ggqqplot(data = mydata,
             x = pms,
             title = paste("Q-Q Plot over", pms),
             color = paid_media_colors[pms_id]
            )
    print(ggqqp)
  } 
  dev.off()
  
  ggQQ_plot_list <- vector("list")
  ggQQ_plt_ID <- 1
  for (pms_ggQQ in paid_media_spends) {
    ggQQ_plot_list[[ggQQ_plt_ID]] <- local({
      pms_ggQQ <- paid_media_spends[[ggQQ_plt_ID]]
      ggQQ_pms_depvar <- ggqqplot(data = mydata,
                                  x = pms_ggQQ,
                                  title = paste("Q-Q Plot over", pms_ggQQ),
                                  color = paid_media_colors[ggQQ_plt_ID])
    })
    
    ggQQ_plt_ID <- ggQQ_plt_ID + 1
    # print(gg_plt_ID)
  }
  figure_arr_ggpQQ <- ggarrange(plotlist=ggQQ_plot_list,
                                # labels = c("A", "B", "C"),
                                ncol = 2, nrow = 3)
  figure_ann_ggpQQ <- annotate_figure(figure_arr_ggpQQ,
                                      top = text_grob("The 'Quantile-Quantile plots of paid media spendings'", 
                                                      color = "black", face = "bold", size = 14)
  )
  figure_ann_ggpQQ
  ggsave(file=paste0(plt_output_dir, "/Q-Q_paid_media_spends.png"), width=17, height=15, dpi=300, device = "png")
  # png(filename = paste0(plt_output_dir, "/Q-Q_paid_media_spends_revenue_figure.png"), width = 1080, height = 720)
  # print(figure_ann_ggpQQ)
  # dev.off()
  print("The 'Graphs of Dependent vs Independent Variables' results have been drawn to '.png' files successfully!")
  # dev.off()
  
  pdf(paste(plt_output_dir, "/Density_paid-media-spends-density-plots.pdf", sep=""), onefile = TRUE)
  for (pms_id in seq_along(paid_media_spends)){
    pms <- paid_media_spends[pms_id]
    ggdens <- ggdensity(data = mydata, 
                        x = pms,
                       add = "mean", 
                       rug = TRUE,
                       color = paid_media_colors[pms_id], 
                       fill = paid_media_colors[pms_id])
    print(ggdens)
  }
  print("The 'density' results have been drawn to '.pdf' files successfully!")
  dev.off()
  
  ggDD_plot_list <- vector("list")
  ggDD_plt_ID <- 1
  for (pms_ggDD in paid_media_spends) {
    ggDD_plot_list[[ggDD_plt_ID]] <- local({
      pms_ggDD <- paid_media_spends[[ggDD_plt_ID]]
      ggDD_pms_depvar <- ggdensity(data = mydata, 
                                   x = pms_ggDD,
                                   add = "mean", 
                                   rug = TRUE,
                                   color = paid_media_colors[ggDD_plt_ID], 
                                   fill = paid_media_colors[ggDD_plt_ID])
      })
    
    ggDD_plt_ID <- ggDD_plt_ID + 1
    # print(gg_plt_ID)
  }
  figure_arr_ggpDD <- ggarrange(plotlist=ggDD_plot_list,
                                # labels = c("A", "B", "C"),
                                ncol = 2, nrow = 3)
  figure_ann_ggpDD <- annotate_figure(figure_arr_ggpDD,
                                      top = text_grob("The 'density plots of paid media spendings'", 
                                                      color = "black", face = "bold", size = 14)
  )
  figure_ann_ggpDD
  ggsave(file=paste0(plt_output_dir, "/Density_paid-media-spends-density-plots.png"), width=17, height=15, dpi=300)
  print("The 'Graphs of Dependent vs Independent Variables' results have been drawn to '.png' files successfully!")
  # dev.off()
}
# qq_plot_dryad <- qqplot_figures(plt_output_dir, paid_media_spends, paid_media_colors)
# qq_plot_dryad

################################################################################

# 6. a graph of depVar vs paid_media_spends (on one graph)
graph_depvar_vs_indepvars <- function(plt_output_dir, mydata, depvar, 
                                            paid_media_spends, depvar_name, paid_media_colors){
  library(ggplot2)
  myColors <- c("#e49444", "#d1615d", "#85b6b2", "#6a9f58", "#5778a4", "#e7ca60", "#a87c9f", "#f1a2a9", "#967662", "#b8b0ac")
  pdf(paste(plt_output_dir, "/paid-media-spends-vs-revenue.pdf", sep=""), onefile = TRUE)
  for (pms_id in seq_along(paid_media_spends)){
    pms <- paid_media_spends[pms_id]
    ggp_pms_depVar <- ggplot(mydata, aes(x = mydata[[pms]], y = depvar)) +
      geom_point(color=paid_media_colors[pms_id]) +
      geom_smooth(color="darkgreen", linewidth=2)+
      # geom_line(color=myColors[pms_id])+
      theme_bw()+
      # theme(axis.text.x = element_text(angle = 90)) +
      labs(x = pms, y = depvar_name)
    # gpp
    ggp_pms_depVar + ggtitle(paste(depvar_name, pms, sep=""))
    print(ggp_pms_depVar)
  }
  # print("The 'Graphs of Dependent vs Independent Variables' results have been drawn to '.pdf' files successfully!")
  dev.off()
  
  gg_plot_list <- vector("list")
  gg_plt_ID <- 1
  for (pms_gg in paid_media_spends) {
    gg_plot_list[[gg_plt_ID]] <- local({
      pms_gg <- paid_media_spends[[gg_plt_ID]]
      gg_pms_depvar <- ggplot(mydata, aes(x = mydata[[pms_gg]], y = depvar, group=1)) +
        geom_line(color=paid_media_colors[gg_plt_ID]) +
        geom_point(color=paid_media_colors[gg_plt_ID]) +
        geom_smooth(color="darkgreen", linewidth=2) +
        theme_bw() +
        labs(x = pms_gg, y = depvar_name)
    })
    gg_plt_ID <- gg_plt_ID + 1
    # print(gg_plt_ID)
  }
  figure_arr_ggp <- ggarrange(plotlist=gg_plot_list,
                              # labels = c("A", "B", "C"),
                              ncol = 2, nrow = 3)
  figure_ann_ggp <- annotate_figure(figure_arr_ggp,
                                     top = text_grob("The relationship between 'paid media spendings' and 'revenue'", 
                                                     color = "black", face = "bold", size = 14)
  )
  figure_ann_ggp
  ggsave(file=paste0(plt_output_dir, "/paid-media-spends-vs-revenue.png"), width=19, height=11, dpi=300)
  print("The 'Graphs of Dependent vs Independent Variables' results have been drawn to '.png' files successfully!")
  # dev.off()
}


############################################################################################################
run_diagnostics <- function(mydata){
  # This variables are used for just conducting some diagnostics tests on dep and indep vars 
  # The index of the depvar_name in the main dataframe is determined manually by checking df colnames as below
  colnames(mydata)
  # In our case it is at the third index of df column names
  # my_depvar_name <- colnames(mydata)[3]
  my_depvar_name <- InputCollect$dep_var
  # my_depvar_name
  my_paid_media_spends <- InputCollect$paid_media_spends
  # my_paid_media_spends
  
  my_paid_media_impressions <- InputCollect$paid_media_vars
  my_context_vars <- InputCollect$context_vars
  # my_context_vars
   my_paid_media_colors = rainbow(length(my_paid_media_spends))
  
  my_depvar <- mydata[[my_depvar_name]]
  # my_depvar
    
  # my_all_media <- all_media_data
  
  # mydata[my_depvar_name]
  # mydata[my_paid_media_spends]
  
  # This part is for conducting some diagnostics tests on dependent and independent variables
  
  # Creating main directory inside dryad results folder. In my case, it is in the Desktop folder.
  diag_res_output_dir <- paste0(OutputCollect$plot_folder,"Diagnostics")
  diag_res_output_dir
  
  # Create sub-main directories for saving results into csv and excel files
  dirs_for_csv = "csv-data"
  csv_output_dir <- create_csv_output_dir(diag_res_output_dir, dirs_for_csv)
  csv_output_dir
  
  # Create sub-main directories for saving results into pdf files
  dirs_for_plt = "plot-data"
  plt_output_dir <- create_plt_output_dir(diag_res_output_dir, dirs_for_plt)
  plt_output_dir
  
  # 1. Cramer von Mises test for Normality with the 'nortest' package. 
  # Note: Performs the Cramer-von Mises test for the composite hypothesis of normality
  cvm_t_f_n <- cvm_test_for_normality(my_depvar, my_depvar_name, csv_output_dir)
  cvm_t_f_n
  
  # 2. Transfer entropy on dependent and independent variables
  te_dryad <- transfer_entropy_test(entropy_method="Shannon", mydata, my_paid_media_spends, my_depvar, 
                                    my_depvar_name, csv_output_dir, plt_output_dir)
  te_dryad
  
  # 3. KPSS Test for Trend Stationarity performs KPSS test on depVar.
  kpss_dryad <- kpss_test_for_trend_stationary(my_depvar, csv_output_dir)
  kpss_dryad
  
  # 4. Examples VIF for Ridge Regression
  vif_ridge_regres_dryad <- vif_ridge_regression(my_depvar, my_depvar_name, 
                                                 csv_output_dir, mydata, my_paid_media_spends, plt_output_dir)
  vif_ridge_regres_dryad 
  
  # 5. Quantile-Quantile Plots
  qq_plot_dryad <- qqplot_figures(plt_output_dir, my_paid_media_spends, my_paid_media_colors)
  qq_plot_dryad
  
  # 6. Graphs of depVar vs paid_media_spends (on one graph)
  dep_indep_vars_graph_dryad <- graph_depvar_vs_indepvars(plt_output_dir, mydata, my_depvar, 
                                                          my_paid_media_spends, my_depvar_name, my_paid_media_colors)
  dep_indep_vars_graph_dryad
  
  # # 7. A graph of paid_media_impressions vs paid_media_spends (on one graph)
   # colnames(my_all_media)
   # pm_im_sp_dryad <- pm_impress_vs_spends_graph(my_all_media, my_paid_media_impressions, my_paid_media_spends, plt_output_dir)
   # pm_im_sp_dryad  
}
