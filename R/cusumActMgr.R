#' @name cusumActMgr
#' @title cusumActMgr Statistical Process Control to monitor benchmark-relative portfolio performance 
#'
#' @description Monitor the Logarithmic Information Ratio (LIR) of a portfolio relative to its benchmark
#' and raise an alarm when sufficient evidence accrues to indicate that its current LIR is <0
#' using an optimal changepoint detection scheme (the CUSUM algorithm).
#' An object of class \code{"tsfm"} is returned
#'
#' @details
#' Assessing the performance of active managers is hard because active returns are noisy.
#' In addition, their risk must be taken into account, and this is commonly measured by the
#' standard deviation of active returns. Empirical studies of active managers across a wide range
#' of asset classes suggest that an Annualized Logarithmic Information Ratio
#' (LIR = Active Log Return / Std. Dev.(Active Log Return)) of 0.5 over a period of 5 years or more
#' is exceptional, as markets are efficient. The majority of active managers deliver active
#' returns and IR close to 0, and even those with a positive IR are at constant risk of having their
#' added value dissipate. Investors, therefore, must continually estimate the current performance of
#' their active portfolios and determine when sufficient evidence has accrued to suggest that their
#' active return and IR have fallen to 0. Put differently, investors need to rapidly detect changes,
#' particularly negative changes, in the performance of their portfolios
#'
#' There is a rich literature on changepoint detection, and of the many available algorithms,
#' the CUSUM (an acronym for CUmulative SUM) stands out, on account of its simplicity,
#' its robustness to the actual distribution of returns, and the optimal trade-off
#' between detection time and the rate of false alarms that it offers.
#' It is closely related to Wald's Sequential Probability Ratio Test (SPRT) but is much simpler
#' to implement, and requires minimal inputs from the user. In this application, it seeks to
#' determine when the IR of a portfolio has changed from a good level (default = 0.5 )
#' to a bad level (default = 0). An alarm is raised when the CUSUM scheme crosses a threshold,
#' which is chosen to make the average time between false alarms 84 months (7 years).
#' By way of comparison, the time taken to detect a transition from an LIR of 0.5 to 0 is
#' 41 months (or 3 1/2 years). This is much faster than the traditional t-test, which would take
#' 16 years to obtain a t-statistic of 2. The threshold can be changed to meet a user's needs
#'
#'
#' @param portfolioName A character string with the name of the portfolio. Required, no default
#' @param benchmarkName A character string with the name of the benchamark. Required, no default
#' @param return_df An xts object containing the columns \code{portfolioName} and \code{benchmarkName}
#'                  of monthly returns. Required, no default
#' @param upper_IR Numeric value representing the LIR of a good portfolio, default = 0.5
#' @param lower_IR Numeric value representing the LIR of a bad portfolio,  default = 0
#' @param lambda_in Exponential weighting constant used when the data seems consistent with
#'                  the current estimate of volatility. Default = 0.9
#' @param lambda_out Exponential weighting constant used when the data seems inconsistent with
#'                   the current estimate of volatility. Default = 0.8
#' @param winsorize Numeric value >1, of the multiple of the standard deviation at which we winsorize.
#'                  The default is 4
#' @param filterStd Logical value, determines if estimated standard deviations are filtered.
#'                  The default is set to \code{FALSE}
#'
#'
#' @return \code{cusumActMgr} returns a \code{list} containing the following xts objects:
#' \item{Log_Active_Returns}{Logarithmic active returns of the fund relative to its benchmark}
#' \item{Annual_Moving_Average}{Vector of annualized moving average annualized returns}
#' \item{Tracking_Error}{Monthly tracking error of the logarithmic active returns}
#' \item{Information_Ratios}{Vector of monthly information ratios}
#' \item{Lindleys_recursion}{Vector of values from Lindley's recursion with a reset after the detection threshold (6.66) is crossed}
#' \item{Annualized_Cusum_Log_IR}{Vector of the CUSUM of the annualized log information ratios}
#' \item{Means}{xts matrix of estimated current mean of the fund's returns in the first column,
#' the benchmark in the second column, and the logarithmic active returns in the third column}
#' \item{Standard_deviations}{The xts matrix of estimated standard deviations of
#' the fund in the first column, the benchmark in the second column, and the
#' logarithmic active returns in the third column.
#' The estimated standard deviation is filtered with a simple digital filter to 
#' induce some additional persistence if \code{filterStd = TRUE} is specified}
#' \item{Protractor_IR}{The xts matrix of the rays of a protractor from 
#' IR = -IR_min in the first column to IR = +IR_max in the seventh column}
#' \item{Protractor_Log_AR}{The xts matrix of the rays of a protractor from 
#' r = -r_min in the first column to r = +r_max in the seventh column}
#' \item{Excess_Volatility}{The difference between the annualized standard deviation of the
#' portfolio and its benchmark}
#'
#' @author 
#' Chindhanai Uthaisaad (GSoC 2017) Vinav Singh Sancheti (2021) and Thomas Philips (2025)
#'
#' @references
#' Philips, T. K., Yashchin, E. and Stein, D. M. (2003). Using Statistical Process Control
#' to Monitor Active Managers, Journal of Portfolio Management, Fall 2003, pp. 86-94
#'
#' @examples
#' # Data Preprocessing
#' df <- read.csv("Example1.csv", header = T, sep = ",", stringsAsFactors = FALSE)
#' df$Date <- as.yearmon(df$Date, format = "%m/%d/%Y")
#' df <- as.xts(df[, -1], order.by = df$Date)
#' 
#' # Run the CusumActMgr routine to get a results object
#' results <- cusumActMgr(portfolioName = colnames(df_xts)[1], 
#'                        benchmarkName = colnames(df_xts)[2], return_df = df)
#' 
#' # Print page 1 of the results
#' chartCusum(results, select_option = 1, 
#' print_to_screen = TRUE, 
#' print_to_png = TRUE,  png_fn = "Mgr vs Bmk Page 1.png",
#' print_to_pdf = FALSE, pdf_fn = "Mgr vs Bmk Page 2.pdf")
#' 
#' # Print page 2 of the results
#' chartCusum(results, select_option = 2,
#'            print_to_screen = TRUE, 
#'            print_to_png = TRUE,  png_fn = "Mgr vs Bmk Page 2.png",
#'            print_to_pdf = FALSE, pdf_fn = "Mgr vs Bmk Page 2.pdf")
#' @export

############################ < LIBRARIES AND FUNCTIONS >###############################
library(xts)
library(zoo)
library(scales)
library(RobStatTM)
library(ggpubr)
library(lemon)
library(gridExtra)
library(ggplot2)
library(dplyr)

# muEst returns a simple weighted average of the current return and the last period's return.
# r      = Current period return
# mu0    = Mean return from the last period
# sigma0 = Estimate of volatility from the last period. If unavailable, set sigma0 = 0
# win_level = Number of standard deviations at which we winsorize (default: win_level =4)
# lambda    = Exponential weighting constant (default: 0.9)

muEst <- function(r, mu0, sigma0, win_level = 4, lambda = 0.9) {
  # Winsorize if the current return is exceptionally large
  if (abs(r - mu0) > win_level * sigma0) {
    r_ <- mu0 + sign(r - mu0) * win_level * sigma0
  } else {
    r_ <- r
  }
  return(lambda * mu0 + (1 - lambda) * r_)
}



# sigmaEst computes a Exponentially Weighted estimate of volatility
# r          = Current period return
# mu0        = Prior period mean return (often set to 0 in finance as market efficiency diminishes the mean)
# sigma0     = Prior period estimated volatility. If unavailable (e.g. in the first period), sigma0 = 0
# win_level  = Number of standard deviations at which we winsorize (default: 4)
# lambda_in  = EW constant when the data seems consistent with the current estimate of volatility (default: 0.9)
# lambda_out = EW constant when the data seems inconsistent with the current estimate of volatility (default: 0.8)

sigmaEst <- function(r, mu0, sigma0, win_level = 4, lambda_in = 0.9, lambda_out = 0.8) {
  # Shift lambda down if current return is exceptionally large or exceptionally small
  lambda <- ifelse((abs(r - mu0) > sigma0 / win_level) && 
                   (abs(r - mu0) < sigma0 * win_level), 
                   lambda_in, lambda_out)
  return(sqrt(lambda * sigma0^2 + (1 - lambda) * (r - mu0)^2))
}



# Charting function, which takes a results object, extracts all the data and then plots it
chartCusum <- function(results_obj, digits = 3, select_option = NULL, 
                       print_to_screen = TRUE, print_to_png = TRUE, print_to_pdf = FALSE,
                       png_fn = NULL, pdf_fn = NULL,...) {
  options(digits = digits)
  select_option.vec <- select_option
  select_option     <- select_option[1]

  repeat {
    switch(select_option,
      `1L` = {
              # Turn zoo objects into dataframes for ggplot
              obj1 <- fortify.zoo(100 * results$Log_Active_Returns)
              obj2 <- fortify.zoo(results$Information_Ratios)
              obj3 <- fortify.zoo(100 * sqrt(12) * results$Tracking_Error)
              obj4 <- fortify.zoo(100 * results$Excess_Volatility[, 3])
      
              #### MONTHLY ACTIVE LOG RETURN PLOT
              names(results$Annual_Moving_Average) <- "AnnualMovingAverage"
              log_ret_plot <- ggplot(data = obj1, aes(x = Index, y = get(ptfName)) ) +
                                geom_bar(stat = "identity", 
                                         aes(fill = get(ptfName) < 0), 
                                         colour = "NA") +
                                scale_fill_manual(guide = "none", 
                                                  breaks = c(TRUE, FALSE), 
                                                  values = c("#F32008", "#269626")) +
                                scale_x_continuous(breaks = pretty_breaks(n = 5)) +																	   
                                geom_line(data = results$Annual_Moving_Average * 100, 
                                          aes(x = Index, 
                                              y = AnnualMovingAverage, 
                                              colour = "12 Month Moving Average"), 
                                          linewidth = 0.8) +
                                labs(title = "Monthly Active Log Return", 
                                     y = "Active Log Return (%)", x = "Month") +
                                scale_colour_manual("", values = c("12 Month Moving Average" = "#3364f6")) +
                                theme(
                                      panel.border = element_rect(colour = "black", 
                                                                  fill = NA), 
                                      legend.position = "inside",
                                      legend.position.inside = c(.5, .1),
                                      legend.direction = "horizontal", 
                                      legend.background = element_rect(fill = "transparent"),
                                      panel.grid.major.x = element_line(linewidth = 0.5, 
                                                                        linetype = "dotted", 
                                                                        colour = "grey"),
                                      panel.grid.major.y = element_line(linewidth = 0.5, 
                                                                        linetype = "dashed", 
                                                                        colour = "grey"),
                                      panel.background = element_blank(), 
                                      plot.title = element_text(hjust = 0.5, size = 18), 
                                      legend.key = element_rect(fill = NA, color = NA)
                                     ) +
                                guides(color = guide_legend(override.aes = list(fill = NA))) +
                                geom_hline(yintercept = 0)
      
      
              #### LOGARITHMIC TRACKING ERROR PLOT
              tracking_err_plot <- ggplot(data = obj3, aes(x = Index, y = Active) ) +
                                    geom_line(aes(), color = 4, linewidth = 0.7) +
                                    labs(title = "Annualized Tracking Error", 
                                         y = "Standard Deviation of Active Log Returns (%)", 
                                         x = "Month") +
                                    theme(
                                          panel.border = element_rect(colour = "black", 
                                                                      fill = NA),
                                          panel.background = element_blank(), 
                                          panel.grid.major.x = element_line(linewidth = 0.5, 
                                                                            linetype = "dotted", 
                                                                            colour = "grey"),
                                          panel.grid.major.y = element_line(linewidth = 0.5, 
                                                                            linetype = "dashed", 
                                                                            colour = "grey"),
                                          plot.title = element_text(hjust = 0.5, size = 18)
                                         ) +
                                    scale_y_continuous(breaks = pretty_breaks(n = 5)) +
                                    scale_x_continuous(breaks = pretty_breaks(n = 5))
      
      
              #### LOGARITHMIC INFORMATION RATIO TWO COLOR GRAPH
              log_ir_graph <- ggplot(data = obj2, aes(x = Index, y = get(ptfName))) +
                                geom_bar(stat = "identity", 
                                         aes(fill = get(ptfName) < 0), 
                                         colour = "NA") +
                                scale_fill_manual(guide = "none", 
                                                  breaks = c(TRUE, FALSE), 
                                                  values = c("#F32008", "#269626")) +
                                labs(title = "Estimated Annualized Log IR", 
                                     y = "Logarithmic IR", x = "Month") +
                                theme(
                                      panel.border = element_rect(colour = "black",
                                                                  fill = NA), 
                                      panel.background = element_blank(),
                                      panel.grid.major.x = element_line(linewidth = 0.5, 
                                                                        linetype = "dotted", 
                                                                        colour = "grey"),
                                      panel.grid.major.y = element_line(linewidth = 0.5, 
                                                                        linetype = "dashed", 
                                                                        colour = "grey"),
                                      plot.title = element_text(hjust = 0.5, size = 18)
                                     ) +
                                geom_hline(yintercept = 0) +
                                scale_x_continuous(breaks = pretty_breaks(n = 5))
      
      
              #### EXCESS VOL GRAPH
              excess_vol <- ggplot(data = obj4,
                                   aes(x = Index, y = ExcessVol)) +
                              geom_line(aes(), color = 4, linewidth = 0.7) +
                              labs(title = "Vol(Portfolio) - Vol(Benchmark)", 
                                   y = "Difference in Annualized Std. Dev. of Log Returns (%)", 
                                   x = "Month") +
                              geom_hline(yintercept = 0, 
                                         linetype = "dotted", 
                                         colour = "black") +
                              theme(panel.border = element_rect(colour = "black", 
                                                                fill = NA), 
                                    panel.background = element_blank(), 
                                    panel.grid.major.x = element_line(linewidth = 0.5, 
                                                                      linetype = "dotted", 
                                                                      colour = "grey"),
                                    panel.grid.major.y = element_line(linewidth = 0.5, 
                                                                      linetype = "dashed", 
                                                                      colour = "grey"),
                                    plot.title = element_text(hjust = 0.5, size = 18)) +
                              scale_y_continuous(breaks = pretty_breaks(n = 5)) +
                              scale_x_continuous(breaks = pretty_breaks(n = 5))  
              
              if (min(obj4$ExcessVol) < 0){
                excess_vol <- excess_vol + geom_hline(yintercept = 0)
              }
      
              # Create a title for the page
              ptf_vs_bmk <- paste0(ptfName, " vs. ", bmkName, "\n", 
                                   index(df_xts)[1], " to ", tail(index(df_xts), 1))
              page_title <- text_grob(ptf_vs_bmk, size = 15, face = "bold")
              
              # View graphs
              if(print_to_screen){
                grid.arrange(log_ret_plot, tracking_err_plot, 
                             log_ir_graph, excess_vol, 
                             layout_matrix = rbind(c(1, 2), c(3, 4)),
                             top = page_title)
              }
      
              ## Save plots to pdf
              if(print_to_pdf){
                pdf(pdf_fn, width = 8, height = 8)
                  grid.arrange(log_ret_plot, tracking_err_plot, 
                               log_ir_graph, excess_vol, 
                               layout_matrix = rbind(c(1, 2), c(3, 4)), 
                               top = page_title)
                dev.off()
              }        
      
              # Save plots to png
              if(print_to_png){
                png(filename = png_fn, width = 8, height = 8, units = "in",
                    pointsize = 12, bg = "white", res = 144, 
                    family = "", restoreConsole = TRUE,
                    type = c("windows", "cairo", "cairo-png"), antialias = "d")
                
                  grid.arrange(log_ret_plot, tracking_err_plot, 
                               log_ir_graph, excess_vol, 
                               layout_matrix = rbind(c(1, 2), c(3, 4)), 
                               top = page_title)
                dev.off()
              }
            },
            
      
      `2L` = {
              names(results$Annualized_Cusum_Log_AR) <- "Cusum_Log_AR"
              names(results$Annualized_Cusum_Log_IR) <- "CusumIR"
              names(results$Lindleys_recursion) <- "LRTest"
              
              # Turn zoo objects into dataframes for ggplot
              # Negate Lindley's recursion so that it plots correctly
              obj1 <- fortify.zoo(results$Annualized_Cusum_Log_AR)
              obj2 <- fortify.zoo(results$Annualized_Cusum_Log_IR)
              obj3 <- fortify.zoo(results$Lindleys_recursion * -1)
              obj4 <- fortify.zoo(results$Means * 100)
              
              colors1 <- c("firebrick4", "firebrick3", "firebrick2", 1, 
                           "green2",     "green3",     "green4")
              
              
              # CUSUM LOG ACTIVE RETURN PLOT:
              cusum_active_log_ret <- ggplot(data = obj1, 
                                             aes(x = Index, y = Cusum_Log_AR)) +
                                        geom_line(aes(), colour = 4, linewidth = 0.7) +
                                        geom_line(data  = results$Protractor_Log_AR, 
                                                  aes(y = `Ray-3`, 
                                                      colour = paste0("Ann. Active Log Return = ", 
                                                                      results$`Log_AR_Slopes`[1], 
                                                                      "% / annum"))) +
                                        geom_line(data  = results$Protractor_Log_AR, 
                                                  aes(y = `Ray-2`, 
                                                      colour = paste0("Ann. Active Log Return = ", 
                                                                      results$`Log_AR_Slopes`[2], 
                                                                      "% / annum"))) +
                                        geom_line(data  = results$Protractor_Log_AR, 
                                                  aes(y = `Ray-1`, 
                                                      colour = paste0("Ann. Active Log Return = ", 
                                                                      results$`Log_AR_Slopes`[3], 
                                                                      "% / annum"))) +
                                        geom_line(data  = results$Protractor_Log_AR, 
                                                  aes(y = `Ray0`, 
                                                      colour = paste0("Ann. Active Log Return = ", 
                                                                      results$`Log_AR_Slopes`[4], 
                                                                      "% / annum"))) +
                                        geom_line(data  = results$Protractor_Log_AR, 
                                                  aes(y = `Ray+1`, 
                                                      colour = paste0("Ann. Active Log Return = ", 
                                                                      results$`Log_AR_Slopes`[5], 
                                                                      "% / annum"))) +
                                        geom_line(data  = results$Protractor_Log_AR, 
                                                  aes(y = `Ray+2`, 
                                                      colour = paste0("Ann. Active Log Return = ", 
                                                                      results$`Log_AR_Slopes`[6], 
                                                                      "% / annum"))) +
                                        geom_line(data  = results$Protractor_Log_AR, 
                                                  aes(y = `Ray+3`, 
                                                      colour = paste0("Ann. Active Log Return = ", 
                                                                      results$`Log_AR_Slopes`[7], 
                                                                      "% / annum"))) +
                                        scale_colour_manual(breaks = paste0("Ann. Active Log Return = ", 
                                                                            results$`Log_AR_Slopes`, 
                                                                            "% / annum"), 
                                                            values = colors1, 
                                                            name = "Slopes of guide lines ") +
                                        theme(
                                              axis.title.y = element_blank(), 
                                              axis.text.y = element_blank(), 
                                              axis.ticks.y = element_blank(), 
                                              plot.title = element_text(hjust = 0.5, 
                                                                        size = 18),
                                              panel.grid.major.x = element_line(linewidth = 0.5, 
                                                                                linetype = "dotted", 
                                                                                colour = "grey"), 
                                              panel.grid.minor.x = element_blank(),
                                              panel.grid.major.y = element_blank(),
                                              panel.grid.minor.y = element_blank(),
                                              panel.background = element_blank(), 
                                              axis.line = element_line(colour = "black"),
                                              legend.direction = "vertical",
                                              panel.border = element_rect(colour = "black", 
                                                                          fill   = NA), 
                                              legend.key = element_rect(colour = "transparent", 
                                                                        fill   = "transparent"),
                                              legend.background = element_blank(),
                                              legend.position = "inside",
                                              legend.position.inside = c(0.65, 0.25),
                                              legend.text  = element_text(size=8),
                                              legend.key.size =  unit(0.3, "cm"),
                                              legend.title=element_text(size=12)    
                                             ) +
                                        geom_line(data = results$Protractor_Log_AR, 
                                                  aes(y = `Ray0`, colour = paste0("Ann. Active Log Return = ", 
                                                                                  results$`Log_AR_Slopes`[4], 
                                                                                  "% / annum"))) +
                                        labs(x = "Year", 
                                             title = "Cumulative Active Log Return") +
                                        scale_y_continuous(expand = c(0, 0)) +
                                        scale_x_continuous(breaks = pretty_breaks(n = 10), 
                                                           expand = c(0, 0))
      
      
      
              # CUSUM LOGARITHMIC IR PLOT
              cusum_log_ir <- ggplot(data = obj2, 
                                     aes(x = Index, y = CusumIR)) +
                                geom_line(aes(), colour = 4, linewidth = 0.7) +
                                geom_line(data = results$Protractor_IR, 
                                          aes(y = `Ray-3`,
                                              colour = paste0("Annualized Log IR = ", 
                                                              results$`IR_Slopes`[1]))) +
                                geom_line(data = results$Protractor_IR, 
                                          aes(y = `Ray-2`, 
                                              colour = paste0("Annualized Log IR = ", 
                                                              results$`IR_Slopes`[2]))) +
                                geom_line(data = results$Protractor_IR, 
                                          aes(y = `Ray-1`, 
                                              colour = paste0("Annualized Log IR = ", 
                                                              results$`IR_Slopes`[3]))) +
                                geom_line(data = results$Protractor_IR, 
                                          aes(y = `Ray0`, 
                                              colour = paste0("Annualized Log IR = ", 
                                                              results$`IR_Slopes`[4]))) +
                                geom_line(data = results$Protractor_IR, 
                                          aes(y = `Ray+1`, 
                                              colour = paste0("Annualized Log IR = ", 
                                                              results$`IR_Slopes`[5]))) +
                                geom_line(data = results$Protractor_IR, 
                                          aes(y = `Ray+2`, 
                                              colour = paste0("Annualized Log IR = ", 
                                                              results$`IR_Slopes`[6]))) +
                                geom_line(data = results$Protractor_IR, 
                                          aes(y = `Ray+3`, 
                                              colour = paste0("Annualized Log IR = ", 
                                                              results$`IR_Slopes`[7]))) +
                                scale_colour_manual(breaks = paste0("Annualized Log IR = ", 
                                                                    results$IR_Slopes), 
                                                    values = colors1, 
                                                    name = "Slopes of guide lines ") +
                                theme(
                                      axis.title.y = element_blank(), 
                                      axis.text.y = element_blank(), 
                                      axis.ticks.y = element_blank(), 
                                      plot.title = element_text(hjust = 0.5, size = 18),
                                      panel.grid.major.x = element_line(linewidth = 0.5, 
                                                                        linetype = "dotted", 
                                                                        colour   = "grey"), 
                                      panel.grid.minor.x = element_blank(),
                                      panel.grid.major.y = element_blank(),
                                      panel.grid.minor.y = element_blank(),
                                      panel.background = element_blank(), 
                                      axis.line = element_line(colour = "black"),
                                      legend.title = element_text(size=12), 
                                      legend.text  = element_text(size=8), 
                                      legend.key.size  =  unit(0.3, "cm"),
                                      legend.direction = "vertical",
                                      legend.key = element_rect(colour = "transparent", 
                                                                fill   = "transparent"),
                                      legend.background = element_blank(),
                                      legend.position = "inside",
                                      legend.position.inside = c(0.75, 0.25),
                                      panel.border = element_rect(colour = "black", 
                                                                  fill   = NA)
                                     ) +
                                geom_line(data = results$Protractor_IR, 
                                          aes(y = `Ray0`, 
                                              colour = paste0("Annualized Log IR = ", 
                                                              results$`IR_Slopes`[4]))) +
                                labs(x = "Year", 
                                     title = "Cumulative Sum: Logarithmic IR") +
                                scale_y_continuous(expand = c(0, 0)) +
                                scale_x_continuous(breaks = pretty_breaks(n = 10), 
                                                   expand = c(0, 0))
      
      
              ###### Lindley's Recursion for the Likelihood Ratio Test
              cusum_levels <- c(-4.25, -5.62, -6.66)
              label_levels <- cusum_levels + 0.30
              lr_test <- ggplot(data = obj3, 
                                aes(x = Index, y = LRTest) ) +
                          geom_line(aes(), color = 4, linewidth = 0.7) +
                          labs(title = "Likelihood Ratio Test", 
                               x = "Year", y = "Scaled Likelihood Ratio") +
                          theme(panel.border = element_rect(colour = "black",fill = NA), 
                                panel.background = element_blank(),
                                panel.grid.major.x = element_blank(), 
                                panel.grid.minor.x = element_blank(),
                                panel.grid.major.y = element_blank(),
                                panel.grid.minor.y = element_blank() ) +
                          scale_y_continuous(expand = c(0, 0)) +
                          scale_x_continuous(expand = c(0, 0), 
                                             breaks = pretty_breaks(n = 10)) +
                          geom_hline(yintercept = 0) +
                          geom_hline(yintercept = cusum_levels[1], 
                                     colour = "green4", linewidth = 1) +
                          annotate("text", x = index(df_xts)[1], y = label_levels[1], 
                                   hjust = 0, label = "   36 mths | 22 mths") +
                          geom_hline(yintercept = cusum_levels[2], colour = "gold", linewidth = 1) +
                          annotate("text", x = index(df_xts)[1], y = label_levels[2], 
                                   hjust = 0, label = "   60 mths | 32 mths") +
                          geom_hline(yintercept = cusum_levels[3], colour = "red", linewidth = 1) +
                          annotate("text", x = index(df_xts)[1], y = label_levels[3], 
                                   hjust = 0, label = "   84 mths | 41 mths") +
                          theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                          annotate("text", x = index(df_xts)[1], y = -2, hjust = 0,
                                   label = "  Expected Time to First Crossing\n   IR = 0.5 | IR = 0 ", 
                                   size = 4)
      
      
              ###### Portfolio Vs Benchmark Scatter plot and Regression Line
              portRet  <- obj4$Portfolio
              benchRet <- obj4$Benchmark
              
              robust_fit       <- lmrobdetMM(portRet ~ benchRet)
              robust_intercept <- robust_fit$coefficients[1]
              robust_slope     <- robust_fit$coefficients[2]
              
              annualized_alpha <- round(12 * robust_intercept, 2)
              robust_beta      <- round(robust_slope, 2)
              
              ptf_vs_bmk_scatter_plot <- ggplot(data = obj4, 
                                                aes(x = Benchmark, 
                                                    y = Portfolio)) +
                                          geom_point(aes(), colour = 4, shape = 4) +
                                          geom_hline(yintercept = 0, 
                                                     linetype = "dotted", 
                                                     colour = "black") +
                                          geom_vline(xintercept = 0, 
                                                     linetype = "dotted", 
                                                     colour = "black") +
                                          labs(x = "Benchmark Log Return (%)", 
                                               y = "Portfolio Log Return (%)", 
                                               title = "Portfolio vs. Benchmark") +
                                          theme(panel.border = element_rect(colour = "black", 
                                                                            fill   = NA), 
                                                legend.position = "none", 
                                                plot.title = element_text(hjust = 0.5, 
                                                                          size = 18),
                                                panel.background = element_blank()) +
                                          annotate("text", 
                                                   x = min(benchRet), 
                                                   y = max(portRet), 
                                                   label = c(paste0("  Robust Alpha: ",    
                                                                    annualized_alpha, "% / ann.\n"),
                                                             paste0("\n  Robust Beta:  ", 
                                                                    robust_beta) ), 
                                                   hjust = 0, vjust = 1) +
                                          geom_abline(aes(slope = robust_slope, 
                                                          intercept = annualized_alpha / 12, 
                                                          color = "red4"), linewidth = 1) 
      
              
              # Create a title for the page
              ptf_vs_bmk <- paste0(ptfName, " vs. ", bmkName, "\n", 
                                   index(df_xts)[1], " to ", tail(index(df_xts), 1))
              page_title <- text_grob(ptf_vs_bmk, size = 15, face = "bold")
              
              # View graphs
              if(print_to_screen){
                grid.arrange(cusum_active_log_ret, cusum_log_ir, 
                             lr_test, ptf_vs_bmk_scatter_plot, 
                             layout_matrix = rbind(c(1, 2), c(3, 4)),
                             top = page_title)
              }
              
              
              # Save plots to pdf
              if(print_to_pdf){
                pdf(pdf_fn, width = 8, height = 8)
                  grid.arrange(cusum_active_log_ret, cusum_log_ir, 
                               lr_test, ptf_vs_bmk_scatter_plot, 
                               layout_matrix = rbind(c(1, 2), c(3, 4)), 
                               top = page_title)
                dev.off()
              }        
      
              # Save plots to png
              if(print_to_png){
                png(filename = png_fn, width = 8, height = 8, units = "in",
                    pointsize = 12, bg = "white", res = 144, 
                    family = "", restoreConsole = TRUE,
                    type = c("windows", "cairo", "cairo-png"), antialias = "d")
                
                  grid.arrange(cusum_active_log_ret, cusum_log_ir, 
                               lr_test, ptf_vs_bmk_scatter_plot, 
                               layout_matrix = rbind(c(1, 2), c(3, 4)), 
                               top = page_title)
                dev.off()
              }
      },
      
      
      invisible()
    )

    
    if (select_option == 0 || length(select_option.vec) == 1) {
      break
    }
    if (length(select_option.vec) > 1) {
      select_option.vec <- select_option.vec[-1]
      select_option <- select_option.vec[1]
      par(ask = TRUE)
    }
    else {
      select_option <- NULL
    }
  }
  par(ask = FALSE)
}



# Compute the CUSUM recursion and all the elements needed for the CUSUM plots
cusumActMgr <- function(portfolioName, benchmarkName, return_df,
                        upper_IR = 0.5,  lower_IR = 0,
                        lambda_in = 0.9, lambda_out = 0.8,
                        winsorize = 4,   filterStd = FALSE) {
  
  # Record the call as an element to be returned
  this.call <- match.call()
  
  # Check to ensure that all arguments are valid
  if (missing(return_df) || !is.xts(return_df)) {
    stop("Invalid args: return_df must be an xts object")
  }
  
  if (missing(portfolioName) || !is.character(portfolioName)) {
    stop("Invalid args: portfolioName must be a character string")
  }
  
  if (missing(benchmarkName) || !is.character(benchmarkName)) {
    stop("Invalid args: benchmarkName must be a character string")
  }
  
  if (winsorize < 1) {
    stop("Invalid args: the threshold for winsorization (winsorize) should be > 1")
  }
  
  if (lambda_in < 0 || lambda_in > 1 || lambda_out < 0 || lambda_out > 1) {
    stop("Invalid args: both lambdas must lie between 0 and 1")
  }
  
  if (!is.logical(filterStd)) {
    stop("Invalid args: filterStd must be a logical value")
  }
  
  # Obtain the returns of the porttolio and the benchmark
  portfolioReturns <- return_df[, portfolioName]
  benchmarkReturns <- return_df[, benchmarkName]
  n <- length(portfolioReturns)
  
  if (n < 2) {
    stop("Invalid args: portfolio returns and benchmark returns must have length >= 2")
  }
  
  if (n != length(benchmarkReturns)) {
    stop("Invalid args: portfolio returns and benchmark returns must have the same length")
  }
  
  # Initialize logarithmic active returns, IR, Lindley's Recursion and TE
  prior_month <- as.yearmon(first(index(portfolioReturns))) - 1 / 12
  all_Months  <- c(prior_month, index(portfolioReturns))
  Lindley     <- xts(rep(0, n + 1), order.by = all_Months)
  
  # Compute the Logarithmic Active Returns
  logActiveReturns <- log((1 + portfolioReturns) / (1 + benchmarkReturns))
  
  # Compute the current mean return, as well as the 
  # unfiltered and filtered Std. Dev. 
  # of the portfolio, its benchmark and the active return
  Means <- matrix(0, ncol = 3, nrow = n + 1)
  uStds <- Means
  fStds <- Means
  
  Means[1, 1] <- ifelse(n >= 11, mean(portfolioReturns[1:11]), mean(portfolioReturns))
  Means[1, 2] <- ifelse(n >= 11, mean(benchmarkReturns[1:11]), mean(benchmarkReturns))
  Means[1, 3] <- ifelse(n >= 11, mean(logActiveReturns[1:11]), mean(logActiveReturns))
  
  uStds[1, 1] <- ifelse(n >= 6, 1.25 * median(abs(portfolioReturns[1:6])),
                        1.25 * median(abs(portfolioReturns)) )
  uStds[1, 2] <- ifelse(n >= 6, 1.25 * median(abs(benchmarkReturns[1:6])),
                        1.25 * median(abs(benchmarkReturns)) )
  uStds[1, 3] <- ifelse(n >= 6, 1.25 * median(abs(logActiveReturns[1:6])),
                        1.25 * median(abs(logActiveReturns)) )
  
  fStds[1, 1] <- uStds[1, 1]
  fStds[1, 2] <- uStds[1, 2]
  fStds[1, 3] <- uStds[1, 3]
  
  # Update the means and unfiltered standard deviations for the portfolio and benchmark
  for (i in 1:n) {
    Means[i + 1, 1] <- muEst(coredata(portfolioReturns[i]), 
                             Means[i, 1], uStds[i, 1], 
                             winsorize, lambda_in)
    Means[i + 1, 2] <- muEst(coredata(benchmarkReturns[i]), 
                             Means[i, 2], uStds[i, 2], 
                             winsorize, lambda_in)
    Means[i + 1, 3] <- muEst(coredata(logActiveReturns[i]), 
                             Means[i, 3], uStds[i, 3], 
                             winsorize, lambda_in)
    
    uStds[i + 1, 1] <- sigmaEst(coredata(portfolioReturns[i]), 
                                Means[i + 1, 1], uStds[i, 1], 
                                winsorize, lambda_in, lambda_out )
    uStds[i + 1, 2] <- sigmaEst(coredata(benchmarkReturns[i]), 
                                Means[i + 1, 2], uStds[i, 2], 
                                winsorize, lambda_in, lambda_out )
    uStds[i + 1, 3] <- sigmaEst(coredata(logActiveReturns[i]), 
                                Means[i + 1, 3], uStds[i, 3], 
                                winsorize, lambda_in, lambda_out )
  }
  
  Stds <- uStds
  
  if (filterStd) {
    # Filter the standard deviations - allow it to rise immediately, 
    # but average the past value and the current estimate when falling
    for (i in 1:n) {
      fStds[i + 1, 1] <- ifelse(uStds[i + 1, 1] > uStds[i, 1], 
                                uStds[i + 1, 1], 0.5 * (uStds[i, 1] + uStds[i + 1, 1]) )
      fStds[i + 1, 2] <- ifelse(uStds[i + 1, 2] > uStds[i, 2], 
                                uStds[i + 1, 2], 0.5 * (uStds[i, 2] + uStds[i + 1, 2]) )
      fStds[i + 1, 3] <- ifelse(uStds[i + 1, 3] > uStds[i, 3], 
                                uStds[i + 1, 3], 0.5 * (uStds[i, 3] + uStds[i + 1, 3]) )
    }
    Stds <- fStds
  }
  
  Means <- xts(Means, order.by = all_Months)
  Stds  <- xts(Stds,  order.by = all_Months)
  colnames(Means) <- colnames(Stds) <- c("Portfolio", "Benchmark", "Active")
  
  # Excess volatility: difference between vol of portfolio and benchmark
  xsVol <- matrix(0, ncol = 3, nrow = n + 1)
  xsVol[1, 1] <- sqrt(12) * sd(coredata(portfolioReturns))
  xsVol[1, 2] <- sqrt(12) * sd(coredata(benchmarkReturns))
  xsVol[2:(n + 1), ] <- Stds[2:(n + 1), ] * sqrt(12)
  xsVol[, 3] <- xsVol[, 1] - xsVol[, 2]
  xsVol      <- xts(xsVol, order.by = all_Months)
  colnames(xsVol) <- c("PortfolioVol", "BenchmarkVol", "ExcessVol")
  
  # Average level of the upper and lower IR inputs, used for Lindley's recursion
  avg_IR_Level <- 0.5 * (upper_IR + lower_IR) / sqrt(12)
  
  ##### Begin looping through the new returns#####
  IR <- coredata(logActiveReturns) / coredata(Stds[-(n + 1), 3])
  IR <- xts(IR, order.by = index(portfolioReturns))
  
  for (i in 1:length(portfolioReturns)) {
    # Lindley's recursion applied to the sequence of estimated log IRs
    # If it exceeds the decision threhsold, reset it to 0
    Lindley[i + 1] <- ifelse(coredata(Lindley[i]) - coredata(IR[i]) + avg_IR_Level < 0, 0,
                             ifelse(coredata(Lindley[i]) > 6.66, max(0, avg_IR_Level - IR[i]),
                                    coredata(Lindley[i]) - coredata(IR[i]) + avg_IR_Level ))
  }
  
  # 12 month moving average returns
  AMA <- xts(rep(0, n), order.by = index(portfolioReturns))
  for (i in 1:n) {
    AMA[i] <- ifelse(i < 12, mean(logActiveReturns[1:i]), mean(logActiveReturns[(i - 11):i]))
  }
  
  # CUSUM IR
  cusum_IR <- xts(cumsum(coredata(IR)), order.by = index(IR))
  
  # Information obtained from Annualized Log IR
  annualized_IR <- sqrt(12) * coredata(cusum_IR)
  lowerLim_IR   <- min(annualized_IR)
  upperLim_IR   <- max(annualized_IR)
  
  spread_IR <- upperLim_IR - lowerLim_IR
  avg_IR    <- spread_IR / n
  
  upperPos_IR <- which.max(annualized_IR)
  lowerPos_IR <- which.min(annualized_IR)
  
  med_IR  <- lowerLim_IR + 0.5 * spread_IR
  peak_IR <- spread_IR / (upperPos_IR - lowerPos_IR)
  max_IR  <- 0.5 * ceiling(abs(peak_IR) + abs(avg_IR))
  
  protractor_width_IR  <- ceiling(0.9 * spread_IR / (2 * max_IR))
  protractor_height_IR <- abs(protractor_width_IR * max_IR)
  
  # There are 7 rays in the IR protractor
  Rays_IR       <- matrix(0, ncol = 7, nrow = n + 1)
  Rays_IR[1, 1] <- med_IR + protractor_height_IR
  for (j in 2:7) {
    Rays_IR[1, j] <- Rays_IR[1, 1] - (j - 1) * protractor_height_IR / 3
  }
  
  for (i in 2:(n + 1)) {
    for (j in 1:4) {
      Rays_IR[i, j] <- max(
        Rays_IR[i - 1, j] - ((Rays_IR[1, j] - med_IR) / protractor_width_IR),
        med_IR
      )
    }
    for (j in 5:7) {
      Rays_IR[i, j] <- min(
        Rays_IR[i - 1, j] - ((Rays_IR[1, j] - med_IR) / protractor_width_IR),
        med_IR
      )
    }
  }
  
  IR_slopes <- c()
  for(i in -3:3){
    if (i != 0) {
      IR_slopes <- append(IR_slopes, (i/3) * max_IR)
    }
    else {
      IR_slopes <- append(IR_slopes, 0)
    }
  }
  
  Rays_IR <- xts(Rays_IR, order.by = all_Months)
  colnames(Rays_IR) <- c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")
  
  # CUSUM of Log Active Returns
  cusum_Log_AR <- xts(100 * cumsum(coredata(logActiveReturns)), 
                      order.by = index(logActiveReturns))
  
  # Information obtained from annualized active returns
  annualized_Log_AR <- 12 * coredata(cusum_Log_AR)
  annualized_Log_AR <- 12 * coredata(cusum_Log_AR)
  lowerLim_Log_AR   <- min(annualized_Log_AR)
  upperLim_Log_AR   <- max(annualized_Log_AR)
  
  spread_Log_AR <- upperLim_Log_AR - lowerLim_Log_AR
  avg_Log_AR    <- spread_Log_AR / n
  
  upperPos_Log_AR <- which.max(annualized_Log_AR)
  lowerPos_Log_AR <- which.min(annualized_Log_AR)
  
  med_Log_AR  <- lowerLim_Log_AR + 0.5 * spread_Log_AR
  peak_Log_AR <- spread_Log_AR / (upperPos_Log_AR - lowerPos_Log_AR)
  max_Log_AR  <- 0.5 * ceiling(abs(peak_Log_AR) + abs(avg_Log_AR))
  protractor_width_Log_AR  <- ceiling(0.9 * spread_Log_AR / (2 * max_Log_AR))
  protractor_height_Log_AR <- abs(protractor_width_Log_AR * max_Log_AR)
  
  # There are 7 rays in the Active Return protractor
  Rays_Log_AR       <- matrix(0, ncol = 7, nrow = n + 1)
  Rays_Log_AR[1, 1] <- med_Log_AR + protractor_height_Log_AR
  for (j in 2:7) {
    Rays_Log_AR[1, j] <- Rays_Log_AR[1, 1] - (j - 1) * protractor_height_Log_AR / 3
  }
  
  for (i in 2:(n + 1)) {
    for (j in 1:4) {
      Rays_Log_AR[i, j] <- max(
        Rays_Log_AR[i - 1, j] - ((Rays_Log_AR[1, j] - med_Log_AR) / protractor_width_Log_AR),
        med_Log_AR
      )
    }
    for (j in 5:7) {
      Rays_Log_AR[i, j] <- min(
        Rays_Log_AR[i - 1, j] - ((Rays_Log_AR[1, j] - med_Log_AR) / protractor_width_Log_AR),
        med_Log_AR
      )
    }
  }
  
  Rays_Log_AR           <- xts(Rays_Log_AR, order.by = all_Months)
  colnames(Rays_Log_AR) <- c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")
  Log_AR_Slopes <- c()
  for (i in -3:3){
    if (i != 0) {
      Log_AR_Slopes <- append(Log_AR_Slopes, (i/3) * max_Log_AR)}
    else
    {
      Log_AR_Slopes <- append(Log_AR_Slopes, 0)
    }
  }
  # Convert all the remaining matrices, vectors etc. to xts objects
  annualized_IR <- xts(annualized_IR, order.by = index(portfolioReturns))
  annualized_Log_AR <- xts(annualized_Log_AR, order.by = index(portfolioReturns))
  
  # Return the updated likelihood ratios exceeding the threshold
  return(list(
    "Log_Active_Returns" = logActiveReturns,
    "Annual_Moving_Average" = AMA,
    "Tracking_Error" = Stds[, 3],
    "Information_Ratios" = IR,
    "Lindleys_recursion" = Lindley,
    "Annualized_Cusum_Log_IR" = annualized_IR,
    "Annualized_Cusum_Log_AR" = annualized_Log_AR,
    "Means" = Means,
    "Protractor_IR" = Rays_IR,
    "Protractor_Log_AR" = Rays_Log_AR,
    "Standard_Deviations" = Stds,
    "Excess_Volatility" = xsVol,
    "Log_AR_Slopes" = round(Log_AR_Slopes,2),
    "IR_Slopes" = round(IR_slopes, 2)
  ))
}


# Simulation for ARL's, too slow for really good confidence intervals, do it in Python or C instead
simulateARL <- function(mu, Threshold, delta, k = 3, lambda = 0.9, Fixed_Sigma = 1) {
  N_Events <- 0
  Sum <- 0
  SumSq <- 0
  ThreeSigmaOverMu <- delta + 1
  while (ThreeSigmaOverMu > delta) {
    L <- 0
    N <- 0
    r_n_1 <- 0
    sigma_n <- 1
    sigma_n_1 <- 1
    while (L < Threshold) {
      r_n <- rnorm(n = 1, mean = mu, sd = 1)
      if ((N > 1) && (Fixed_Sigma == 1)) {
        sigma_n <- sqrt(lambda * sigma_n_1^2 + (1 - lambda) * 0.5 * (r_n - r_n_1)^2)
      }
      else {
        sigma_n <- 1
      }
      IR_hat <- r_n / sigma_n_1
      sigma_n_1 <- sigma_n
      r_n_1 <- r_n
      L <- max(0, L - IR_hat + mean(mu))
      N <- N + 1
    }
    N_Events <- N_Events + 1
    Sum <- Sum + N
    SumSq <- SumSq + N^2
    ARL <- Sum / N_Events
    Sigma <- ifelse(N_Events < 10, 1e5, sqrt((SumSq - Sum^2 / N_Events) / (N_Events * (N_Events - 1))))
    ThreeSigmaOverMu <- k * Sigma / ARL
  }
  s <- Sigma * sqrt(N_Events)
  return(c(ARL, s))
}
