# File:   
# 


# INSTALL AND LOAD PACKAGES ################################
#sessionInfo()
#Sys.getlocale()

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,tidyverse,corrplot,ggthemes) 



# MPT Packages #############################################
suppressMessages(library(PortfolioAnalytics))
suppressMessages(library(foreach))
suppressMessages(library(iterators))
suppressMessages(library(ROI))
suppressMessages(library(ROI.plugin.quadprog))
suppressMessages(library(ROI.plugin.glpk))
# suppressMessages(library(DEoptim))  # For non-convex optimization
# suppressMessages(library(GenSA))    # Simulated annealing for global optimization



# Basic functions ##########################################
simple_message        <- function(msg){return(cat(paste("","ℹ",msg),"\n"))}
successful_message    <- function(msg){return(cat(paste("\033[0;32m","","✔",msg,"\033[0m"),"\n\n"))}
warning_message       <- function(msg){return(cat(paste("\033[0;33m","","!",msg,"\033[0m"),"\n\n"))}
error_message         <- function(msg){return(cat(paste("\033[0;31m","","ⓧ",msg,"\033[0m"),"\n\n"))}
plot_title <- function(text,textsize=9,fill="gray13") { return(ggplot()+geom_text(aes(x=0,y=0,label=text),size=textsize)+theme_void()+theme(plot.background=element_rect(fill=fill,colour=fill))) }
# MPT Customized Functions #################################
MPT_asset_price_action <- function(asset_input){
  df <- extract_data %>% filter(asset==asset_input) %>% mutate(varln=log(price/lag(price))) %>% select(date,varln)
  if(nrow(df)==0){
    warning_message(paste("Asset not found",asset_input))
  }
  colnames(df) <- c("date",asset_input)
  return(df)
}
MPT_asset_returns <- function(asset_list=c("PETR4","VALE3","ITUB4","ELET3"),replace_NA=NA){
  df <- MPT_asset_price_action(asset_list[1])
  if(length(asset_list)>1) for(i in 2:length(asset_list)) { df <- df %>% left_join(MPT_asset_price_action(asset_list[i]),by="date") }
  if(!is.na(replace_NA)){ for(c in colnames(df %>% select(-c("date")))){ df[is.na(df[,c]),c]=replace_NA }
  } else { for(c in colnames(df)){ df=df[!is.na(df[,c]),] } }# Remove NAs 
  tryCatch({
    corrplot(round(cor(
      df %>% select(-c("date"))),2), 
      method = "color", 
      addCoef.col="navy", 
      order = "AOE", 
      number.cex=0.975)
  }, error=function(e) { warning_message(e) })
  df <- zoo(df[,!names(df) %in% c("date")] %>% as.matrix(),df$date)
  print(head(df, 10))
  simple_message(paste("Assets = ",paste(colnames(df),collapse=" : ")))
  return(df)
}


# Load price data ##########################################
extract_data <- readRDS("data/extract_data.rds")



# MPT execution with simple parameters #####################
returns <- MPT_asset_returns(c("PETR4","VALE3","ITUB4","WEGE3","ABEV3","AZUL4","MGLU3","ASAI3","CSAN3","USIM5")) # leaders and laggards
funds <- colnames(returns) # vector of assets in the process
#returns <- returns[index(returns)>"2022-07-01",]

portf_test <- portfolio.spec(assets=funds)
portf_test <- add.constraint(portfolio=portf_test,type="full_investment")       # Add full investment constraint to the portfolio object
portf_test <- add.constraint(portfolio=portf_test,type="long_only")             # Long-Only Constraint (no short selling)
portf_test <- add.constraint(portfolio=portf_test,type="box",min=0.0,max=0.4)   # Weights min and max
portf_test <- add.objective (portfolio=portf_test,type="risk"    ,name="var")   # Add objective to minimize variance: var / StdDev produce same results
portf_test <- add.objective (portfolio=portf_test,type="return"  ,name="mean")  # Add objective to maximize returns
portf_test
opt_test <- optimize.portfolio(R=returns, portfolio=portf_test,optimize_method="ROI",trace=T) # 
opt_test
plot(opt_test,chart.assets=TRUE,xlim=c(-0.01, 0.22))
chart.RiskReward(opt_test,risk.col="ES",return.col="mean",chart.assets=TRUE,xlim=c(-0.01, 0.22))



# MPT backtest rebalancing #################################
bt_test <- optimize.portfolio.rebalancing(
  R=returns,
  portfolio=portf_test,
  optimize_method="ROI",
  rebalance_on="quarters",# days months quarters
  training_period=290) # training_period 200..900
bt_test
bt_test.r <- Return.portfolio(R=returns,weights=extractWeights(bt_test))
colnames(bt_test.r) <- "optimize"
charts.PerformanceSummary(bt_test.r)
chart.Weights(bt_test)



#





# END ######################################################





