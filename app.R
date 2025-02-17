#
#
# source("app.R")

# INSTALL AND LOAD PACKAGES ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,shiny,shinyjs,shinydashboard,dplyr,tidyr,ggplot2,gridExtra,grid,ggthemes) 


# Install MPT packages #####################################
suppressMessages(library(PortfolioAnalytics))
suppressMessages(library(foreach))
suppressMessages(library(iterators))
suppressMessages(library(ROI))
suppressMessages(library(ROI.plugin.quadprog))
suppressMessages(library(ROI.plugin.glpk))
library(tseries)
library(corrplot)
library(reshape2)





# Basic functions ##########################################
simple_message        <- function(msg){return(cat(paste("","ℹ",msg),"\n"))}
successful_message    <- function(msg){return(cat(paste("\033[0;32m","","✔",msg,"\033[0m"),"\n\n"))}
warning_message       <- function(msg){return(cat(paste("\033[0;33m","","!",msg,"\033[0m"),"\n\n"))}
error_message         <- function(msg){return(cat(paste("\033[0;31m","","ⓧ",msg,"\033[0m"),"\n\n"))}
plot_title <- function(text,textsize=9,fill="gray13") { return(ggplot()+geom_text(aes(x=0,y=0,label=text),size=textsize)+theme_void()+theme(plot.background=element_rect(fill=fill,colour=fill))) }



# Assets groups ############################################
read_IBOV_file <- function(){return(read_B3_index_file("data/IBOVDia_06-01-25.csv"))} 
read_B3_index_file <- function(filename){ 
  if(!file.exists(filename)){
    print(paste("File does not exists",filename))
    return(NULL)
  }
  df <- NULL
  tryCatch({df <- suppressWarnings(read.csv(filename,sep=";",header=F) %>% filter(V3!="",V3!="Tipo")) %>% rename(asset=V1,share=V5) %>% select(asset,share)})
  return(df)
}




# read_mt5_file_path <- function(filename,path){
#   filename_path=file.path(path,filename)
#   if(!file.exists(filename_path)) return(warning_message(paste("File does not exists",filename_path)))
#   simple_message(paste("Reading from",filename,"updated on",format(file.info(filename_path)$mtime,"%Y-%m-%d %H:%M")))
#   extract_data <- import(filename_path)
#   if(!ncol(extract_data)>=16) extract_data$V16 <- sapply(strsplit(as.character(extract_data$V2)," ",fixed=T),`[`,2)
#   extract_data <- extract_data %>% 
#     # mutate(V16=ifelse(ncol(extract_data)>=16,V16,sapply(strsplit(as.character(V2)," ",fixed=T),`[`,2))) %>% 
#     mutate(V2=as.Date(as.character(V2),format="%Y%m%d"))
#   
#   # strsplit()
#   colnames(extract_data) = c("asset","date","price","open","high","low","volume_tick","volume_real","d1","d2","d3","atr","f1","f2","f3","hour")
#   extract_data <- extract_data %>% arrange(date) %>% 
#     filter(!(year(date)<2019 & asset %in% c("JHSF3","SIMH3"))) %>% 
#     group_by(asset) %>% arrange(date) %>% mutate(close_ln=log(price),var_ln=log(price/lag(price)),r_ln=close_ln,atr=ifelse(atr>1000,0,atr)) %>% ungroup()
#   print(paste(extract_data %>% distinct(date) %>% nrow(),"distinct dates from",min(extract_data$date),"to",max(extract_data$date)))
#   print(extract_data %>% group_by(year=year(date)) %>% summarise(N=n()) %>% column_to_rownames(var="year") %>% t())
#   print(extract_data %>% group_by(date) %>% summarise(N=n()) %>% tail(5) %>% column_to_rownames(var="date") %>% t())
#   print(select(assets,asset,extraction_id,IBXL,IBOV,IBXX,SMLL) %>% inner_join(distinct(extract_data,asset),by="asset") %>% 
#           group_by(IBXL,IBOV,IBXX,SMLL) %>% summarise(N=n()) %>% as.data.frame() %>% 
#           summarise(IBXL=sum(N[IBXL]),IBOV=sum(N[IBOV]),IBXX=sum(N[IBXX]),SMLL=sum(N[SMLL]),N=sum(N)))
#   missing_df <- select(assets,asset,extraction_id,IBXL,IBOV,SMLL) %>% anti_join(select(extract_data,asset),by="asset")
#   if(nrow(missing_df)){
#     print(paste("There are",nrow(missing_df),"missing assets"))
#     # print(missing_df %>% group_by(IBXL,IBOV,SMAL) %>% summarise(N=n()) %>% as.data.frame())
#   }
#   missing_df <- select(assets,asset,extraction_id,IBXL,IBOV,IBBR,SMLL) %>% filter(IBBR) %>% inner_join(select(extract_data %>% distinct(asset),asset),by="asset") %>% anti_join(select(extract_data %>% filter(date==max(date)),asset),by="asset")
#   if(nrow(missing_df)){
#     #warning(paste("There are",nrow(missing_df),"missing assets",missing_df %>% select(asset)))
#     #print(missing_df)
#     print(paste("There are",nrow(missing_df),"missing assets from the last date:",toString(missing_df$asset)))
#     print(missing_df %>% group_by(IBXL,IBOV,SMLL) %>% summarise(N=n()) %>% as.data.frame())
#   }
#   return(extract_data)
# }
# library(tidyverse)
# library(rio)
# assets <- import(file.path("~/Zion","__R25a_assets.txt")) %>% as_tibble()
# extract_data <- read_mt5_file_path("__extracted_data_R25a_20250112.txt","~/Zion")
# #extract_ETF <- read_mt5_file_path("__extracted_data_R25a_ETF_20250111.txt","~/Zion")
# extract_data %>% filter(!asset %in% c(unique(extract_ETF$asset))) %>%
#   #rbind(extract_ETF) %>%
#   select(asset,date,price) #%>% saveRDS("data/extract_data.rds")
# Cache prices #############################################
simple_message("Loading prices")
tryCatch({ extract_data <- readRDS("data/extract_data.rds") },
error=function(e){message(paste("ERROR",e))},
finally={}
)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
yahoo_data_daily_prefix="y_prices_"
y_data_filename <- function(date,EOD=(hour(Sys.time())>17)){
  prefix=yahoo_data_daily_prefix
  if(EOD) prefix=paste0(prefix,"EOD_")
  return(paste0(prefix,format(date,"%Y%m%d"),".txt")) 
}
cache_yahoo_prices <- function(asset_list,SA_BR=T){ 
  return_df <- NULL 
  # Clear old cache files
  for(i in 1:30){suppressWarnings(file.remove(y_data_filename(Sys.Date()-10-i,F)))}
  for(i in 1:30){suppressWarnings(file.remove(y_data_filename(Sys.Date()-10-i,T)))}
  y_filename=y_data_filename(Sys.Date())
  if(file.exists(y_filename)){
    print(paste("Reading from",y_filename))
    return_df <- import(y_filename) %>% mutate(date=as.Date(date))
    asset_list <- asset_list %>% anti_join(return_df %>% distinct(asset),by="asset")
  }
  if(length(asset_list)==0) return(return_df)
  for(i in 1:length(asset_list)){
    asset_name=asset_list[i]
    print(paste("Downloading",i,asset_name,"at",Sys.time()))
    tryCatch({
      df <- tq_get(paste0(asset_name,ifelse(SA_BR,".SA","")),from='2019-01-01',to=today()+1,get="stock.prices") %>% 
        mutate(adjust_gap=close-adjusted,open=open-adjust_gap,high=high-adjust_gap,low=low-adjust_gap) %>% 
        rename(price=adjusted,traded_price=close) %>% mutate(asset=asset_name)
      return_df <- return_df %>% rbind(df)
    },
    error=function(e){message(paste("ERROR",e))},
    finally={}
    )
  }
  if(nrow(return_df %>% filter(is.na(price)))>0){
    for(i in 1:5) if(file.exists(y_data_filename(Sys.Date()-i,T))) break;
    previous_filename=y_data_filename(Sys.Date()-i,T)
    if(file.exists(previous_filename)){
      previous_df <- import(previous_filename) %>% mutate(date=as.Date(date))
      return_df <- suppressWarnings(return_df %>% anti_join(return_df %>% filter(is.na(price)) %>% distinct(date,asset)) %>% 
                                      full_join(previous_df %>% right_join(return_df %>% filter(is.na(price)) %>% distinct(date,asset))) %>% arrange(asset,date))
    }
  }
  return_df <- return_df %>% group_by(asset) %>% arrange(date) %>% mutate(open=ifelse(open==0,lag(open),open),high=ifelse(high==0,lag(high),high),low=ifelse(low==0,lag(low),low)) %>% ungroup()
  return_df %>% export(y_filename,sep=";")
  return(return_df)
}
if(!exists("extract_data")) extract_data <- cache_yahoo_prices(asset_list=c('BOVA11','SMAL11','ALOS3','ABEV3','ASAI3','AURE3','AMOB3','AZUL4','AZZA3','B3SA3','BBSE3','BBDC3','BBDC4','BRAP4','BBAS3','BRKM5','BRAV3','BRFS3','BPAC11','CXSE3','CRFB3','CCRO3','CMIG4','COGN3','CPLE6','CSAN3','CPFE3','CMIN3','CVCB3','CYRE3','ELET3','ELET6','EMBR3','ENGI11','ENEV3','EGIE3','EQTL3','FLRY3','GGBR4','GOAU4','NTCO3','HAPV3','HYPE3','IGTI11','INBR32','IRBR3','ISAE4','ITSA4','ITUB4','JBSS3','KLBN11','RENT3','LREN3','LWSA3','MGLU3','POMO4','MRFG3','BEEF3','MRVE3','MULT3','ROXO34','PCAR3','PAGS34','PETR3','PETR4','RECV3','PRIO3','PETZ3','PSSA3','RADL3','RAIZ4','RDOR3','RAIL3','SBSP3','SANB11','STBP3','SMTO3','CSNA3','SLCE3','STOC31','SUZB3','TAEE11','VIVT3','TIMS3','TOTS3','UGPA3','USIM5','VALE3','VAMO3','VBBR3','VIVA3','WEGE3','XPBR31','YDUQ3'))



# Modern Portfolio Theory ##################################
simple_message("Loading MPT functions")
returns <- NULL
assign("PMPT_rebal",NULL,envir=.GlobalEnv)
assign("PMPT_returns",NULL,envir=.GlobalEnv)
# MPT_asset_price_action <- function(asset_input){
#   df <- extract_data %>% filter(asset==asset_input) %>% mutate(varln=log(price/lag(price))) %>% select(date,varln)
#   if(nrow(df)==0){
#     warning_message(paste("Asset not found",asset_input))
#   }
#   colnames(df) <- c("date",asset_input)
#   return(df)
# }
# MPT_asset_returns <- function(asset_list=c("PETR4","VALE3","ITUB4","ELET3")){
#   df <- MPT_asset_price_action(asset_list[1])
#   for(i in 2:length(asset_list)) { df <- df %>% left_join(MPT_asset_price_action(asset_list[i]),by="date") }
#   return(df)
# }
# MPT_prepare_returns <- function(asset_list){
#   if(length(asset_list)==0) return(NULL)
#   simple_message(paste(asset_list,collapse=":"))
#   simple_message("Prepare price")
#   returns <- MPT_asset_returns(asset_list)
#   simple_message("Removing NAs")
#   for(c in colnames(returns)){ returns=returns[!is.na(returns[,c]),] }
#   simple_message("Transforming Time Series")
#   returns <- zoo(returns[,!names(returns) %in% c("date")] %>% as.matrix(),returns$date)
#   simple_message(paste(nrow(returns),"dates since",min(index(returns))))
#   return(returns)
# }
MPT_asset_price_action <- function(asset_input){
  df <- extract_data %>% filter(asset==asset_input) %>% mutate(varln=log(price/lag(price))) %>% select(date,varln)
  if(nrow(df)==0){
    warning_message(paste("Asset not found",asset_input))
  }
  colnames(df) <- c("date",asset_input)
  return(df)
}
MPT_asset_returns <- function(asset_list=c("PETR4","VALE3","ITUB4","ELET3")){
  df <- MPT_asset_price_action(asset_list[1])
  for(i in 2:length(asset_list)) { df <- df %>% left_join(MPT_asset_price_action(asset_list[i]),by="date") }
  
  for(c in colnames(df)){ df=df[!is.na(df[,c]),] } # Remove NAs
  
  corrplot(round(cor(
    df %>% select(-c("date"))),2), 
    method = "color", 
    addCoef.col="navy", 
    order = "AOE", 
    number.cex=0.975)
  
  df <- zoo(df[,!names(df) %in% c("date")] %>% as.matrix(),df$date)
  
  print(head(df, 10))
  
  simple_message(paste("Assets = ",paste(colnames(df),collapse=" : ")))
  
  return(df)
}
MPT_validate_returns_plot <- function(returns){
  if(is.null(returns))                          { return(plot_title("Cannot plot \nasset returns NULL",fill="white"))
  } else if(ncol(returns)>30 | nrow(returns)<60){ return(plot_title(paste("Cannot plot ","\nncol =",ncol(returns),"\nnrow =",nrow(returns)),fill="white"))
  } else { return(NULL) }
}
MPT_create_portfolio <- function(assets_input,constrains_input,objectives_input,box_min=0,box_max=1){
  simple_message("Create portfolio object")
  portf_test <- portfolio.spec(assets=assets_input)
  simple_message(constrains_input)
  if(!is.na(match("full_investment",constrains_input)))
    portf_test <- add.constraint(portfolio=portf_test, type="full_investment") # Add full investment constraint to the portfolio object
  if(!is.na(match("long_only",constrains_input)))
    # portf_test <- add.constraint(portfolio=portf_test,type="box",min=0,max=1)  # Long-Only Constraint (no short selling)
    portf_test <- add.constraint(portfolio=portf_test,type="long_only")  # Long-Only Constraint (no short selling)
  if(!is.na(match("box",constrains_input)))
    portf_test <- add.constraint(portfolio=portf_test,type="box",min=as.double(box_min),max=as.double(box_max)) # Box Constrained Weights
  simple_message(objectives_input)
  if(!is.na(match("risk",objectives_input)))
    portf_test <- add.objective(portfolio=portf_test,type="risk",name="var") # Add objective to minimize variance: var StdDev produce similar results
  if(!is.na(match("ETL",objectives_input)))
    portf_test <- add.objective(portfolio=portf_test,type="risk",name="ETL",arguments=list(p=0.95))  # 95% confidence level (Minimize Expected Tail Loss)
  if(!is.na(match("return",objectives_input)))
    portf_test <- add.objective(portfolio=portf_test,type="return",name="mean") # Add objective to maximize return
  return(portf_test)
}
MPT_portfolio_spec_MVO <- function(funds,box=NULL){
  portf <- portfolio.spec(assets=funds)
  portf <- add.constraint(portfolio=portf, type="full_investment") # Add full investment constraint to the portfolio object
  portf <-  add.objective(portfolio=portf, type="risk"    ,name="var") # Add objective to minimize variance: var / StdDev produce similar results
  portf <-  add.objective(portfolio=portf, type="return"  ,name="mean") # Add objective to Maximize Expected Return – Choose asset weights that maximize the portfolio’s expected return
  if(!is.null(box)) portf <- add.constraint(portfolio=portf,type="box",min=box[1],max=box[2])
  return(portf)
}
MPT_portfolio_spec_MVO_ETL <- function(funds,box=NULL){
  portf <- MPT_portfolio_spec_MVO(funds)
  portf <- add.objective(portfolio=portf,type="risk",name="ETL",arguments=list(p=0.95))  # 95% confidence level (Minimize Expected Tail Loss)
  if(!is.null(box)) portf <- add.constraint(portfolio=portf,type="box",min=box[1],max=box[2])
  return(portf)
}
MPT_portfolio_spec_PMPT <- function(funds){
  portf <- portfolio.spec(assets=funds)
  # portf <- add.constraint(portfolio=portf, type="full_investment") # Add full investment constraint to the portfolio object
  portf <- add.constraint(portfolio=portf,type="weight_sum",min_sum=0.99,max_sum=1.01) # Fix a warning message 
  portf <- add.constraint(portfolio=portf,type="box",min=0,max=1) # 
  portf <-  add.objective(portfolio=portf,type="return"  ,name="mean")
  portf <-  add.objective(portfolio=portf,type="risk"    ,name="SortinoRatio") # ,arguments=list(MAR=0.0004) Assumes MAR of 10% per year daily returns # MAR = risk-free rate → Penalizes underperformance vs. safe assets.
  portf <-  add.objective(portfolio=portf,type="risk"    ,name="ETL",arguments=list(p=0.95))  # 95% confidence level (Minimize Expected Tail Loss)
  # portf <-  add.objective(portfolio=portf,type="return", name="Omega", arguments=list(target=0)) # This fails
  return(portf)
}
MPT_portfolio_performance_plot <- function(returns,rebal=NULL,weights_input=NULL,graph_text="weighted input",plot_each_asset_returns=F){
  constrains_text=""
  objective_text=""
  if(is.null(weights_input)){
    simple_message(paste("Rebalanced portfolio optimization was executed in",round(as.numeric(rebal$elapsed_time,units="mins")),"mins"))
    weights_input <- extractWeights(rebal)
    print(rebal$portfolio$assets)
    funds <- names(rebal$portfolio$asset)
    rebal_returns <- Return.portfolio(R=returns,weights=weights_input)
    
    c <- rebal$portfolio$constraints
    for(i in 1:length(c)){
      if(c[[i]]$type=="box" & min(c[[i]]$min)==0 & max(c[[i]]$max)==1) { constrains_text=paste(constrains_text,"long-only")
      } else if(c[[i]]$type=="box") { constrains_text=paste(constrains_text,"box",paste0("[",min(c[[i]]$min),"..",max(c[[i]]$max),"]"))
      } else { constrains_text=paste(constrains_text,c[[i]]$type) }
    }
    objective_text=paste(sapply(rebal$portfolio$objectives , function(x) x$name),collapse=" | ")
    
  } else {
    print(colnames(weights_input))
    funds <- colnames(weights_input)
    rebal_returns <- Return.portfolio(R=returns,weights=weights_input)
    constrains_text <- graph_text
  }
  colnames(rebal_returns) <- "optimize"
  asset_colors <- c("optimize"="purple3","GOLD11"="gold","HASH11"="orange","QBTC11"="orange","IVVB11"="dodgerblue3","NASD11"="dodgerblue1","XINA11"="red4","BOVA11"="green4","SMAL11"="green2")
  if(any(funds %in% names(asset_colors))) { 
    s_fill <- scale_fill_manual(values=asset_colors)
    s_colors <- scale_color_manual(values=asset_colors)
  } else { 
    s_fill <- scale_fill_brewer(palette="Greys") 
    s_colors <- scale_color_manual(values=c("optimize"="purple3"))
  }
  curve_capital_df <- NULL
  if(plot_each_asset_returns) curve_capital_df <- rbind(data.frame(returns  ) %>% mutate(date=index(returns  )) %>% pivot_longer(cols=-date,names_to="asset",values_to="var"))
  curve_capital_df <- curve_capital_df %>%
    rbind(data.frame(rebal_returns) %>% mutate(date=index(rebal_returns)) %>% pivot_longer(cols=-date,names_to="asset",values_to="var")) %>% as_tibble() %>% mutate(var=(var+1)) %>% filter(date>min(index(rebal_returns))) %>%
    group_by(asset) %>% arrange(date) %>% mutate(cumvar=cumprod(var)) %>% 
    ungroup()
  
  
  
  grid.arrange(
    data.frame(weights_input) %>% mutate(date=index(weights_input)) %>% pivot_longer(cols=-date,names_to="asset",values_to="weight") %>% 
      mutate(bar_label=ifelse((weight>.17),paste0(asset,"\n",sprintf("%.0f%%",weight*100)),NA)) %>% 
      ggplot(aes(factor(format(date,"%Y-%m")),weight))+geom_col(aes(fill=asset),alpha=.5,color="black",group=1,width=1)+
      geom_text(aes(label=bar_label,group=asset),color="black",size=3,alpha=.5,position=position_stack(vjust=.5),angle=90)+ylab("weights")+s_fill+
      theme_minimal()+theme(legend.position="none",axis.text.x=element_blank(),axis.title.x=element_blank()),
    curve_capital_df %>% 
      ggplot(aes(date,cumvar))+geom_line(aes(color=asset,alpha=ifelse(asset=="optimize",1,.2)))+scale_alpha_identity()+
      # annotate(geom="label",x=min(index(rebal_returns))+30,y=max(curve_capital_df$cumvar)*.95,color="gray",hjust=0,label.size=NA,label.r=unit(0,"pt"),label=paste(sapply(rebal$portfolio$constraints, function(x) x$type),collapse=" | "))+
      annotate(geom="label",x=min(index(rebal_returns))+30,y=max(curve_capital_df$cumvar)*.95,color="gray",alpha=.5,hjust=0,label.size=NA,label.r=unit(0,"pt"),label=constrains_text)+
      # annotate(geom="label",x=min(index(rebal_returns))+30,y=max(curve_capital_df$cumvar)*.85,color="gray",hjust=0,label.size=NA,label.r=unit(0,"pt"),label=paste(sapply(rebal$portfolio$objectives , function(x) x$name),collapse=" | "))+
      annotate(geom="label",x=min(index(rebal_returns))+30,y=max(curve_capital_df$cumvar)*.85,color="gray",alpha=.5,hjust=0,label.size=NA,label.r=unit(0,"pt"),label=objective_text)+
      ylab("Optimized Cumulative Return")+s_colors+scale_x_date(position="top")+
      theme_minimal()+theme(legend.position="none",axis.title.y=element_text(color="purple"),axis.title.x=element_blank()),
    heights=c(1,2)
  )
}


# Hierarchical Risk Parity #################################
simple_message("Loading HRP functions")
# assign("rets",NULL,envir=.GlobalEnv)
rets <- NULL
# HRP_compare <- NULL
assign("HRP_compare",NULL,envir=.GlobalEnv)
HRP_portfolio_returns <- function(portfolio_name){
  if(is.na(portfolio_name)) { return(NULL)
  } else if(portfolio_name=="BR_TOP_2017"       ) { df <- na.omit(MPT_asset_returns(c("ITUB4","VALE3","BBDC4","ABEV3","B3SA3","BBAS3","BRFS3","UGPA3","LREN3","VIVT3","CCRO3","BBSE3","RADL3")))
  } else if(portfolio_name=="BR_IBOV"           ) { df <- na.omit(MPT_asset_returns(setdiff(read_IBOV_file()$asset,c("AMOB3","BBDC3","BRAP4","GOAU4","ELET6","ITSA4","PETR3","AZZA3","MGLU3","PRIO3","USIM5","AURE3","IGTI11","ISAE4"))))
  } else if(portfolio_name=="BTC_US_GOLD_CHI"   ) { df <- na.omit(MPT_asset_returns(c("HASH11","QBTC11","QETH11","GOLD11"))) %>% filter(date>"2022-08-05")
  } else if(portfolio_name=="ETF_BR_US_CHI_BTC" ) { df <- na.omit(MPT_asset_returns(c("BOVA11","SMAL11","XINA11","GOLD11","NASD11","HASH11","IVVB11","QBTC11","QETH11","DIVO11","PIBB11","CMDB11","MATB11"))) %>% filter(date>"2022-06-01")
  } else { return(NULL) }
  return(zoo(df[,!names(df) %in% c("date")] %>% as.matrix(),df$date))
}
getIVP <- function(covMat) {
  invDiag <- 1/diag(as.matrix(covMat))
  weights <- invDiag/sum(invDiag)
  return(weights)
}
getClusterVar <- function(covMat, cItems) {
  covMatSlice <- covMat[cItems, cItems]
  weights <- getIVP(covMatSlice)
  cVar <- t(weights) %*% as.matrix(covMatSlice) %*% weights
  return(cVar)
}
getRecBipart <- function(covMat, sortIx) {
  w <- rep(1,ncol(covMat))
  w <- recurFun(w, covMat, sortIx)
  return(w)
}
recurFun <- function(w, covMat, sortIx) {
  subIdx <- 1:trunc(length(sortIx)/2)
  cItems0 <- sortIx[subIdx]
  cItems1 <- sortIx[-subIdx]
  cVar0 <- getClusterVar(covMat, cItems0)
  cVar1 <- getClusterVar(covMat, cItems1)
  alpha <- 1 - cVar0/(cVar0 + cVar1)
  
  # scoping mechanics using w as a free parameter
  w[cItems0] <- w[cItems0] * c(alpha)
  w[cItems1] <- w[cItems1] * c(1-alpha)
  
  if(length(cItems0) > 1) {
    w <- recurFun(w, covMat, cItems0)
  }
  if(length(cItems1) > 1) {
    w <- recurFun(w, covMat, cItems1)
  }
  return(w)
}
# covMat <- read.csv('cov.csv', header = FALSE)
# corMat <- read.csv('corMat.csv', header = FALSE)
# clustOrder <- hclust(dist(corMat), method = 'single')$order
# out <- getRecBipart(covMat, clustOrder)
# out

# function to append missing (I.E. assets not selected) asset names and sort into original order
appendMissingAssets <- function(wts, allAssetNames, wtsDate) {
  absentAssets <- allAssetNames[!allAssetNames %in% names(wts)]
  absentWts <- rep(0, length(absentAssets))
  names(absentWts) <- absentAssets
  wts <- c(wts, absentWts)
  wts <- xts(t(wts), order.by=wtsDate)
  wts <- wts[,allAssetNames]
  return(wts)
}

# 
HRP_portfolio_rebal <- function(rets){
  invVolWts <- list()
  minVolWts <- list()
  hrpWts <- list()
  ep <- endpoints(rets, on =  "months")
  nMonths = 6 # month lookback (6 as per parameters from allocateSmartly)
  nVol = 20 # day lookback for volatility (20 ibid)
  
  for(i in 1:(length(ep)-nMonths)) {
    
    # get returns subset and compute absolute momentum
    retSubset <- rets[c(ep[i]:ep[(i+nMonths)]),]
    retSubset <- retSubset[-1,]
    moms <- Return.cumulative(retSubset)
    # 
    # 
    # Review the way it selects assets
    #
    # 
    # select top performing assets and subset returns for them
    # highRankAssets <- rank(moms) >= 6 # top 5 assets
    highRankAssets <- rank(moms) >= trunc(length(colnames(moms))*.3)+1 # top 30% assets
    posReturnAssets <- moms > 0 # positive momentum assets
    selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
    selectedSubset <- retSubset[,selectedAssets] # subset returns slice
    
    if(sum(selectedAssets)==0) { # if no qualifying assets, zero weight for period
      
      wts <- xts(t(rep(0, ncol(retSubset))), order.by=last(index(retSubset)))
      colnames(wts) <- colnames(retSubset)
      invVolWts[[i]] <- minVolWts[[i]] <- hrpWts[[i]] <- wts
      
    } else if (sum(selectedAssets)==1) { # if one qualifying asset, invest fully into it
      
      wts <- xts(t(rep(0, ncol(retSubset))), order.by=last(index(retSubset)))
      colnames(wts) <- colnames(retSubset)
      wts[, which(selectedAssets==1)] <- 1
      invVolWts[[i]] <- minVolWts[[i]] <- hrpWts[[i]] <- wts
      
    } else { # otherwise, use weighting algorithms
      
      cors <- cor(selectedSubset) # correlation
      volSubset <- tail(selectedSubset, nVol) # 20 day volatility
      vols <- StdDev(volSubset)
      covs <- t(vols) %*% vols * cors
      
      # minimum volatility using portfolio.optim from tseries
      minVolRets <- t(matrix(rep(1, sum(selectedAssets))))
      minVolWt <- portfolio.optim(x=minVolRets, covmat = covs)$pw
      names(minVolWt) <- colnames(covs)
      minVolWt <- appendMissingAssets(minVolWt, colnames(retSubset), last(index(retSubset)))
      minVolWts[[i]] <- minVolWt
      
      # inverse volatility weights
      invVols <- 1/vols 
      invVolWt <- invVols/sum(invVols) 
      invNames <- colnames(invVolWt)
      invVolWt <- as.numeric(invVolWt) 
      names(invVolWt) <- invNames
      invVolWt <- appendMissingAssets(invVolWt, colnames(retSubset), last(index(retSubset)))
      invVolWts[[i]] <- invVolWt
      
      # hrp weights
      clustOrder <- hclust(dist(cors), method = 'single')$order
      hrpWt <- getRecBipart(covs, clustOrder)
      names(hrpWt) <- colnames(covs)
      hrpWt <- appendMissingAssets(hrpWt, colnames(retSubset), last(index(retSubset)))
      hrpWts[[i]] <- hrpWt
    }
  }
  
  # Lastly, the program puts together all of the weights, and adds a cash investment for any period without any investments.
  invVolWts <- round(do.call(rbind, invVolWts), 3) # round for readability
  minVolWts <- round(do.call(rbind, minVolWts), 3)
  hrpWts <- round(do.call(rbind, hrpWts), 3)
  
  return_list <- list()
  return_list$invVolWts <- invVolWts
  return_list$minVolWts <- minVolWts
  return_list$hrpWts <- hrpWts
  return(return_list)
}






# UI #######################################################
simple_message("Loading UI")
ui <- dashboardPage(
  
  dashboardHeader(title="CAPM",dropdownMenu(type = "notifications", badgeStatus = "warning",notificationItem(icon = icon("users"), status = "info","5 new members joined today"),notificationItem(icon = icon("warning"), status = "danger","Resource usage near limit."),notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),status = "success", "25 sales made"),notificationItem(icon = icon("user", lib = "glyphicon"),status = "danger", "You changed your username"))),
  dashboardSidebar(sidebarMenu(
    menuItem("|||"                ,tabName="main_page"     ,icon=NULL),
    menuItem(" Assets collection" ,tabName="assets"        ,icon=NULL),
    menuItem(" Modern Portfolio Theory"             ,tabName="study"         ,icon=icon("education",lib="glyphicon")),
    menuItem(" Hierarchical Risk Parity"            ,tabName="hrp"           ,icon=icon("education",lib="glyphicon")),
    menuItem(" Post-Modern Portfolio Theory"        ,tabName="pmpt"          ,icon=icon("education",lib="glyphicon")),
    menuItem(" Black-Litterman model"               ,tabName="blm"           ,icon=icon("education",lib="glyphicon")),
    menuItem(" Fama–French three-factor model"      ,tabName="fama"          ,icon=icon("education",lib="glyphicon")),
    menuItem("About me"           ,tabName="about_me"      ,icon=NULL)
    # menuItem("  Backtest Analysis",tabName="backtest",icon=suppressMessages(icon("chart-simple",lib="font-awesome"))),
    # menuItem("  Data Mining",tabName="backtest_conditions",icon=suppressMessages(icon("chart-simple",lib="font-awesome"))),
    # menuItem("  Pair Analysis",tabName="pair_analysis",icon=suppressMessages(icon("chart-simple",lib="font-awesome")))
  )),
  dashboardBody(tabItems(
    tabItem("main_page"     ,uiOutput("UI_main_page")),
    tabItem("assets"        ,uiOutput("UI_assets")),
    tabItem("study"         ,uiOutput("UI_mpt_study")),
    tabItem("hrp"           ,uiOutput("UI_hrp_study")),
    tabItem("pmpt"          ,uiOutput("UI_pmpt_study")),
    tabItem("blm"           ,uiOutput("UI_blm_study")),
    tabItem("fama"          ,uiOutput("UI_fama_study")),
    tabItem("about_me"      ,uiOutput("xxxUI_about_me"))
  ))
)


# Server ###################################################
simple_message("Loading server")
server <- function(input, output, session) {
  
  
  
  
  
  # Fama-French Study ##############################################
  output$UI_fama_study <- renderUI({
    fluidRow(
      fluidRow(
        column(1,div()),
        column(10,
               h2("Fama–French three-factor model"),
               p(""),br(),
               p(""),p(style="text-align: right; font-size: 10px;","source: https://"),br(),
               br()
        ),
        column(1,div())
      ),
      
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_fama_study_inputs_panel"))),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_fama_study_graphs"),br(),uiOutput("UI_fama_study_graphs_compare"))),
        column(1,div())
      ),
      h2("...")
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Black-Litterman Study ##############################################
  output$UI_blm_study <- renderUI({
    fluidRow(
      fluidRow(
        column(1,div()),
        column(10,
               h2("Black-Litterman model"),
               p("is a portfolio optimization framework that improves Markowitz’s Mean-Variance Optimization (MVO) by incorporating investor views on asset returns."),br(),
               p("In finance, the Black–Litterman model is a mathematical model for portfolio allocation developed in 1990 at Goldman Sachs by Fischer Black and Robert Litterman. It seeks to overcome problems that institutional investors have encountered in applying modern portfolio theory in practice. The model starts with an asset allocation based on the equilibrium assumption (assets will perform in the future as they have in the past) and then modifies that allocation by taking into account the opinion of the investor regarding future asset performance."),p(style="text-align: right; font-size: 10px;","source: https://en.wikipedia.org/wiki/Black%E2%80%93Litterman_model"),br(),
               p(strong("1. Combines Market Equilibrium (CAPM) with Investor Views"),
                 div(
                   tags$ul(
                     tags$li("Uses implied equilibrium returns derived from a market-capitalization-weighted portfolio."),
                     tags$li("Allows investors to express views (absolute or relative) and integrates them into the model."),
                   ))),
               p(strong("2. Overcomes MVO Issues"),
                 div(
                   tags$ul(
                     tags$li("Reduces extreme weights that arise from pure mean-variance optimization."),
                     tags$li("More stable and intuitive allocation compared to standard MVO."),
                   ))),
               p(strong("3. Mathematical Foundation"),
                 div(
                   tags$ul(
                     tags$li("Bayesian framework updates the prior (equilibrium returns) with investor views to create posterior expected returns."),
                   ))),
               p(strong("4. Typical Use Cases"),
                 div(
                   tags$ul(
                     tags$li("Institutional portfolio management."),
                     tags$li("Asset allocation with subjective inputs."),
                   ))),
               code("print('Hello')")
        ),
        column(1,div())
      ),
      
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_blm_study_inputs_panel"))),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_blm_study_graphs"),br(),uiOutput("UI_blm_study_graphs_compare"))),
        column(1,div())
      ),
      h2("...")
    )
  })
  
  
  
  
  
  
  # PMPT Study ##############################################
  output$UI_pmpt_study <- renderUI({
    fluidRow(
      fluidRow(
        column(1,div()),
        column(10,
               h2("Post-Modern Portfolio Theory (PMPT)"),
               p("PMPT is an extension of Modern Portfolio Theory (MPT) that emphasizes downside risk instead of total risk (variance). It considers measures such as semi-variance, Sortino ratio, Conditional Value-at-Risk (CVaR), and Omega ratio to optimize portfolios."),
               p("The differences in risk, as defined by the standard deviation of returns, between the PMPT and the MPT is the key factor in portfolio construction. The MPT assumes symmetrical risk whereas the PMPT assumes asymmetrical risk. Downside risk is measured by target semi-deviation, termed downside deviation, and captures what investors fear most: having negative returns."),
               p("The Sortino ratio was the first new element introduced into the PMPT rubric by Rom and Ferguson, which was designed to replace MPT’s Sharpe ratio as a measure of risk-adjusted return, and improved upon its ability to rank investment results. Volatility skewness, which measures the ratio of a distribution’s percentage of total variance from returns above the mean to the returns below the mean, was the second portfolio-analysis statistic to be added to the PMPT rubric."),
               p(style="text-align: right; font-size: 10px;","source: https://www.investopedia.com/terms/p/pmpt.asp")),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_pmpt_study_inputs_panel"))),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_pmpt_study_graphs"),br())),
        column(1,div())
      ),
      h2("...")
    )
  })
  output$UI_pmpt_study_inputs_panel <- renderUI({ 
    fluidRow(
      column(6,selectInput("UI_pmpt_study_portfolio", "Choose a portfolio:",
                           c("BR Top market cap 2017"      = "BR_TOP_2017",
                             # "IBOV 2022"                   = "BR_IBOV",
                             "IBOV TOP10"                  = "BR_IBOV_TOP10",
                             "ETF BR+US+CHI+GOLD"          = "ETF_BR_US_GOLD_CHI",
                             "ETF BR+US+CHI+BTC"           = "ETF_BR_US_CHI_BTC"
                           ))),
      column(3,div(style="margin-top: 25px;",actionButton("UI_pmpt_study_load","Load backtest"))),
      column(3,div(style="margin-top: 25px;",dateInput("UI_pmpt_study_rebalancing_start_date",NULL,value="2022-01-01",width="90px"))))
      # column(1,div()))
  })
  observeEvent(input$UI_pmpt_study_load,{
    simple_message(paste("Rebalancing backtest portfolio",input$UI_pmpt_study_portfolio))
    paths_files="data"
    assign("PMPT_rebal",NULL,envir=.GlobalEnv)
    assign("PMPT_returns",NULL,envir=.GlobalEnv)
    switch (input$UI_pmpt_study_portfolio,
            BR_TOP_2017 = {
              returns <- MPT_asset_returns(c("ITUB4","VALE3","BBDC4","ABEV3","PETR4","B3SA3","BBAS3","BRFS3","UGPA3","COGN3","LREN3","VIVT3","CCRO3","BBSE3","RADL3"))
              bt_test <- readRDS(file.path(paths_files,paste0("MPT_portfolio_rebal_PMPT_IBOV_TOP2017_2015_20250203_27m_.rds")))
            },
            BR_IBOV={
              returns <- MPT_asset_returns((extract_data %>% filter(!asset %in% c("ELET6","BBDC3","GOAU4","BRAP4","PETR3","ITSA4")) %>% right_join(read_IBOV_file(),by="asset") %>% group_by(asset) %>% summarise(N=n()) %>% arrange(N) %>% filter(N>1250) %>% arrange(asset))$asset)
              returns <- returns[index(returns)>"2021-08-01",]
              bt_test <- readRDS(file.path(paths_files,paste0("MPT_portfolio_rebal_PMPT_IBOV_20250203_1h14m_.rds")))
            },
            BR_IBOV_TOP10={
              returns <- MPT_asset_returns(c("PETR4","VALE3","ITUB4","ELET3","WEGE3","SBSP3","B3SA3","ABEV3","JBSS3","SUZB3"))
              bt_test <- readRDS(file.path(paths_files,paste0("MPT_portfolio_rebal_PMPT_IBOV_TOP10_2015_20250203_17m_.rds")))
            },
            ETF_BR_US_GOLD_CHI={
              returns <- MPT_asset_returns(c("BOVA11","SMAL11","DIVO11","XINA11","GOLD11","IVVB11"))
              returns <- returns[index(returns)>"2021-08-01",]
              bt_test <- readRDS(file.path(paths_files,paste0("MPT_portfolio_rebal_PMPT_ETFs_BR_US_CHI_GOLD_20250203_2m_.rds")))
            },
            ETF_BR_US_CHI_BTC={
              returns <- MPT_asset_returns(c("BOVA11","SMAL11","XINA11","GOLD11","NASD11","HASH11","IVVB11","QBTC11","QETH11","DIVO11","PIBB11","CMDB11","MATB11"))
              returns <- returns[index(returns)>"2021-08-01",]
              bt_test <- readRDS(file.path(paths_files,paste0("MPT_portfolio_rebal_PMPT_ETFs_BR_US_CHI_CRYPTO_20250203_3m_.rds")))
            }
    )
    assign("PMPT_rebal",bt_test,envir=.GlobalEnv)
    assign("PMPT_returns",returns,envir=.GlobalEnv)
    funds <- colnames(PMPT_returns)
    output$UI_pmpt_study_plot_portfolio1 <- renderText({ funds })
    # hrpWts <- HRP_portfolio_rebal(returns)$hrpWts
    
    output$UI_pmpt_study_graphs <- renderUI({
      fluidRow(
        fluidRow(column(4,h2("Rebalancing")),column(8,textOutput("UI_pmpt_study_plot_portfolio1"))),
        br(),plotOutput("UI_pmpt_study_plot_rebalancing",height="700px"),
        br(),actionButton("UI_pmpt_study_compare","Compare with other methods"),br(),
        br(),uiOutput("UI_pmpt_study_graphs_compare"),
        br()) 
    })
    output$UI_pmpt_study_plot_rebalancing <- renderPlot({
      returns_plot <- returns[index(returns)>input$UI_pmpt_study_rebalancing_start_date]
      MPT_portfolio_performance_plot(returns_plot,PMPT_rebal,plot_each_asset_returns=length(funds)<20)
    })
    
    output$UI_pmpt_study_graphs_compare_weights <- NULL
    output$UI_pmpt_study_graphs_compare_curve <- NULL
    output$UI_pmpt_study_graphs_compare <- renderUI({ div() })
  })
  
  observeEvent(input$UI_pmpt_study_compare,{
    simple_message(paste("Compare PMPT with other optimization methods",input$UI_pmpt_study_portfolio))
    funds <- colnames(PMPT_returns)
    output$UI_pmpt_study_plot_portfolio2 <- renderText({ funds })
    
    # showModal(modalDialog("Running rebalancing with other optimization methods...",footer=NULL))
    hrpWts <- HRP_portfolio_rebal(PMPT_returns)$hrpWts
    
    output$UI_pmpt_study_graphs_compare <- renderUI({
      fluidRow(
        plotOutput("UI_pmpt_study_graphs_compare_weights",height="700px"),br(),br(),
        fluidRow(column(4,h4("Compare optimization methods")),column(8,textOutput("UI_pmpt_study_plot_portfolio2"))),
        plotOutput("UI_pmpt_study_graphs_compare_curve",height="700px"),br())
    })
    output$UI_pmpt_study_graphs_compare_weights <- renderPlot({
      returns_plot <- PMPT_returns[index(PMPT_returns)>input$UI_pmpt_study_rebalancing_start_date]
      grid.arrange(
        MPT_portfolio_performance_plot(returns_plot,weights=hrpWts[,!names(hrpWts) %in% c("cash")],graph_text="HRP optimization"),
        MPT_portfolio_performance_plot(returns_plot,PMPT_rebal),
        MPT_portfolio_performance_plot(returns_plot,optimize.portfolio.rebalancing(R=PMPT_returns,portfolio=MPT_portfolio_spec_MVO    (funds,c(0,.4)),optimize_method="ROI",rebalance_on="quarters",training_period=90)),
        grid.draw(grid::rectGrob(gp = grid::gpar(fill = "white"))),heights=c(1,1,1,0))
    })
    output$UI_pmpt_study_graphs_compare_curve <- renderPlot({
      returns_plot <- PMPT_returns[index(PMPT_returns)>input$UI_pmpt_study_rebalancing_start_date]
      equalWts <- hrpWts[,!names(hrpWts) %in% c("cash")]
      equalWts[] <- 1/length(funds)
      compare <- cbind(
        Return.portfolio(R=returns_plot,weights=equalWts),
        Return.portfolio(R=returns_plot,weights=hrpWts[,!names(hrpWts) %in% c("cash")]),
        Return.portfolio(R=returns_plot,weights=extractWeights(PMPT_rebal)),
        Return.portfolio(R=returns_plot,weights=extractWeights(optimize.portfolio.rebalancing(R=PMPT_returns,MPT_portfolio_spec_MVO    (funds,c(0,.4)),optimize_method="ROI",rebalance_on="quarters",training_period=90))))
      colnames(compare) <- c("Equal","Hierarchical.Risk.Parity","Post.MPT","MVO..MPT")
      compare <- na.omit(compare)
      scale_color_opt_methods <- scale_color_manual(values=c("Hierarchical.Risk.Parity"="black","Post.MPT"="palegreen3","MVO..MPT"="aquamarine4","Equal"="gray89"))
      grid.arrange(
        chart.CumReturns(compare,plot.engine = "ggplot2")+theme_excel_new()+scale_color_opt_methods+labs(color="opt.methods",title="Cumulative Return")+theme(legend.position="top",legend.text = element_text()),
        chart.Drawdown  (compare,plot.engine = "ggplot2")+theme_excel_new()+scale_color_opt_methods+labs(color="opt.methods",title="Drawdown"         )+theme(legend.position="none",axis.text.x=element_blank()),
        heights=c(3,1)
      )
    })
    # removeModal()
  })
  
  
  
  
  
  
  
  
  # HRP Study ##############################################
  output$UI_hrp_study <- renderUI({
    fluidRow(
      fluidRow(
        column(1,div()),
        column(10,
               h2("Hierarchical Risk Parity (HRP)"),
               p("Hierarchical risk parity (HRP) is a portfolio optimization approach that does not require inversion of the covariance matrix. HRP is a more robust way of constructing portfolios. Hierarchical risk parity was first suggested by Marcos Lopez De Prado in 2016."),
               p("To perform a traditional mean- variance optimization, as suggested by Harry Markowitz in 1952, we need to use a covariance matrix. The difficulty is not the covariance matrix itself, but rather that the covariance matrix is hard to estimate in practice. This is because we need a large number of observations to estimate a covariance matrix. To give an example, suppose we have 50 assets (N). In that case, the covariance matrix is 50 x 50, we need to estimate a total of 1225 parameters. Generally, if the number of observations T is lower than N, the covariance cannot be estimated. Worse still, it is not possible to take the inverse. And it is exactly this inverse that is needed to perform a traditional mean-variance optimization."),
               p("To solve the problem, hierarchical risk parity takes another approach. Instead of inverting the covariance matrix, the matrix is analyzed. In particular, hierarchical risk parity applies hierarchical clustering. This means that the securities in the universe are clustered together. After clustering the securities, the covariance matrix is then reordered, such that similar stocks are close to each other. Finally, the portfolio is optimized by performing and inverse volatility weighting approach where risk is allocated to the clusters iteratively."),
               p(style="text-align: right; font-size: 10px;","source: https://breakingdownfinance.com/finance-topics/modern-portfolio-theory/hierarchical-risk-parity/")),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_hrp_study_inputs_panel"))),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_hrp_study_graphs"),br(),uiOutput("UI_hrp_study_graphs_compare"))),
        column(1,div())
      ),
      h2("...")
    )
  })
  output$UI_hrp_study_inputs_panel <- renderUI({ 
    fluidRow(
      fluidRow(
        column(6,selectInput("UI_hrp_study_portfolio", "Choose a portfolio:",
                             c("BR Top market cap 2017"      = "BR_TOP_2017",
                               "IBOV 2022"                   = "BR_IBOV",
                               "Internacional BTC+US+CHI"    = "BTC_US_GOLD_CHI",
                               "ETF BR+US+CHI+BTC"           = "ETF_BR_US_CHI_BTC"
                             ))),
        column(3,div(style="margin-top: 25px;",actionButton("UI_hrp_study_rebalancing"   ,"Backtest"))),
        column(3,div())
      )
      # column(6,checkboxGroupInput("UI_mpt_study_constrains","Constrains:",selected=c("full_investment"),
      #                             choiceNames=list("Full Investment","Long Only","Box Constrained Weights"),
      #                             choiceValues=list("full_investment","long_only","box")),fluidRow(column(6,textInput("UI_mpt_study_constrains_box_min","min",value=0)),column(6,textInput("UI_mpt_study_constrains_box_max","max",value=0.4)))),
      # column(6,checkboxGroupInput("UI_mpt_study_objectives","Objectives:",selected=c("risk"),
      #                             choiceNames=list("Minimize Variance","Maximize Return","Minimize Expected Tail Loss (this can take time with a long window)"),
      #                             choiceValues=list("risk","return","ETL")))),
    )
  })
  observeEvent(input$UI_hrp_study_rebalancing,{
    simple_message(paste("Rebalancing backtest portfolio",input$UI_hrp_study_portfolio))
    rets <- HRP_portfolio_returns(input$UI_hrp_study_portfolio)
    simple_message(paste("dates from",min(index(rets)),"to",max(index(rets))))
    output$UI_hrp_study_plot_portfolio <- renderText({ colnames(rets) })
    output$UI_hrp_study_plot_rebalancing <- renderPlot({
      
      rebal <- HRP_portfolio_rebal(rets)
      invVolWts <- rebal$invVolWts
      hrpWts <- rebal$hrpWts
      minVolWts <- rebal$minVolWts
      
      # allocate to cash if no allocation made due to all negative momentum assets
      invVolWts$cash <- 0; invVolWts$cash <- 1-rowSums(invVolWts)
      hrpWts$cash <- 0; hrpWts$cash <- 1-rowSums(hrpWts)
      minVolWts$cash <- 0; minVolWts$cash <- 1-rowSums(minVolWts)
      # cash value will be zero
      rets$cash <- 0
      
      # compute backtest returns
      invVolRets <- Return.portfolio(R = rets, weights = invVolWts)
      minVolRets <- Return.portfolio(R = rets, weights = minVolWts)
      hrpRets <- Return.portfolio(R = rets, weights = hrpWts)
      
      compare <- cbind(hrpRets,invVolRets,minVolRets)
      colnames(compare) <- c("HRP","invVol","minVol")
      charts.PerformanceSummary(compare)
      assign("HRP_compare",compare,envir=.GlobalEnv)
      assign("HRP_weights",hrpWts ,envir=.GlobalEnv)
      # charts.PerformanceSummary(compare,plot.engine="ggplot2")+theme_minimal()+theme(legend.position="none")
      # rbind(table.AnnualizedReturns(compare), maxDrawdown(compare), CalmarRatio(compare))  
      
    })
    output$UI_hrp_study_graphs <- renderUI({ 
      fluidRow(
        fluidRow(column(4,h2("Rebalancing")),column(8,textOutput("UI_hrp_study_plot_portfolio"))),
        br(),plotOutput("UI_hrp_study_plot_rebalancing",height="700px"),
        br(),actionButton("UI_hrp_study_compare_mpt","Compare with MPT"),
        br()) 
    })
    output$UI_hrp_study_graphs_compare <- renderUI({ fluidRow(div()) })
  })
  observeEvent(input$UI_hrp_study_compare_mpt,{
    
    simple_message(paste("Compare with MPT",input$UI_hrp_study_portfolio))
    rets <- HRP_portfolio_returns(input$UI_hrp_study_portfolio)
    simple_message(paste("dates from",min(index(rets)),"to",max(index(rets))))
    
    showModal(modalDialog("Running rebalancing training...",footer=NULL))
    returns <- rets[,!names(rets) %in% c("cash")]
    funds <- colnames(returns)
    simple_message(paste(funds,collapse=" : "))
    portf_test <- portfolio.spec(assets=funds)
    portf_test <- add.constraint(portfolio=portf_test,type="full_investment") # Add full investment constraint to the portfolio object
    portf_test <- add.constraint(portfolio=portf_test,type="long_only")  # Long-Only Constraint (no short selling)
    # portf_test <- add.constraint(portfolio=portf_test,type="box",min=0.0,max=0.4) #
    portf_test <- add.objective(portfolio=portf_test,type="risk"    ,name="var") # Add objective to minimize variance: var / StdDev produce similar results
    portf_test <- add.objective(portfolio=portf_test,type="risk"    ,name="ETL",arguments=list(p=0.95))  # 95% confidence level (Minimize Expected Tail Loss)
    portf_test <- add.objective(portfolio=portf_test,type="return"  ,name="mean")
    bt_test <-
      optimize.portfolio.rebalancing(R=returns, portfolio=portf_test, # %>% tail(-1)
                                     optimize_method="ROI", # ROI 
                                     rebalance_on="months",# days months quarters
                                     training_period=90) # training_period 200..900
    bt_test.r <- Return.portfolio(R=returns,weights=extractWeights(bt_test))
    colnames(bt_test.r) <- "MPT"
    removeModal()
    
    
    output$UI_hrp_study_plot_compare <- renderPlot({
      compare <- cbind(get("HRP_compare",envir=.GlobalEnv), bt_test.r)
      charts.PerformanceSummary(compare,main="Compare with MPT")
      
    })
    
    output$UI_hrp_study_plot_compare_weights <- renderPlot({
      hrpWts <- get("HRP_weights",envir=.GlobalEnv)
      mptWts <- extractWeights(bt_test)
      legend_position <- ifelse(length(colnames(hrpWts)>20),"none","top")
      grid.arrange(
        data.frame(hrpWts) %>% mutate(date=index(hrpWts)) %>% pivot_longer(cols=-date,names_to="asset",values_to="weight") %>% 
          mutate(bar_label=ifelse((weight>.17),paste0(asset,"\n",sprintf("%.0f%%",weight*100)),NA)) %>% 
          ggplot(aes(factor(format(date,"%Y-%m")),weight))+geom_col(aes(fill=asset),alpha=.5,color="black",group=1,width=1)+
          geom_text(aes(label=bar_label,group=asset),color="black",size=3,alpha=.5,position=position_stack(vjust=.5),angle=90)+ylab("HRP")+labs(title="Weigths rebalancing over time")+
          theme_minimal()+theme(legend.position="none",axis.text.x=element_text(angle=90),axis.title.x=element_blank()),
        data.frame(mptWts) %>% mutate(date=index(mptWts)) %>% pivot_longer(cols=-date,names_to="asset",values_to="weight") %>% 
          mutate(bar_label=ifelse((weight>.17),paste0(asset,"\n",sprintf("%.0f%%",weight*100)),NA)) %>% 
          ggplot(aes(factor(format(date,"%Y-%m")),weight))+geom_col(aes(fill=asset),alpha=.5,color="black",group=1,width=1)+
          geom_text(aes(label=bar_label,group=asset),color="black",size=3,alpha=.5,position=position_stack(vjust=.5),angle=90)+ylab("MPT")+
          theme_minimal()+theme(legend.position=legend_position,axis.text.x=element_text(angle=90),axis.title.x=element_blank()),
        ncol=1)
    })
    output$UI_hrp_study_graphs_compare <- renderUI({ 
      fluidRow(
        # fluidRow(column(4,h2("")),column(8,div())),
        br(),plotOutput("UI_hrp_study_plot_compare",height="700px"),
        br(),plotOutput("UI_hrp_study_plot_compare_weights",height="700px"),
        br()) 
    })
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  # MPT Study ##############################################
  output$UI_mpt_study <- renderUI({
    fluidRow(
      fluidRow(
        column(1,div()),
        column(10,
               h2("Modern Portfolio Theory (MPT)"),
               p("The modern portfolio theory (MPT) is a practical method for selecting investments in order to maximize their overall returns within an acceptable level of risk. This mathematical framework is used to build a portfolio of investments that maximize the amount of expected return for the collective given level of risk."),
               p("American economist Harry Markowitz pioneered this theory in his paper Portfolio Selection, which was published in the Journal of Finance in 1952. He was later awarded a Nobel Prize for his work on modern portfolio theory."),
               p("A key component of the MPT theory is diversification. Most investments are either high risk and high return or low risk and low return. Markowitz argued that investors could achieve their best results by choosing an optimal mix of the two based on an assessment of their individual tolerance to risk."),
               p(style="text-align: right; font-size: 10px;","source: www.investopedia.com")),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_mpt_study_groups_panel"),uiOutput("UI_mpt_study_inputs_panel"))),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,fluidRow(uiOutput("UI_mpt_study_graphs"))),
        column(1,div())
      ),
      h2("...")
    )
  })
  assets_top_17 <- c("ITUB4","VALE3","BBDC4","ABEV3","PETR4","B3SA3","BBAS3","BRFS3","UGPA3","COGN3","LREN3","VIVT3","CCRO3","BBSE3","RADL3")
  assets_top_25 <- c("PETR4","VALE3","ITUB4","WEGE3","ABEV3")
  assets_hi_lo  <- c("PETR4","VALE3","ITUB4","WEGE3","ABEV3","AZUL4","MGLU3","ASAI3","CSAN3","USIM5")
  assets_oil    <- c("PETR4","CSAN3","PRIO3","BRAV3","VBBR3","UGPA3","RAIZ4","SMTO3")
  assets_iron   <- c("VALE3","GGBR4","CSNA3","CMIN3","USIM5")
  assets_banks  <- c("ITUB4","BBDC4","BBAS3","SANB11","INBR32","ROXO34")
  assets_agro   <- c("RAPT4","PCAR3","CAML3","KEPL3","VAMO3","HBSA3","GMAT3","TUPY3","CRFB3","MDIA3","BEEF3","SOJA3","TTEN3","MRFG3","JALL3","RANI3","AGRO3","DXCO3","ASAI3","RAIZ4","SMTO3","SLCE3" ,"RAIL3","BRFS3","CSAN3","JBSS3","ABEV3","KLBN11","SUZB3")
  assets_util   <- c("AURE3","NEOE3","CSMG3","ALUP11","AMBP3","SAPR11","CPFE3","TAEE11","ELET6","EGIE3","ISAE4","ENGI11","CPLE6","CMIG4","ENEV3","EQTL3","SBSP3")
  assets_etf_1  <- c("BOVA11","SMAL11","PIBB11","DIVO11","CMDB11","MATB11","XINA11","GOLD11","NASD11","IVVB11")
  assets_etf_2  <- c("GOLD11","IVVB11","XINA11","BOVA11")
  output$UI_mpt_study_groups_panel <- renderUI({ fluidRow(br(),
    tabsetPanel(
      tabPanel("Stocks",fluidRow(p(style="text-align: center;",
        actionButton("UI_mpt_study_select_top_17",paste0("Top Marketcap 2017\n" ,"")),br(),
        actionButton("UI_mpt_study_select_top_25",paste0("Top Marketcap 2025 "  ,paste(assets_top_25,collapse=":"))),br(),
        actionButton("UI_mpt_study_select_hi_lo" ,paste0("Leaders and Laggards" ,"")),br(),
      ))),
      tabPanel("Sectors",fluidRow(p(style="text-align: center;",
        actionButton("UI_mpt_study_select_oil"   ,paste0("Oil "                 ,paste(assets_oil   ,collapse=":"))),br(),
        actionButton("UI_mpt_study_select_iron"  ,paste0("Iron Ore "            ,paste(assets_iron  ,collapse=":"))),br(),
        actionButton("UI_mpt_study_select_banks" ,paste0("Banks "               ,paste(assets_banks ,collapse=":"))),br(),
        actionButton("UI_mpt_study_select_agro"  ,paste0("Agro "                ,"")),
        actionButton("UI_mpt_study_select_util"  ,paste0("Utilities "           ,"")),br(),
      ))),
      tabPanel("ETF",fluidRow(p(style="text-align: center;",
        actionButton("UI_mpt_study_select_etf_1",paste0("" ,paste(assets_etf_1,collapse=":"))),br(),
        actionButton("UI_mpt_study_select_etf_2",paste0("" ,paste(assets_etf_2,collapse=":"))),br(),
      ))),
    ),
      br())})
  observeEvent(input$UI_mpt_study_select_top_17, { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_top_17) })
  observeEvent(input$UI_mpt_study_select_top_25, { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_top_25) })
  observeEvent(input$UI_mpt_study_select_hi_lo , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_hi_lo ) })
  observeEvent(input$UI_mpt_study_select_oil   , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_oil   ) })
  observeEvent(input$UI_mpt_study_select_iron  , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_iron  ) })
  observeEvent(input$UI_mpt_study_select_banks , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_banks ) })
  observeEvent(input$UI_mpt_study_select_agro  , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_agro  ) })
  observeEvent(input$UI_mpt_study_select_util  , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_util  ) })
  observeEvent(input$UI_mpt_study_select_etf_1 , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_etf_1 ) })
  observeEvent(input$UI_mpt_study_select_etf_2 , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_etf_2 ) })
  output$UI_mpt_study_inputs_panel <- renderUI({ fluidRow(
      selectInput("UI_mpt_study_assets_input","Choose the assets, inputs parameters and then your visualization",unique(extract_data$asset),c(""),multiple=T,width="100%"),
      fluidRow(
        column(6,checkboxGroupInput("UI_mpt_study_constrains","Constrains:",selected=c("full_investment","long_only"),
                                    choiceNames=list("Full Investment","Long Only","Box Constrained Weights"),
                                    choiceValues=list("full_investment","long_only","box")),fluidRow(column(6,textInput("UI_mpt_study_constrains_box_min","min",value=0)),column(6,textInput("UI_mpt_study_constrains_box_max","max",value=0.4)))),
        column(6,checkboxGroupInput("UI_mpt_study_objectives","Objectives:",selected=c("risk","return"),
                                    choiceNames=list("Minimize Variance","Maximize Return","Minimize Expected Tail Loss (ETL) aka Conditional Value-at-Risk (CVaR) (this can take time with a long window)"),
                                    choiceValues=list("risk","return","ETL")))),
      fluidRow(
        column(7,
               actionButton("UI_mpt_study_covariance"    ,"Covariance"),
               actionButton("UI_mpt_study_correlations"  ,"Correlation"),
               actionButton("UI_mpt_study_run"           ,"Run Optimization"),
               actionButton("UI_mpt_study_rebalancing"   ,"Rebalancing")),
        column(2,textInput("UI_mpt_study_rebalancing_training_period",NULL,value=90,width="60px")),
        column(3,dateInput("UI_mpt_study_rebalancing_start_date"     ,NULL,value="2023-01-01",width="90px"))),
      br())})
  # MPT Study Covariance ###################################
  observeEvent(input$UI_mpt_study_covariance,{
    returns <- MPT_asset_returns(input$UI_mpt_study_assets_input)
    output$UI_mpt_study_plot_covariance <- renderPlot({
      returns_validate <- MPT_validate_returns_plot(returns)
      if(!is.null(returns_validate)) { plot(returns_validate)
      } else { 
        cov_matrix <- cov(returns)
        cov_melted <- melt(cov_matrix)
        grid.arrange(
          cov_melted %>% ggplot()+geom_boxplot(aes(value),outlier.size=.5)+labs(title="Covariance values distribution",x="")+theme_minimal()+theme(axis.text.y=element_blank()),
          cov_melted %>% ggplot(aes(x=Var1,y=Var2,fill=value))+geom_tile()+
            scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=mean(cov_melted$value))+
            theme_minimal()+theme(axis.text.x=element_text(angle=90))+
            labs(title = "Covariance matrix heatmap", x = "", y = ""),
          heights=c(1,3)
        )
      }
    })
    output$UI_mpt_study_graphs <- renderUI({ fluidRow(h2("Matrix of covariances"),plotOutput("UI_mpt_study_plot_covariance",height="500px"),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()) })
  })
  # MPT Study Correlations #################################
  observeEvent(input$UI_mpt_study_correlations,{
    # returns <- MPT_prepare_returns(input$UI_mpt_study_assets_input)
    returns <- MPT_asset_returns(input$UI_mpt_study_assets_input)
    output$UI_mpt_study_plot_correlations <- renderPlot({
      returns_validate <- MPT_validate_returns_plot(returns)
      # if(is.null(returns))                          { plot_title("Cannot plot correlations\nasset returns NULL",fill="white")
      # } else if(ncol(returns)>20 | nrow(returns)<60){ plot_title(paste("Cannot plot correlations","\nncol =",ncol(returns),"\nnrow =",nrow(returns)),fill="white")
      if(!is.null(returns_validate)) { plot(returns_validate)
      } else { corrplot(round(cor(returns),2),method="color",addCoef.col="navy",order="AOE",number.cex=0.975) }
    })
    output$UI_mpt_study_graphs <- renderUI({ fluidRow(h2("Correlation"),plotOutput("UI_mpt_study_plot_correlations",height="500px"),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()) })
  })
  # MPT Study Optimization #################################
  observeEvent(input$UI_mpt_study_run,{
    # returns <- MPT_prepare_returns(input$UI_mpt_study_assets_input)
    returns <- MPT_asset_returns(input$UI_mpt_study_assets_input)
    output$UI_mpt_study_plot_optimize <- renderPlot({
      returns_validate <- MPT_validate_returns_plot(returns)
      # if(is.null(returns))                          { plot_title("Cannot plot correlations\nasset returns NULL",fill="white")
      # } else if(ncol(returns)>20 | nrow(returns)<60){ plot_title(paste("Cannot plot correlations","\nncol =",ncol(returns),"\nnrow =",nrow(returns)),fill="white")
      if(!is.null(returns_validate)) { plot(returns_validate)
      } else {
        portf_test <- MPT_create_portfolio(colnames(returns),input$UI_mpt_study_constrains,input$UI_mpt_study_objectives,input$UI_mpt_study_constrains_box_min,input$UI_mpt_study_constrains_box_max)
        simple_message("Optimize portfolio")
        opt_test <- optimize.portfolio(R=returns,portfolio=portf_test,optimize_method="ROI",trace=T)
        plot(opt_test,chart.assets=TRUE,xlim=c(-0.01, 0.22))
      }
    })
    output$UI_mpt_study_graphs <- renderUI({ fluidRow(h2("Optimization"),plotOutput("UI_mpt_study_plot_optimize",height="700px")) })
  })
  # MPT Study Rebalancing ##################################
  asset_colors <- c("optimize"="purple3","GOLD11"="gold","HASH11"="orange","QBTC11"="orange","IVVB11"="dodgerblue3","NASD11"="dodgerblue1","XINA11"="red4","BOVA11"="green4","SMAL11"="green2")
  observeEvent(input$UI_mpt_study_rebalancing,{
    # showModal(modalDialog("Running rebalancing training...",footer=NULL))
    # returns <- MPT_prepare_returns(c("XINA11","GOLD11","IVVB11","HASH11"))
    # returns <- MPT_prepare_returns(input$UI_mpt_study_assets_input)
    returns <- MPT_asset_returns(input$UI_mpt_study_assets_input)
    dates_count <- nrow(returns[as.Date(index(returns)) > as.Date(input$UI_mpt_study_rebalancing_start_date), ])
    # returns %>% tail(dates_count+as.integer(90)) %>% head(10)
    study_rebalancing_training_period <- as.integer(input$UI_mpt_study_rebalancing_training_period)
    returns <- returns %>% tail(dates_count+study_rebalancing_training_period)
    
    
    study_constrains <- input$UI_mpt_study_constrains
    study_objectives <- input$UI_mpt_study_objectives
    study_constrains_box_min <- input$UI_mpt_study_constrains_box_min
    study_constrains_box_max <- input$UI_mpt_study_constrains_box_max
    
    
    
    output$UI_mpt_study_plot_weights_portfolio <- renderText({ colnames(returns) })
    output$UI_mpt_study_plot_weights <- renderPlot({
      returns_validate <- MPT_validate_returns_plot(returns)
      if(!is.null(returns_validate)) { plot(returns_validate)
      } else {
        showModal(modalDialog("Running rebalancing training...",footer=NULL))
        # portf_test <- MPT_create_portfolio(colnames(returns),c("full_investment","box"),c("risk","return"),"0.0","0.4")
        portf_test <- MPT_create_portfolio(colnames(returns),study_constrains,study_objectives,study_constrains_box_min,study_constrains_box_max)
        print(portf_test)
        simple_message("Rebalancing backtest portfolio")
        # bt_test <- optimize.portfolio.rebalancing(R=returns,portfolio=portf_test,optimize_method="ROI",rebalance_on="quarters",training_period=90)
        bt_test <- optimize.portfolio.rebalancing(R=returns,portfolio=portf_test,optimize_method="ROI",rebalance_on="quarters",training_period=study_rebalancing_training_period)
        print(bt_test)
        output$UI_mpt_study_plot_weights_status <- renderPrint({ bt_test$elapsed_time })
        output$UI_mpt_study_plot_weights_summary <- renderPrint({ summary(bt_test) })
        simple_message("Calculating weighted returns")
        bt_test.r <- Return.portfolio(R=returns,weights=extractWeights(bt_test))
        colnames(bt_test.r) <- "optimize"
        
        # if(any(names(bt_test$portfolio$asset) %in% names(asset_colors))) { s_fill <- scale_fill_manual(values=asset_colors)
        # } else { s_fill <- scale_fill_brewer(palette="Paired") }
        # chart.Weights(bt_test,main="Weigths over time",las=2)
        # data.frame(extractWeights(bt_test)) %>% mutate(date=index(extractWeights(bt_test))) %>% pivot_longer(cols=-date,names_to="asset",values_to="weight") %>% 
        #   ggplot(aes(factor(format(date,"%Y-%m")),weight))+geom_col(aes(fill=asset),color="black",group=1,width=1)+
        #   xlab("")+s_fill+
        #   theme_minimal()+theme(legend.position="top",axis.text.x=element_text(angle=90))
        
        # print(names(bt_test$portfolio$asset))
        if(any(names(bt_test$portfolio$asset) %in% names(asset_colors))) { 
          s_fill <- scale_fill_manual(values=asset_colors)
          s_colors <- scale_color_manual(values=asset_colors)
        } else { 
          s_fill <- scale_fill_brewer(palette="Greys") 
          s_colors <- scale_color_manual(values=c("optimize"="purple3"))
        }
        curve_capital_df <- NULL %>% 
          rbind(data.frame(returns  ) %>% mutate(date=index(returns  )) %>% pivot_longer(cols=-date,names_to="asset",values_to="var")) %>% 
          rbind(data.frame(bt_test.r) %>% mutate(date=index(bt_test.r)) %>% pivot_longer(cols=-date,names_to="asset",values_to="var")) %>% 
          as_tibble() %>% 
          # mutate(var=exp(var)) %>%
          mutate(var=(var+1)) %>%
          # filter(date>"2021-10-01") %>% 
          filter(date>min(index(bt_test.r))) %>%
          group_by(asset) %>% arrange(date) %>% mutate(cumvar=cumprod(var)) %>% #ungroup() %>% #tail(10)
          ungroup()
        removeModal()
        grid.arrange(
          data.frame(extractWeights(bt_test)) %>% mutate(date=index(extractWeights(bt_test))) %>% pivot_longer(cols=-date,names_to="asset",values_to="weight") %>% 
            mutate(bar_label=ifelse((weight>.17),paste0(asset,"\n",sprintf("%.0f%%",weight*100)),NA)) %>% 
            ggplot(aes(factor(format(date,"%Y-%m")),weight))+geom_col(aes(fill=asset),alpha=.5,color="black",group=1,width=1)+
            geom_text(aes(label=bar_label,group=asset),color="black",size=3,alpha=.5,position=position_stack(vjust=.5),angle=90)+xlab("")+s_fill+
            # theme_minimal()+theme(legend.position="top",axis.text.x=element_text(angle=90))
            theme_minimal()+theme(legend.position="none",axis.text.x=element_blank(),axis.title.x=element_blank()),
          # NULL %>% 
          #   rbind(data.frame(returns  ) %>% mutate(date=index(returns  )) %>% pivot_longer(cols=-date,names_to="asset",values_to="var")) %>% 
          #   rbind(data.frame(bt_test.r) %>% mutate(date=index(bt_test.r)) %>% pivot_longer(cols=-date,names_to="asset",values_to="var")) %>% 
          #   as_tibble() %>% 
          #   # mutate(var=exp(var)) %>%
          #   mutate(var=(var+1)) %>%
          #   # filter(date>"2021-10-01") %>% 
          #   filter(date>min(index(bt_test.r))) %>%
          #   group_by(asset) %>% arrange(date) %>% mutate(cumvar=cumprod(var)) %>% ungroup() %>% #tail(10)
          curve_capital_df %>% 
            ggplot(aes(date,cumvar))+geom_line(aes(color=asset,alpha=ifelse(asset=="optimize",1,.2)))+scale_alpha_identity()+
            # scale_color_manual(values=c("optimize"="purple3","GOLD11"="gold","HASH11"="orange","QBTC11"="orange","IVVB11"="dodgerblue3","NASD11"="dodgerblue1","XINA11"="red4","BOVA11"="green4","SMAL11"="green2"))+
            annotate(geom="label",x=min(index(bt_test.r))+30,y=max(curve_capital_df$cumvar)*.95,color="gray",hjust=0,label.size=NA,label.r=unit(0,"pt"),label=paste(sapply(portf_test$constraints, function(x) x$type),collapse=" | "))+
            annotate(geom="label",x=min(index(bt_test.r))+30,y=max(curve_capital_df$cumvar)*.85,color="gray",hjust=0,label.size=NA,label.r=unit(0,"pt"),label=paste(sapply(portf_test$objectives , function(x) x$name),collapse=" | "))+
            ylab("Optimized Cumulative Return")+s_colors+scale_x_date(position="top")+
            theme_minimal()+theme(legend.position="none",axis.title.y=element_text(color="purple"),axis.title.x=element_blank()),
          heights=c(1,2)
        )
        
      }
    })
    output$UI_mpt_study_graphs <- renderUI({ fluidRow(fluidRow(column(6,h2("Rebalancing backtest")),column(6,textOutput("UI_mpt_study_plot_weights_portfolio"))),
                                                      plotOutput("UI_mpt_study_plot_weights",height="600px"),br(),verbatimTextOutput("UI_mpt_study_plot_weights_status"),verbatimTextOutput("UI_mpt_study_plot_weights_summary")) })
    # removeModal()
  })
  
  
  
  # MAIN ###################################################
  output$UI_main_page <- renderUI({
    fluidRow(
      fluidRow(
        column(1,div()),
        column(10,
               p(style="text-align: center; font-size: 10px;","Disclaimer: this is not an investment recommendation, this is a statistical study with data visualization "),
               h1("What Is the Capital Asset Pricing Model (CAPM)?"),
               p("The capital asset pricing model describes the relationship between systematic risk and expected return for assets, particularly stocks. It is a finance model that establishes a linear relationship between the required return on an investment and risk."),
               p("CAPM is based on the relationship between an asset’s beta, the risk-free rate and the equity risk premium, or the expected return on the market minus the risk-free rate."),
               p("CAPM evolved as a way to measure this systematic risk. It is widely used throughout finance for pricing risky securities and generating expected returns for assets, given the risk of those assets and cost of capital."),
               p(strong("KEY TAKEAWAYS"),
                 div(
                   tags$ul(
                     tags$li("The capital asset pricing model is a financial model that calculates the expected rate of return for an asset."),
                     tags$li("CAPM does this by using the expected return on both the market and a risk-free asset, and the asset’s correlation or sensitivity to the market (beta)."),
                     tags$li("There are some limitations to the CAPM, such as making unrealistic assumptions and relying on a linear interpretation of risk vs. return."),
                     tags$li("Despite its issues, the CAPM formula is still widely used because it is simple and allows for easy comparisons of investment alternatives."),
                     tags$li("For instance, it is used in conjunction with modern portfolio theory (MPT) to understand portfolio risk and expected"),
                   ))),
               p(style="text-align: right; font-size: 10px;","source: www.investopedia.com"),
               p("Using the CAPM to build a portfolio is supposed to help an investor manage their risk. If an investor were able to use the CAPM to perfectly optimize a portfolio’s return relative to risk, it would exist on a curve called the efficient frontier, as shown in the following graph."),
               img(style="display: block; margin-left: auto; margin-right: auto; height: 350px",src="https://www.investopedia.com/thmb/Yer-5_eZyP12ofB-XKLSgZ8ODvE=/750x0/filters:no_upscale():max_bytes(150000):strip_icc()/CapitalAssetPricingModelCAPM1_2-e6be6eb7968d4719872fe0bcdc9b8685.png"),
               br(),br(),br(),
               img(style="display: block; margin-left: auto; margin-right: auto; height: 250px",src="efficient_frontier_and_correlation.png"),
               br(),br(),br()),
        column(1,div())
      ),
      fluidRow(
        column(1,div()),
        column(10,div()),
        column(1,div())
      ),
      h2("...")
    )
  })
  
  
  
  
  # About me ###############################################
  
  
  
} 



shinyApp(ui=ui,server=server) 






