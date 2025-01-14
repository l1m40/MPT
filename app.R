#
#
# source("app.R")

# INSTALL AND LOAD PACKAGES ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,shiny,shinyjs,shinydashboard,dplyr,tidyr,ggplot2,gridExtra) 


# Install MPT packages #####################################
suppressMessages(library(PortfolioAnalytics))
suppressMessages(library(foreach))
suppressMessages(library(iterators))
suppressMessages(library(ROI))
suppressMessages(library(ROI.plugin.quadprog))
suppressMessages(library(ROI.plugin.glpk))
library(corrplot)






# Basic functions ##########################################
simple_message        <- function(msg){return(cat(paste("","ℹ",msg),"\n"))}
successful_message    <- function(msg){return(cat(paste("\033[0;32m","","✔",msg,"\033[0m"),"\n\n"))}
warning_message       <- function(msg){return(cat(paste("\033[0;33m","","!",msg,"\033[0m"),"\n\n"))}
error_message         <- function(msg){return(cat(paste("\033[0;31m","","ⓧ",msg,"\033[0m"),"\n\n"))}
plot_title <- function(text,textsize=9,fill="gray13") { return(ggplot()+geom_text(aes(x=0,y=0,label=text),size=textsize)+theme_void()+theme(plot.background=element_rect(fill=fill,colour=fill))) }




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
#   select(asset,date,price) #%>% saveRDS("extract_data.rds")
# Cache prices #############################################
simple_message("Loading prices")
tryCatch({ extract_data <- readRDS("extract_data.rds") },
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
MPT_asset_price_action <- function(asset_input){
  df <- extract_data %>% filter(asset==asset_input) %>% mutate(varln=log(price/lag(price))) %>% select(date,varln)
  colnames(df) <- c("date",asset_input)
  return(df)
}
MPT_asset_returns <- function(asset_list=c("PETR4","VALE3","ITUB4","ELET3")){
  df <- MPT_asset_price_action(asset_list[1])
  for(i in 2:length(asset_list)) { df <- df %>% left_join(MPT_asset_price_action(asset_list[i]),by="date") }
  return(df)
}
MPT_prepare_returns <- function(asset_list){
  if(length(asset_list)==0) return(NULL)
  simple_message(paste(asset_list,collapse=":"))
  simple_message("Prepare price")
  returns <- MPT_asset_returns(asset_list)
  simple_message("Removing NAs")
  for(c in colnames(returns)){ returns=returns[!is.na(returns[,c]),] }
  simple_message("Transforming Time Series")
  returns <- zoo(returns[,!names(returns) %in% c("date")] %>% as.matrix(),returns$date)
  simple_message(paste(nrow(returns),"dates since",min(index(returns))))
  return(returns)
}
MPT_validate_returns_plot <- function(returns){
  if(is.null(returns))                          { return(plot_title("Cannot plot correlations\nasset returns NULL",fill="white"))
  } else if(ncol(returns)>20 | nrow(returns)<60){ return(plot_title(paste("Cannot plot correlations","\nncol =",ncol(returns),"\nnrow =",nrow(returns)),fill="white"))
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





# UI #######################################################
simple_message("Loading UI")
ui <- dashboardPage(
  
  dashboardHeader(title="CAPM",dropdownMenu(type = "notifications", badgeStatus = "warning",notificationItem(icon = icon("users"), status = "info","5 new members joined today"),notificationItem(icon = icon("warning"), status = "danger","Resource usage near limit."),notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),status = "success", "25 sales made"),notificationItem(icon = icon("user", lib = "glyphicon"),status = "danger", "You changed your username"))),
  dashboardSidebar(sidebarMenu(
    menuItem("|||"                ,tabName="about_me"      ,icon=NULL),
    menuItem(" Assets collection" ,tabName="assets"        ,icon=NULL),
    menuItem(" MPT Study"         ,tabName="study"         ,icon=icon("education",lib="glyphicon"))
    # menuItem("  Backtest Analysis",tabName="backtest",icon=suppressMessages(icon("chart-simple",lib="font-awesome"))),
    # menuItem("  Data Mining",tabName="backtest_conditions",icon=suppressMessages(icon("chart-simple",lib="font-awesome"))),
    # menuItem("  Pair Analysis",tabName="pair_analysis",icon=suppressMessages(icon("chart-simple",lib="font-awesome")))
  )),
  dashboardBody(tabItems(
    tabItem("about_me"      ,uiOutput("UI_about_me")),
    tabItem("assets"        ,uiOutput("UI_assets")),
    tabItem("study"         ,uiOutput("UI_mpt_study"))
  ))
)


# Server ###################################################
simple_message("Loading server")
server <- function(input, output, session) {
  
  
  
  
  
  
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
  assets_oil    <- c("PETR4","CSAN3","PRIO3","BRAV3","VBBR3","UGPA3","RAIZ4","SMTO3")
  assets_iron   <- c("VALE3","GGBR4","CSNA3","CMIN3","USIM5")
  assets_banks  <- c("ITUB4","BBDC4","BBAS3","SANB11","INBR32","ROXO34")
  assets_etf_1  <- c("BOVA11","SMAL11","PIBB11","DIVO11","CMDB11","MATB11","XINA11","GOLD11","NASD11","IVVB11")
  assets_etf_2  <- c("GOLD11","IVVB11","XINA11","BOVA11")
  output$UI_mpt_study_groups_panel <- renderUI({ fluidRow(br(),
    tabsetPanel(
      tabPanel("Stocks",fluidRow(p(style="text-align: center;",
        actionButton("UI_mpt_study_select_top_17",paste0("Top Marketcap 2017\n" ,"")),br(),
        actionButton("UI_mpt_study_select_top_25",paste0("Top Marketcap 2025 "  ,paste(assets_top_25,collapse=":"))),br(),
      ))),
      tabPanel("Sectors",fluidRow(p(style="text-align: center;",
        actionButton("UI_mpt_study_select_oil"   ,paste0("Oil "                 ,paste(assets_oil   ,collapse=":"))),br(),
        actionButton("UI_mpt_study_select_iron"  ,paste0("Iron Ore "            ,paste(assets_iron  ,collapse=":"))),br(),
        actionButton("UI_mpt_study_select_banks" ,paste0("Banks "               ,paste(assets_banks ,collapse=":"))),br(),
      ))),
      tabPanel("ETF",fluidRow(p(style="text-align: center;",
        actionButton("UI_mpt_study_select_etf_1",paste0("" ,paste(assets_etf_1,collapse=":"))),br(),
        actionButton("UI_mpt_study_select_etf_2",paste0("" ,paste(assets_etf_2,collapse=":"))),br(),
      ))),
    ),
      br())})
  observeEvent(input$UI_mpt_study_select_top_17, { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_top_17) })
  observeEvent(input$UI_mpt_study_select_top_25, { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_top_25) })
  observeEvent(input$UI_mpt_study_select_oil   , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_oil   ) })
  observeEvent(input$UI_mpt_study_select_iron  , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_iron  ) })
  observeEvent(input$UI_mpt_study_select_banks , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_banks ) })
  observeEvent(input$UI_mpt_study_select_etf_1 , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_etf_1 ) })
  observeEvent(input$UI_mpt_study_select_etf_2 , { updateSelectInput(session,"UI_mpt_study_assets_input",selected=assets_etf_2 ) })
  output$UI_mpt_study_inputs_panel <- renderUI({ fluidRow(
      selectInput("UI_mpt_study_assets_input","Choose the assets, inputs parameters and then your visualization",unique(extract_data$asset),c(""),multiple=T,width="100%"),
      fluidRow(
        column(6,checkboxGroupInput("UI_mpt_study_constrains","Constrains:",selected=c("full_investment"),
                                    choiceNames=list("Full Investment","Long Only","Box Constrained Weights"),
                                    choiceValues=list("full_investment","long_only","box")),fluidRow(column(6,textInput("UI_mpt_study_constrains_box_min","min",value=0)),column(6,textInput("UI_mpt_study_constrains_box_max","max",value=0.4)))),
        column(6,checkboxGroupInput("UI_mpt_study_objectives","Objectives:",selected=c("risk"),
                                    choiceNames=list("Minimize Variance","Maximize Return","Minimize Expected Tail Loss (this can take time with a long window)"),
                                    choiceValues=list("risk","return","ETL")))),
      fluidRow(
        column(7,
               actionButton("UI_mpt_study_correlations"  ,"Correlation"),
               actionButton("UI_mpt_study_run"           ,"Run Optimization"),
               actionButton("UI_mpt_study_rebalancing"   ,"Rebalancing")),
        column(2,textInput("UI_mpt_study_rebalancing_training_period",NULL,value=90,width="60px")),
        column(3,dateInput("UI_mpt_study_rebalancing_start_date"     ,NULL,value="2023-01-01",width="90px"))),
      br())})
  # MPT Study Correlations #################################
  observeEvent(input$UI_mpt_study_correlations,{
    returns <- MPT_prepare_returns(input$UI_mpt_study_assets_input)
    output$UI_mpt_study_correlation <- renderPlot({
      returns_validate <- MPT_validate_returns_plot(returns)
      # if(is.null(returns))                          { plot_title("Cannot plot correlations\nasset returns NULL",fill="white")
      # } else if(ncol(returns)>20 | nrow(returns)<60){ plot_title(paste("Cannot plot correlations","\nncol =",ncol(returns),"\nnrow =",nrow(returns)),fill="white")
      if(!is.null(returns_validate)) { plot(returns_validate)
      } else { corrplot(round(cor(returns),2),method="color",addCoef.col="navy",order="AOE",number.cex=0.975) }
    })
    output$UI_mpt_study_graphs <- renderUI({ fluidRow(h2("Correlation"),plotOutput("UI_mpt_study_correlation"),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()) })
  })
  # MPT Study Optimization #################################
  observeEvent(input$UI_mpt_study_run,{
    returns <- MPT_prepare_returns(input$UI_mpt_study_assets_input)
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
    returns <- MPT_prepare_returns(input$UI_mpt_study_assets_input)
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
        removeModal()
        grid.arrange(
          data.frame(extractWeights(bt_test)) %>% mutate(date=index(extractWeights(bt_test))) %>% pivot_longer(cols=-date,names_to="asset",values_to="weight") %>% 
            mutate(bar_label=ifelse((weight>.17),paste0(asset,"\n",sprintf("%.0f%%",weight*100)),NA)) %>% 
            ggplot(aes(factor(format(date,"%Y-%m")),weight))+geom_col(aes(fill=asset),alpha=.5,color="black",group=1,width=1)+
            geom_text(aes(label=bar_label,group=asset),color="black",size=3,alpha=.5,position=position_stack(vjust=.5),angle=90)+xlab("")+s_fill+
            # theme_minimal()+theme(legend.position="top",axis.text.x=element_text(angle=90))
            theme_minimal()+theme(legend.position="none",axis.text.x=element_blank(),axis.title.x=element_blank()),
          NULL %>% 
            rbind(data.frame(returns  ) %>% mutate(date=index(returns  )) %>% pivot_longer(cols=-date,names_to="asset",values_to="var")) %>% 
            rbind(data.frame(bt_test.r) %>% mutate(date=index(bt_test.r)) %>% pivot_longer(cols=-date,names_to="asset",values_to="var")) %>% 
            as_tibble() %>% 
            # mutate(var=exp(var)) %>%
            mutate(var=(var+1)) %>%
            # filter(date>"2021-10-01") %>% 
            filter(date>min(index(bt_test.r))) %>%
            group_by(asset) %>% arrange(date) %>% mutate(cumvar=cumprod(var)) %>% ungroup() %>% #tail(10)
            ggplot(aes(date,cumvar))+geom_line(aes(color=asset,alpha=ifelse(asset=="optimize",1,.2)))+scale_alpha_identity()+
            # scale_color_manual(values=c("optimize"="purple3","GOLD11"="gold","HASH11"="orange","QBTC11"="orange","IVVB11"="dodgerblue3","NASD11"="dodgerblue1","XINA11"="red4","BOVA11"="green4","SMAL11"="green2"))+
            ylab("Optimized Cumulative Return")+s_colors+scale_x_date(position="top")+
            theme_minimal()+theme(legend.position="none",axis.title.y=element_text(color="purple"),axis.title.x=element_blank()),
          heights=c(1,2)
        )
        
      }
    })
    output$UI_mpt_study_graphs <- renderUI({ fluidRow(fluidRow(column(6,h2("Rebalancing backtest")),column(6,textOutput("UI_mpt_study_plot_weights_portfolio"))),
                                                      plotOutput("UI_mpt_study_plot_weights",height="600px")) })
    # removeModal()
  })
  
  
  
  # About me ###############################################
  output$UI_about_me <- renderUI({
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
  
} 



shinyApp(ui=ui,server=server) 






