lib <- c("ggplot2", "tidyr", "readxl")
lapply(lib, function(x){library(x, character.only = TRUE)})

#
# IB Trading specific variables
#
platform <- "TWS"     # Options: TWS, IBG
acct <- "Paper"    # Options: Live, Paper
active_trade_ids <- c()
ts_static <<- TradingSession(22, platform, acct)

#
# Common parameters for server and ui
#
blotter_field_default_width <- "90px"
max_blotter_size <- 5
max_message_count <- 3
econ_indi_panel_default_width <- 12
econ_indi_tab_names <- c("gei_dt", "lei_dt", "coi_dt", "lai_dt")

#
# Load trading class depending on OS
#
if(R.Version()$os == "linux-gnu"){
  onedrive.dir <- "/home/kmin/"
  cls.rpo.dir <- paste0(onedrive.dir, "ShinyApps/TraderPortal/Helper/")
  shiny.dir <- paste0(onedrive.dir, "ShinyApps/TraderPortal/")
  watchlist.dir <- paste0(onedrive.dir, "ShinyApps/TraderPortal/Helper/")
  ei.fn <- paste0(cls.rpo.dir, "EconomicIndicatorsDescription.xlsx")
} else {
  onedrive.dir <- "C:/Users/KE/OneDrive/"
  cls.rpo.dir <- paste0(onedrive.dir, "Development/R/Repository/Class/")
  shiny.dir <- paste0(onedrive.dir, "Development/Shiny/ShiyTraderPortal/")
  watchlist.dir <- paste0(onedrive.dir, "Investment/Research/Economic Indicators/")
  ei.fn <- paste0(shiny.dir, "EconomicIndicatorsDescription.xlsx")
  
}
ws.fn <- paste0(watchlist.dir, "Watchlist.xlsx")

#
# Load portfolio status function
#
setwd(cls.rpo.dir)
source("IB_TWS_TradingSession.R")
source("FinancialSecurityHistoricalData.R")
source("EconomicIndicators.R")

##
# Utility functions for shiny trading portal
##

#
# Set shiny directory
#
setwd(shiny.dir)

#
# Get portfolio data
# 
UtilGetPortfolio <- function(){
  ts_tmp <- TradingSession(11, platform, acct)
  ts_tmp <- TSSetTransmit(ts_tmp, FALSE)     #Prevert trade from actually happening
  ts_tmp <- TSRetrievePortHoldings(ts_tmp)
  TSCloseTradingSession(ts_tmp)
  port_prelim <- ts_tmp$ts_port_holdings
  forex <- ts_tmp$ts_exchange_rate
  
  if(nrow(port_prelim) == 0){
    update_time <- Sys.time()
    holdings <- data.frame(Ticker = character(0), stringsAsFactors = FALSE)
    port_intrim <- data.frame(Ticker = character(0),
                              SecurityType = character(0),
                              Position = numeric(0),
                              Cost = numeric(0),
                              MktPrc = numeric(0),
                              UnrealizedPNL = numeric(0),
                              UnrealizedPNLPrc = character(0),
                              stringsAsFactors = FALSE)
    
    us_cash <- ts_tmp$ts_us_cash_balance
    port_us_cash <- data.frame(Ticker = "USD",
                               SecurityType = "CASH",
                               Position = round(us_cash,2),
                               Cost = 0,
                               MkrPrc =  round(us_cash * forex,2),
                               UnrealizedPNL = 0,
                               UnrealizedPNLPrc = "0.00%",
                               stringsAsFactors = FALSE)
    
    ca_cash <- ts_tmp$ts_ca_cash_balance
    port_ca_cash <- data.frame(Ticker = "CAD",
                               SecurityType = "CASH",
                               Position = round(ca_cash,2),
                               Cost = 0,
                               MkrPrc = round(ca_cash * 1,2),
                               UnrealizedPNL = 0,
                               UnrealizedPNLPrc = "0.00%",
                               stringsAsFactors = FALSE)
    
    port <- dplyr::bind_rows(list(port_us_cash, port_ca_cash))  # Output 3
  } else {
    update_time <- port_prelim$TimeStamp[1]  # Output 1
    port_prelim <- port_prelim[port_prelim$LocalTicker != "USD.CAD",]
    port_prelim$Ticker <- paste0(port_prelim$LocalTicker, "-", port_prelim$Currency)
    
    holdings <- port_prelim[,"Ticker"]  # Output 2
    
    port_prelim$UnrealizedPNLPrc <- paste0(round(port_prelim$UnrealizedPNL/(port_prelim$Position*port_prelim$AvgCost)*100,2),"%")
    port_prelim$Cost <- round(port_prelim$AvgCost, 2)
    port_prelim$Position <- round(port_prelim$Position, 2)
    
    port_intrim <- port_prelim[,c("Ticker", "SecurityType", "Position", "Cost", "MktPrc", "UnrealizedPNL", "UnrealizedPNLPrc")]
    
    us_cash <- ts_tmp$ts_us_cash_balance
    port_us_cash <- data.frame(Ticker = "USD",
                               SecurityType = "CASH",
                               Position = round(us_cash,2),
                               Cost = 0,
                               MkrPrc = forex,
                               UnrealizedPNL = 0,
                               UnrealizedPNLPrc = "0.00%",
                               stringsAsFactors = FALSE)
    
    ca_cash <- ts_tmp$ts_ca_cash_balance
    port_ca_cash <- data.frame(Ticker = "CAD",
                               SecurityType = "CASH",
                               Position = round(ca_cash,2),
                               Cost = 0,
                               MkrPrc = 1,
                               UnrealizedPNL = 0,
                               UnrealizedPNLPrc = "0.00%",
                               stringsAsFactors = FALSE)
    
    port <- dplyr::bind_rows(list(port_intrim, port_us_cash, port_ca_cash))  # Output 3
  }
  
  return(list(update_datetime = update_time,
              holdings = holdings,
              portfolio = port))
}

#
# Find current holding
#
UtilFindCurrentHolding <- function(ticker_with_current){
  port <- UtilGetPortfolio()$port
  if(nrow(port) == 0){
    pos <- 0
  } else {
    holding <- port[port$Ticker == ticker_with_current,]
    
    if(nrow(holding) == 0){
      pos <- 0
    } else {
      pos <- holding[,"Position"]
    }
  }

}

#
# Trade equity functions
#
UtilTradeEquityWithIB <- function(blotter){
	for(i in 1:nrow(blotter)){
		tik_with_crcy <- paste0(blotter[i,"LocalTicker"], "-", blotter[i,"Currency"])
		side <- blotter[i,"Action"]
		trade_shares <- blotter[i,"Quantity"]
		transmit <- blotter[i,"TradeSwitch"]
		
		#
		# Check the current position
		#
		curr_holding <- UtilFindCurrentHolding(tik_with_crcy)
		if(side == "Buy"){
			expected_after_holding <- curr_holding + trade_shares
		} else {
			expected_after_holding <- curr_holding - trade_shares
		}
		
		#
		# Trade
		#
		# ts_static <<- TradingSession(22, platform, acct)
		ts_static <<- TSSetTransmit(ts_static, transmit)     
		ts_static <<- TSSetPrelimTradeList(ts_static, blotter)
		ts_static <<- TSGenFnlTradeList(ts_static)
		ts_static <<- TSExecuteAllTrades(ts_static)
		curr_trd_id <- ts_static$ts_trade_ids[length(ts_static$ts_trade_ids)]
		print(curr_trd_id)
		err_msg <- ts_static$ts_last_trade_message[length(ts_static$ts_trade_ids)]
		# TSCloseTradingSession(ts_static)
		
		#
		# Run a loop to check if the trade is sucessful
		#
		flag <- 0
		while(i <= 3){
			actual_after_holding <- UtilFindCurrentHolding(tik_with_crcy)
			ifelse(actual_after_holding == expected_after_holding, flag <- 1, flag <- 0)
			
			if(flag == 1){
				break
			} else {
				i <- i + 1
				Sys.sleep(1)
			}
		}
	
		#
		# Output results
		#
		trade_res <- blotter
		trade_date <- format(Sys.Date(), "%Y-%m-%d")
		trade_time <- format(Sys.time(), "%H:%M:%S")
		if(flag == 1){
			trade_res$Date <- trade_date
			trade_res$Time <- trade_time
			trade_res$Result <- "Success"
			trade_res$TradeID <- curr_trd_id
			trade_res <- trade_res[,c(ncol(trade_res),1:(ncol(trade_res)-1))]
			
			msg <- data.frame(Date = trade_date,
							          Time = trade_time,
							          Msg = paste0("Trade (",curr_trd_id, ") ", tik_with_crcy, " is successfully traded (", side, ") at ",
											               trade_date, " ", trade_time),
							          stringsAsFactors = FALSE)
		} else {
		  if(curr_trd_id != -1){
		    active_trade_ids <<- c(active_trade_ids, curr_trd_id)   # Update background active trades
		  }
		  trade_res$Date <- trade_date
			trade_res$Time <- trade_time
			trade_res$Result <- "Failed"
			trade_res$TradeID <- curr_trd_id
			trade_res <- trade_res[,c(ncol(trade_res),1:(ncol(trade_res)-1))]
			
			msg <- data.frame(Date = trade_date,
							          Time = trade_time,
							          Msg = paste0("Trade (",curr_trd_id, ") ", tik_with_crcy, " is not traded (", side, ") at ",
											               trade_date, " ", trade_time),
							          stringsAsFactors = FALSE)
		}
	
	}
  return(list(trade_rec = trade_res, msg_rec = msg))
}

# blotter <- data.frame(LocalTicker = "IEFA",
#                       Action = "Buy",
#                       Quantity = 10,
#                       OrderType = "Lmt",
#                       LimitPrice = 20,
#                       SecurityType = "Stk",
#                       Currency = "USD",
#                       TradeSwitch = FALSE,
#                       stringsAsFactors = FALSE)
# res <- UtilTradeWithIB(blotter)

#
# Trade forex functions
#
UtilTradeForexWithIB <- function(blotter){
  for(i in 1:nrow(blotter)){
    transmit <- blotter[i,"TradeSwitch"]
    
    #
    # Trade
    #
    # ts_static <<- TradingSession(22, platform, acct)
    ts_static <<- TSSetTransmit(ts_static, transmit)     
    ts_static <<- TSExecuteTrade(ts_static, blotter[i,])
    
    res <- ts_static$ts_trade_results[length(ts_static$ts_trade_results)]
  }
  return(0)
}

blotter <- data.frame(From = "USD",
                      To = "CAD",
                      Quantity = "100",
                      SecurityType = "Forex",
                      TradeSwitch = FALSE,
                      stringsAsFactors = FALSE)
res <- UtilTradeForexWithIB(blotter)

#
# Cancel all trades
#
UtilCancelAllTrades <- function(){
  # Cancel all trades
  TSCancelAllTrades(ts_static)
  active_trade_ids <<- c()
  
  # Re-open ts Static
  # TSCloseTradingSession(ts_static)
  # ts_static <<- TradingSession(22, platform, acct)
}
# res <- UtilCancelAllTrades()

#
# Download etf historical price and calculate return
#
UtilGetMarketReturn <- function(){
  #
  # Setup
  #
  start.date <- as.Date("2013-01-01") 
  end.date <- Sys.Date()
  watchlist <- read_excel(ws.fn, sheet="Watchlist")
  watchlist <- na.omit(watchlist)
  ei.etf.keys <- paste("$", watchlist$LocalTicker, sep="")
  
  #
  # Retrieve quotes
  #
  fshd1 <- FinancialSecurityHistoricalData(id = 1, hist_startdate = start.date,
                                           hist_enddate = end.date)
  fshd1 <- FSHDSetWatchlist(fshd1, watchlist)
  fshd1 <- FSHDObtainAllHistPrcs(fshd1)
  ei.etf <- fshd1$FSHD_hist_cumret
  colnames(ei.etf) <- paste0(watchlist$Comments, " (", watchlist$LocalTicker, ")")
  
  return(ei.etf)
}

UtilGetStockHistReturn <- function(ticker_w_crncy){
  #
  # Setup
  #
  start.date <- as.Date("2013-01-01")
  end.date <- Sys.Date()
  
  pos <- regexpr("-", ticker_w_crncy)[1]
  if(pos == -1){
    ticker <- ticker_w_crncy
    currency <- "USD"
  } else {
    ticker <- substr(ticker_w_crncy, 1, pos-1)
    currency <- substr(ticker_w_crncy, pos+1, nchar(ticker_w_crncy))
  }
  
  watchlist <- data.frame(LocalTicker = ticker,
                          Currency = currency,
                          SecurityType = 'Stk',
                          Comments = 'None',
                          stringsAsFactors = FALSE)
  
  #
  # Retrieve quotes
  #
  fshd1 <- FinancialSecurityHistoricalData(id = 1, hist_startdate = start.date,
                                           hist_enddate = end.date)
  fshd1 <- FSHDSetWatchlist(fshd1, watchlist)
  fshd1 <- FSHDObtainAllHistPrcs(fshd1)
  ei.etf <- fshd1$FSHD_hist_cumret
  colnames(ei.etf) <- ticker_w_crncy
  
  return(ei.etf)
}

#
# Plot etf return data
#
UtilPlotMarketReturn <- function(master_plot_data, market, period){
  
  if(market == "Equity"){
    plot_data_prelim <- master_plot_data[,1:3]
  } else if (market == "Tbond"){
    plot_data_prelim <- master_plot_data[,4:7]
  } else if (market == "Cbond"){
    plot_data_prelim <- master_plot_data[,8:10]
  } else {
    # do nothing
    plot_data_prelim <- master_plot_data
  }
  
  # Filter based on period
  if(period == "5D"){
    offset <- 5
  } else if(period == "1M"){
    offset <- 252/12
  } else if(period == "3M"){
    offset <- 252/12 * 3
  } else if(period == "6M"){
    offset <- 252/12 * 6
  } else if(period == "1Y"){
    offset <- 252
  } else if(period == "3Y"){
    offset <- 252*3
  } else if(period == "5Y"){
    offset <- 252*5
  } else if(period == "YTD"){
    offset <- 252
  } else {
    offset <- 0
  }
  
  base <- plot_data_prelim[nrow(plot_data_prelim)-offset-1,]
  fac <- 1/as.vector((1+base))
  last_rec <- nrow(plot_data_prelim)
  plot_data_prelim <- plot_data_prelim[(last_rec-offset):last_rec,]
  plot_data_prelim_mtx <- (1+plot_data_prelim) %*% diag(fac, nrow = length(fac)) - 1
  
  # Transform data to dataframe
  plot_data_prelim_df <- data.frame(Period = index(plot_data_prelim),
                                    Value = plot_data_prelim_mtx,
                                    stringsAsFactors = FALSE)
  colnames(plot_data_prelim_df) <- c("Period",colnames(plot_data_prelim))
  plot_data_final <- tidyr::gather(plot_data_prelim_df, Security, CumRet, -Period)
  
  YearMonthDay <- function(x) format(x, "%Y-%m-%d")
  my_plot <- ggplot(plot_data_final, aes(x = Period, y = CumRet, color = Security)) +
    geom_point() + 
    geom_line() +
    #scale_x_date(date_breaks = "1 day", labels = YearMonthDay) +
    ggtitle(paste0("Cumulative Return for ", market, " Market")) +
    labs(caption = paste0("Plot produced on ", Sys.time()))
    theme(rect = element_rect(fill = "#C0C0C0"),
          panel.background = element_rect(fill = "#C0C0C0"),
          legend.key = element_rect(fill = "#C0C0C0"),
          legend.position = "bottom",
          text = element_text(color = "#000000"),
          axis.text = element_text(color = "#000000"),
          axis.ticks = element_line(color = "#000000"),
          axis.text.x = element_text(color = "#000000", angle = 45))
  
  return(my_plot)
}

#
# Get lastest quote
#
UtilGetStockLastestPrice <- function(ticker_w_crncy){
  
  pos <- regexpr("-", ticker_w_crncy)[1]
  if(pos == -1){
    ticker <- ticker_w_crncy
  } else {
    ticker <- substr(ticker_w_crncy, 1, pos-1)
    currency <- substr(ticker_w_crncy, pos+1, nchar(ticker_w_crncy))
    if(currency == "CAD"){
      ticker <- paste0(ticker,".TO")
    } 
  }
  
  lprc_prelim <- getSymbols(ticker, auto.assign = FALSE)
  lprc_final <- lprc_prelim[nrow(lprc_prelim),1:5]
  colnames(lprc_final) <- c("Open", "High", "Low", "Close", "Volume")
  
  return(lprc_final)
}

#
# Economic Indicators functions
#
UtilGetEconIndicators <- function(){
  
  #
  # Setup run parameters
  #
  end.date <- Sys.Date() - days(day(Sys.Date())) + 1
  mth.seq <- rev(seq(from = end.date, length = 12, by="-1 month")) 
  start.date <- mth.seq[1]
  
  # Fred items
  dict.keys <- c("kPI",
                 "kPCE",
                 "kDPI",
                 "kMDGNO",
                 "kNCGeACNO",
                 "kNCGNO",
                 "kBP",
                 "kHS",
                 "kRFSS",
                 "kISR4W",
                 "kGDP",
                 "kRGDP",
                 "kCCO",
                 "kIP",
                 "kCU",
                 "kM_2",
                 "kROS",
                 "kOER",
                 "kS",
                 "kCPI",
                 "kECI",
                 "kAW4M",
                 "kOT4M",
                 "kTNFP",
                 "kUR",
                 "kHPI",
                 "kCIL",
                 "kFFR",
                 "kT10YFF",
                 "kEHS",
                 "kSP_500",
                 "kVIX",
                 "kCFNAI",
                 "kIJC",
                 "kICS",
                 "kSTLFSI")
  dict.values <- c("PI",
                   "PCE",
                   "DSPI",
                   "DGORDER",
                   "NEWORDER",
                   "ANDENO",
                   "PERMIT",
                   "HOUST",
                   "RSAFS",
                   "WHLSLRIRSA",
                   "GDP",
                   "GDPC96",
                   "TOTALSL",
                   "INDPRO",
                   "TCU",
                   "M2",
                   "CUSR0000SAS2RS",
                   "CUSR0000SEHC01",
                   "CUSR0000SAS",
                   "CPIAUCSL",
                   "ECIALLCIV",
                   "AWHAEMAN",
                   "CES3000000004",
                   "PAYEMS",
                   "UNRATE",
                   "USSTHPI",
                   "BUSLOANS",
                   "FF",
                   "T10YFF",
                   "EXHOSLUSM495N",
                   "SP500",
                   "VIXCLS",
                   "CFNAI",
                   "ICSA",
                   "UMCSENT",
                   "STLFSI")
  names(dict.values) <- dict.keys
  
  # Quandl items
  qdict.keys <- c("kPMI", "kSD")
  qdict.values <- c("ISM/MAN_PMI", "ISM/MAN_DELIV")
  names(qdict.values) <- qdict.keys
  
  ###########################################################################
  ############################# Monthly Data ################################
  ###########################################################################
  ei1 <- EconomicIndicators(id = 1, fred_items = dict.values, quandl_items = qdict.values,
                            hist_startdate = start.date, hist_enddate = end.date)
  # 
  # Download economic items
  ei1 <- EIDownloadAllFredItems(ei1)
  ei1 <- EIDownloadAllQuandlItems(ei1)
  
  # 
  # Data aggregation for monthly data
  #
  ei.mthly <- merge.xts(ei1$EI_fred_data,
                        ei1$EI_quandl_data,
                        all= TRUE)
  
  #
  # Format monthly data
  #
  res <- lapply(1:length(mth.seq), function(j, mth.seq){
    
    bom <- mth.seq[j]
    yr <- year(bom)
    mh <- month(bom)
    
    if(mh == 12){
      eom <- as.Date(paste(yr+1, "-", "01", "-01", sep="")) - 1
    } else {
      eom <- as.Date(paste(yr, "-", mh+1, "-01", sep="")) - 1
    }
    prd <- paste(bom, eom, sep="/")
    
    sub.data <- ei.mthly[prd]
    month.mean <- colMeans(sub.data, na.rm = TRUE)
    month.mean <- sapply(month.mean, function(x){ format(round(x, 2), nsmall=2, big.mark=",") })
    res <- as.data.frame(month.mean)
    colnames(res) <- names(res)
    
    return(res)
  }, mth.seq)
  pd.mthly.output <- dplyr::bind_cols(res)
  pd.mthly.output[is.na(pd.mthly.output)] <- ""
  colnames(pd.mthly.output) <- mth.seq
  pd.mthly.output$Key <- c(dict.keys, qdict.keys)
  
  #
  # Load data from excel
  #
  gei_lookup <- readxl::read_excel(ei.fn, sheet = "General")
  lei_lookup <- readxl::read_excel(ei.fn, sheet = "Leading")
  coi_lookup <- readxl::read_excel(ei.fn, sheet = "Coincident")
  lai_lookup <- readxl::read_excel(ei.fn, sheet = "Lagging")
  
  #
  # Merge Data
  #
  gei_data <- merge.data.frame(gei_lookup, pd.mthly.output, by="Key")
  lei_data <- merge.data.frame(lei_lookup, pd.mthly.output, by="Key")
  coi_data <- merge.data.frame(coi_lookup, pd.mthly.output, by="Key")
  lai_data <- merge.data.frame(lai_lookup, pd.mthly.output, by="Key")
  
  #
  # Return data
  #
  return(list(gei_dt = gei_data,
              lei_dt = lei_data,
              coi_dt = coi_data,
              lai_dt = lai_data))
  
}

#
# Manual open & close connection
#
OpenCloseConn <- function(dirc = c("open", "close")){
  d <- match.arg(dirc)
  if(d == "open"){
    if(!TSIsConnected(ts_static)){
      ts_static <<- TradingSession(22, platform, acct)
    }
  } else {
    if(TSIsConnected(ts_static)){
      TSCloseTradingSession(ts_static)
    }
  }
}