lib <- c("ggplot2", "tidyr", "readxl")
lapply(lib, function(x){library(x, character.only = TRUE)})

#
# Common parameters for server and ui
#
blotter_field_default_width <- "90px"
max_blotter_size <- 100
max_message_count <- 5


#
# Utility functions for shiny trading portal
#

#
# Load trading class
#
onedrive.dir <- "C:/Users/KE/OneDrive/"
cls.rpo.dir <- paste0(onedrive.dir, "Development/R/Repository/Class/")
shiny.dir <- paste0(onedrive.dir, "Development/Shiny/ShiyTraderPortal/")
setwd(cls.rpo.dir)

#
# Load portfolio status function
#
source("IB_TWS_TradingSession.R")
UtilGetPortfolio <- function(){
  ts1 <- TradingSession(1, "TWS")
  ts1 <- TSSetTransmit(ts1, FALSE)     #Prevert trade from actually happening
  ts1 <- TSRetrievePortHoldings(ts1)
  TSCloseTradingSession(ts1)
  port_prelim <- ts1$ts_port_holdings

  update_time <- port_prelim$TimeStamp[1]  # Output 1
  
  port_prelim <- port_prelim[port_prelim$LocalTicker != "USD.CAD",]
  port_prelim$Ticker <- paste0(port_prelim$LocalTicker, "-", port_prelim$Currency)
  
  holdings <- port_prelim[,"Ticker"]  # Output 2
  
  port_prelim$UnrealizedPNLPrc <- paste0(round(port_prelim$UnrealizedPNL/(port_prelim$Position*port_prelim$AvgCost)*100,2),"%")
  port_prelim$Cost <- round(port_prelim$AvgCost, 2)
  port_prelim$Position <- round(port_prelim$Position, 2)
  
  port_intrim <- port_prelim[,c("Ticker", "SecurityType", "Position", "Cost", "UnrealizedPNL", "UnrealizedPNLPrc")]
  
  us_cash <- ts1$ts_us_cash_balance
  port_us_cash <- data.frame(Ticker = "USD",
                             SecurityType = "CASH",
                             Position = round(us_cash,2),
                             Cost = 0,
                             UnrealizedPNL = 0,
                             UnrealizedPNLPrc = "0.00%",
                             stringsAsFactors = FALSE)
  
  ca_cash <- ts1$ts_ca_cash_balance
  port_ca_cash <- data.frame(Ticker = "CAD",
                             SecurityType = "CASH",
                             Position = round(ca_cash,2),
                             Cost = 0,
                             UnrealizedPNL = 0,
                             UnrealizedPNLPrc = "0.00%",
                             stringsAsFactors = FALSE)
  
  port <- dplyr::bind_rows(list(port_intrim, port_us_cash, port_ca_cash))  # Output 3
  
  return(list(update_datetime = update_time,
              holdings = holdings,
              portfolio = port))
}

#
# Find current holding
#
UtilFindCurrentHolding <- function(ticker_with_current){
  port <- UtilGetPortfolio()$port
  holding <- port[port$Ticker == ticker_with_current,]
  
  if(nrow(holding) == 0){
	pos <- 0
  } else {
	pos <- holding[,"Position"]
  }
}

#
# Trade functions
#
UtilTradeWithIB <- function(blotter){
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
		ts1 <- TradingSession(1, "TWS")
		ts1 <- TSSetTransmit(ts1, transmit)     
		ts1 <- TSSetPrelimTradeList(ts1, blotter)
		ts1 <- TSGenFnlTradeList(ts1)
		ts1 <- TSExecuteAllTrades(ts1)
		err_msg <- ts1$ts_last_trade_message
		TSCloseTradingSession(ts1)
		
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
			msg <- data.frame(Date = trade_date,
							  Time = trade_time,
							  Msg = paste0(tik_with_crcy, " is successfully traded (", side, ") at ",
											trade_date, " ", trade_time),
							  stringsAsFactors = FALSE)
		} else {
			trade_res$Date <- trade_date
			trade_res$Time <- trade_time
			trade_res$Result <- "Failed"
			msg <- data.frame(Date = trade_date,
							  Time = trade_time,
							  Msg = paste0(tik_with_crcy, " is not traded (", side, ") at ",
											trade_date, " ", trade_time, " : ", err_msg),
							  stringsAsFactors = FALSE)
		}
	
	}
  return(list(trade_rec = trade_res, msg_rec = msg))
}

# blotter <- data.frame(LocalTicker = "SPY",
#                       Action = "Buy",
#                       Quantity = 1,
#                       OrderType = "Mkt",
#                       LimitPrice = 0,
#                       SecurityType = "Stk",
#                       Currency = "USD",
#                       TradeSwitch = FALSE,
#                       stringsAsFactors = FALSE)
# res <- UtilTradeWithIB(blotter)

#
# Download etf historical price and calculate return
#
source("FinancialSecurityHistoricalData.R")
source("EconomicIndicators.R")
UtilGetMarketReturn <- function(){
  #
  # Setup
  #
  start.date <- as.Date("2013-01-01") 
  end.date <- Sys.Date()
  invt.dir <- paste0(onedrive.dir, "Investment/")
  ws.fn <- paste(invt.dir,"Research/Economic Indicators/",
                 "Watchlist.xlsx",sep="")
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
  plot_data_final <- tidyr::gather(plot_data_prelim_df, ETF, CumRet, -Period)
  
  YearMonthDay <- function(x) format(x, "%Y-%m-%d")
  my_plot <- ggplot(plot_data_final, aes(x = Period, y = CumRet, color = ETF)) +
    geom_point() + 
    geom_line() +
    #scale_x_date(date_breaks = "1 day", labels = YearMonthDay) +
    ggtitle(paste0("Cumulative Return for ", market, " Market")) +
    labs(caption = paste0("Plot produced on ", Sys.time()))
    theme(rect = element_rect(fill = "#C0C0C0"),
          panel.background = element_rect(fill = "#C0C0C0"),
          legend.key = element_rect(fill = "#C0C0C0"),
          text = element_text(color = "#000000"),
          axis.text = element_text(color = "#000000"),
          axis.ticks = element_line(color = "#000000"),
          axis.text.x = element_text(color = "#000000", angle = 45))
  
  return(my_plot)
}
x <- UtilGetStockHistReturn("SPY-USD")
y <- UtilPlotMarketReturn(x, "HAHAHA", "1Y")
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
# Set shiny directory
#
setwd(shiny.dir)