lib <- c("shiny","shinythemes","DT")
lapply(lib, function(x){library(x, character.only = TRUE)})
source("utility.R")

#
# Shiny ui
#
ui <- fluidPage(theme = shinytheme("united"),
  
  # Some custom CSS
  tags$head(
    tags$style(HTML("
        /* Smaller font for preformatted text */
        pre, table.table {
          font-size: smaller;
        }
        body {
          font-size: 12px;
          font-weight: bold;
          /*color: #ffffff;*/
          /*background: #000000;*/
        }
        .macro_block {
          border: 1px solid #ccc;
          border-radius: 1px;
          padding: 0px 0px;
          margin: 1px 0px;
          height: 550px;
        }
        .micro_block {
          border: 1px solid #ccc;
          border-radius: 1px;
          padding: 0px 0px;
          margin: 1px 0px;
          height: 400px;
        }
        .nano_block {
          border: 1px solid #ccc;
          border-radius: 1px;
          padding: 0px 0px;
          margin: 1px 0px;
          height: 148px;
        }
        .blotter_fields {
          padding: 0px 5px;
          margin: 0px 5px;
          width: 90px;
          float: left;
        }
        .blotter_fields_right {
          padding: 0px 5px;
          margin: 0px 5px;
          width: 90px;
          float: right;
        }
        #ticker1, #currency1, #side1, #shares1, #type1, #limit_price1, 
        #trade_value1, #transmit1, #trade1 {
          padding: 1px 5px;
          margin: 0px 0px;
          height: 100%;
        }
        #ticker02, #currency02, #side02, #shares02, #type02, #limit_price02, 
        #trade_value02, #transmit02, #trade02 {
          padding: 1px 5px;
          margin: 0px 0px;
          height: 100%;
        }
        #ticker03, #currency03, #side03, #shares03, #type03, #limit_price03, 
        #trade_value03, #transmit03, #trade03 {
          padding: 1px 5px;
          margin: 0px 0px;
          height: 100%;
        }
        #ticker04, #currency04, #side04, #shares04, #type04, #limit_price04, 
        #trade_value04, #transmit04, #trade04 {
          padding: 1px 5px;
          margin: 0px 0px;
          height: 100%;
        }
        #ticker05, #currency05, #side05, #shares05, #type05, #limit_price05, 
        #trade_value05, #transmit05, #trade05 {
          padding: 1px 5px;
          margin: 0px 0px;
          height: 100%;
        }
        #ticker_search, #ticker_search_submit, #ticker_news, #ticker_news_submit,
        #eq_perf_period, #eq_perf_period_submit, #tb_perf_period, #tb_perf_period_submit, 
        #cb_perf_period, #cb_perf_period_submit, #add_trade_list, #add_trade_list_submit {
          padding: 0px 0px;
          margin: 0px 0px;
          height: 35px;
        }
        #portfolio_dt{
          /*background-color: #000000;*/
        }
        .trade_item1, .trade_item2{
          diplay:block;
        }
      "))
  ),
  
  navbarPage("Navigation",
    tabPanel("Trader Portal",
             # title
             # titlePanel("Trader Portal"),
             
             # First row
             fluidRow(
               
               # Portfolio Column
               column(6, id = "portfolio", style = "padding:0px 1px 0px 10px",
                      tags$div(class = "macro_block",
                               tags$h4("Portfolio", style="float:left"),
                               tags$h5(textOutput("last_update_time"), style="float:right"),
                               tags$div(
                                 DT::dataTableOutput("portfolio_dt")
                               ),
                               tags$div(style="float:left; padding:0px, margin:0px, height:50%",
                                        selectInput("add_trade_list", NULL, choices = c("CAD"), width = blotter_field_default_width)
                               ),
                               tags$div(style="float:left; padding:0px, margin:0px, height:50%",
                                        actionButton("add_trade_list_submit", "Add to blotter", width = blotter_field_default_width)
                               )
                      )
               ),
               # End
               
               # Blotter/Message Column
               column(6, style = "padding:0px 10px 0px 1px",
                      
                      fluidRow(
                        # Blotter Column
                        column(12,id = "blotter",
                               tags$div(class = "micro_block",
                                        tags$h4("Blotter", style="float:left"),
                                        tags$div(id = "blotter_size_div", style="float:right",
                                                 selectInput("blotter_size_selector", NULL, choices = 1:10, width = blotter_field_default_width)
                                        ),
                                        tags$table(id = "blotter_table",
                                                     tags$div(class = "blotter_fields", "Ticker"),
                                                     tags$div(class = "blotter_fields", "Currency"),
                                                     tags$div(class = "blotter_fields", "Side"),
                                                     tags$div(class = "blotter_fields", "Shares"),
                                                     tags$div(class = "blotter_fields", "Type"),
                                                     tags$div(class = "blotter_fields", "Limit Price"),
                                                     tags$div(class = "blotter_fields", "Trade Value"),
                                                     tags$div(class = "blotter_fields", "Transmit"),
                                                     tags$div(class = "blotter_fields", "Trade"),
                                                     br(),

                                                   lapply(1:max_blotter_size, function(i){
                                                     tags$div(style="display:block", uiOutput(paste0('trade_item', i), inline = FALSE, style="display:block"))
                                                   })
                                        ),
                                        tags$br()
                               )
                        )
                        # End
                      ),
                      
                      fluidRow(
                        # Message Column
                        column(12, id = "message",
                               tags$div(class = "nano_block",
                                        tags$h4("Message"),
                                        lapply(1:max_message_count, function(i){
                                          tags$div(style="display:block", textOutput(paste0('message', i), inline = FALSE))
                                        })
                               )
                        )
                        # End
                      )
               )
               # End
               
             ),
             
             # Second row
             fluidRow(
               
               # Market Column
               column(6, id = "market", style = "padding:0px 1px 0px 10px",
                      tags$div(class = "macro_block",
                               tags$h4("Market", tyle="float:left"),
                               tabsetPanel(position = "below",
                                           # Equity curve
                                           tabPanel("Equity",
                                                    tags$div(style="float:right; padding:0px, margin:0px, height:50%",
                                                             selectInput("eq_perf_period", NULL, choices = c("5D", "1M", "3M", "6M", "1Y", "3Y", "5Y", "YTD"), width = blotter_field_default_width)
                                                    ),
                                                    plotOutput("equity_mkt")
                                                    
                                           ),
                                           # End
                                           
                                           # Bond
                                           tabPanel("Treasury bond",
                                                    tags$div(style="float:right; padding:0px, margin:0px, height:50%",
                                                             selectInput("tb_perf_period", NULL, choices = c("5D", "1M", "3M", "6M", "1Y", "3Y", "5Y", "YTD"), width = blotter_field_default_width)
                                                    ),
                                                    plotOutput("tbond_mkt")
                                           ),
                                           # End
                                           
                                           # Treasury yield curve
                                           tabPanel("Corporate bond",
                                                    tags$div(style="float:right; padding:0px, margin:0px, height:50%",
                                                             selectInput("cb_perf_period", NULL, choices = c("5D", "1M", "3M", "6M", "1Y", "3Y", "5Y", "YTD"), width = blotter_field_default_width)
                                                    ),
                                                    plotOutput("cbond_mkt")
                                           )
                                           # End
                               ),
                               tags$br(),
                               tags$br(),
                               tags$br()
                      )
               ),
               # End
               
               # Watchlist Column
               column(6, style = "padding:0px 10px 0px 1px",
                      
                      # Watchlist Column
                      column(12, id = "watchlist", style = "padding:0px 1px 0px 1px",
                             tags$div(class = "macro_block",
                                      tags$h4("Watchlist", style="float:left"),
                                      tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                                               actionButton("ticker_search_submit", "Get quote", width = blotter_field_default_width)
                                      ),
                                      tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                                               textInput("ticker_search", NULL, value = "", width = blotter_field_default_width, placeholder = "AAPL-USD")
                                      ),
                                      tags$div(style="padding:0px, margin:0px, height:100%",
                                               textOutput("prev_day_quote")),
                                      tags$div(style="padding:0px, margin:0px, height:100%",
                                               plotOutput("hist_return"))
                                      
                             )
                      )
                      # End     
                      
               ) # End of column
             ) # End of fluidrow
    ), # End of trader portal tab
    tabPanel("Market News",
        # News column
        fluidRow(
          column(12, id = "news",
              tags$div(class = "nano_block",
                       tags$h4("News", style="float:left"),
                       tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                                actionButton("ticker_news_submit", "Get news", width = blotter_field_default_width)
                       ),
                       tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                                textInput("ticker_news", NULL, value = "", width = blotter_field_default_width)
                       )
              )
          )
        ) # End
    ),
    tabPanel("Economic Indicators",
      tags$h4("Here are the instructions!"),
      # Economic indicators column
      fluidRow(
        column(12, id = "ei",
               tags$div(#class = "macro_block",
                        tags$h4("Economic Indicators"),
                        
                        tabsetPanel(position = "below",
                                    # Equity curve
                                    tabPanel("Leading"
                                    ),
                                    # End
                                    
                                    # Bond
                                    tabPanel("Coincident"
                                    ),
                                    # End
                                    
                                    # Treasury yield curve
                                    tabPanel("Lagging"
                                    )
                                    # End
                        ),
                        
                        tags$br()
               )      
        )
      )
      # End
    ),
    tabPanel("Instructions",
             tags$h4("Here are the instructions!")
    )
  )
  # End of navbarpage
)
