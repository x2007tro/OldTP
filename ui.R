lib <- c("shiny","shinythemes","DT")
lapply(lib, function(x){library(x, character.only = TRUE)})
source("utility.R")

#
# UI Layout (Trader Portal)
#
# |     portfolio (macro_block)    |   Blotter (micro_block)   |        
#                                  |   Message (nano_block)   |
#      
# |     Market (macro_block)       |   Watchlist (macro_block)   |
# 
#
#
#

#
# Shiny ui
#
ui <- fluidPage(theme = shinytheme("lumen"),
  
  # Some custom CSS, testing something, 12
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
          height: 440px;
        }
        .nano_block {
          border: 1px solid #ccc;
          border-radius: 1px;
          padding: 0px 0px;
          margin: 1px 0px;
          height: 108px;
        }
        .blotter_fields {
          padding: 0px 5px;
          margin: 0px 5px;
          width: 80px;
          float: left;
        }
        .blotter_fields_right {
          padding: 0px 5px;
          margin: 0px 5px;
          width: 90px;
          float: right;
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
        .add_trade_list, .add_trade_list_submit{
          width:200px;
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
                                        fluidRow(      column(12,
                                            tags$h4("Blotter", style="float:left"),
                                            tags$div(id = "blotter_size_div", style="float:right",
                                                     selectInput("blotter_size_selector", NULL, choices = 1:10, width = blotter_field_default_width)
                                            )
                                        )),
                                        fluidRow(column(12,
                                                 tags$div(style="display:block",
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
                                                            fluidRow(column(12,
                                                                            tags$div(style="display:block", uiOutput(paste0('trade_item', i), inline = FALSE))
                                                            ))
                                                          })
                                                 ) 
                                        ))
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
                                      fluidRow(column(12,
                                                      tags$h4("Watchlist", style="float:left"),
                                                      tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                                                               actionButton("ticker_search_submit", "Get quote", width = blotter_field_default_width)
                                                      ),
                                                      tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                                                               textInput("ticker_search", NULL, value = "AAPL-USD", width = blotter_field_default_width)
                                                      )
                                      )),
                                      fluidRow(column(12,
                                                      tags$h5("Latest information"),
                                                      tags$div(style="padding:0px, margin:0px, height:100%",
                                                               textOutput("prev_day_quote")), br(),
                                                      tags$h5("Historical Performance (1 Year)"),
                                                      tags$div(style="padding:0px, margin:0px, height:100%",
                                                               plotOutput("hist_return"))
                                      ))
                             )
                      )
                      # End     
                      
               ) # End of column
             ) # End of fluidrow
    ), # End of trader portal tab
    tabPanel("Trade History",
             fluidRow(
               column(12,
                      fluidRow(
                        tags$h4(tags$b("Past Trades"))
                      ),
                      fluidRow(
                        DT::dataTableOutput("past_trades")
                      )
               )
             ),
             fluidRow(
               column(12,
                      fluidRow(
                        tags$h4(tags$b("Past Messages"))    
                      ),
                      fluidRow(
                        DT::dataTableOutput("past_messages")     
                      )
               )
             )
    ),
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
      # Economic indicators column
      fluidRow(
        column(12, id = "ei",
               tags$div(tabsetPanel(position = "below",
                                    
                                    # Equity curve
                                    tabPanel("General",
                                             column(econ_indi_panel_default_width,
                                                    DT::dataTableOutput("gei_dt"))
                                    ),
                                    # End
                                    
                                    # Equity curve
                                    tabPanel("Leading",
                                             column(econ_indi_panel_default_width,
                                                    DT::dataTableOutput("lei_dt"))
                                    ),
                                    # End
                                    
                                    # Bond
                                    tabPanel("Coincident",
                                             column(econ_indi_panel_default_width,
                                                    DT::dataTableOutput("coi_dt"))
                                    ),
                                    # End
                                    
                                    # Treasury yield curve
                                    tabPanel("Lagging",
                                             column(econ_indi_panel_default_width,
                                                    DT::dataTableOutput("lai_dt"))
                                    ),
                                    # End
                                    
                                    # Introduction to Economic Indicators
                                    tabPanel("What are Economic Indicators?",
                                             column(12,
                                                    br(),
                                                    tags$div("An economic indicator is a statistic about an economic activity. 
                                                              Economic indicators allow analysis of economic performance and predictions of future performance.
                                                              The indicators provided by this website are also referred to as The Conference Board Economic Indicators"),
                                                    br(),
                                                    tags$div("Useful links:"),
                                                    tags$ul(
                                                      tags$li(tags$a(href="https://www.investopedia.com/university/conferenceboard/", "The Conference Board Economic Indicators from Investopedia")),
                                                      tags$li(tags$a(href="https://www.conference-board.org/", "The Conference Board Official Website"))
                                                    ))
                                    )
                                    # End
                                    
                        )
               )      
        )
      )
      # End
    ),
    tabPanel("Development",
             tags$h5("Future features under development:"),
             tags$ul(
               tags$li("Add configuration page"),
               tags$li("Better looking theme"),
               tags$li("Add news search function"),
               tags$li("Clean up and reorganize UI"),
               tags$li("Add error handling in Server"),
               tags$li("Parameterize more items")
             ),
             br(), br(),
             tags$h5("Features developed"),
             tags$ul(
               tags$li("Add economic indicators"),
               tags$li("Automatically calculate trade values"),
               tags$li("Add historical trade/messages")
             )
    ),
    tabPanel("Configuration",
             tags$h5("Options")
    )
  )
  # End of navbarpage
)
