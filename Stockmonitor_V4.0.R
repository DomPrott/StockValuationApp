library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)

library(dplyr)
library(DT)
library(reactablefmtr)
library(plotly)
library(tidyverse)
library(quantr)
library(tidyquant)
library(ggplot2)
library(quantmod)
library(scales)
library(shinyauthr)
library(rlang)
library(TTR)
library(caret)
library(smooth)

library(rugarch)
library(forecast)
library(tseries)
library(prophet)




#####################################

options(digits=4)

options(scipen = 10)

growth_rate <- function(data){
  round((data-lag(data))/lag(abs(data)), digits = 3)*100
}


#####################################

shinyApp(
  
  ui = dashboardPage(skin="midnight",
                     
                     dashboardHeader(title = HTML(paste0(icon("fas fa-landmark"), " Stock Valuation"))),
                     
                     
                     dashboardSidebar(
                       sidebarMenu(id = "sidebarid",
                                   textInput("ticker", "Insert ticker symbol", value = "AMZN"),
                                   helpText(div(style = "text-align: center;", "Please use capital letters")),
                                   menuItem("Chart", tabName = "chart", icon = icon("fas fa-chart-line")),
                                   menuItem("Forecasting", tabName = "analysis", icon = icon("fas fa-chart-line")),
                                   menuItem("DCF", tabName = "dcf", icon = icon("fas fa-square-poll-vertical")),
                                   menuItem("Earnings Power Value", tabName = "epv", icon = icon("fas fa-square-poll-vertical")),
                                   conditionalPanel(
                                     condition = "input.sidebarid == 'chart'",
                                     helpText(""),
                                     
                                     #dateInput("date", "Starting date"),
                                     #selectInput("dateBreaks","Interval", 
                                     #            choices = c("1 M"=1, "3 M"=3, "6 M"=6, 
                                     #                        "1 Y"=12, "5 Y"=60, "Max"= 2)),
                                     
                                     
                                     radioGroupButtons("type", "Choices", 
                                                       choices = c("Chart"=1, "Candlestick"=2),
                                                       status = "success"
                                     ),
                                     
                                     
                                     dateRangeInput("dateRange", "Choose an interval", start = today()-30, end = today()),
                                     
                                     radioGroupButtons("interval", 
                                                       choices = c("1 M"=1, "6 M"=2, "1 Y"=3, "5 Y"=4),
                                                       status = "success"
                                     ),
                                     
                                     hr(),
                                     radioButtons("indicator", "Indicators",
                                                  choices = c("Price"=1, "Simple Moving Average"=2, "Bollinger Bands"=3,
                                                              "Exponential Moving Average"=4, "MACD"=5, "ALL"=6)),
                                     ###########
                                     useShinyjs(),
                                     div(style="display:inline-block;width:87%;text-align: center;", actionButton("button", "Compute", class = "btn-block")) #align button
                                   ),
                                   conditionalPanel(
                                     condition = 'input.sidebarid == "analysis"',
                                     helpText(div(style = "text-align: left;", HTML("  1. Double-click to zoom in and out"))),
                                     helpText(div(style = "text-align: left;", HTML("  2. If error, press Predict again"))),
                                     helpText(div(style = "text-align: left;", HTML("  3. Prophet may take some time"))),
                                     sliderInput("horizon", "Horizon", min = 1, max = 90, value = 10),
                                     div(style="display:inline-block;width:87%;text-align: center;", actionButton("button_predict", "Predict", class = "btn-block")) #align button
                                   ),
                                   conditionalPanel(
                                     condition = "input.sidebarid == 'dcf'",
                                     helpText("In Percent"),
                                     radioButtons("scenarios", "FCF Growth Scenarios",
                                                  choices = c("Optimistic"=1, "3-year Average"=2, "Pessimistic"=3)),
                                     sliderInput("mos", "Margin of Safety [in %]", min = 0, max = 30, value = 20),
                                     sliderInput("industry_growth", "Avg. Industry Growth [in %]", min = 0, max = 20, value = 5),
                                     sliderInput("discount", "Discount Rate [in %]", min = 0, max = 30, value = 8),
                                     sliderInput("dcf_horizon", "Horizon", min = 2, max = 10, value = 5),
                                     div(style="display:inline-block;width:87%;text-align: center;", actionButton("button_dcf", "Compute", class = "btn-block"))
                                   ),
                                   conditionalPanel(
                                     condition = 'input.sidebarid == "epv"',
                                     helpText(HTML("")),
                                   )
                       )
                     ),
                     dashboardBody(
                       fluidRow(
                         
                         uiOutput("box1"),
                         uiOutput("box2"),
                         uiOutput("box3"),
                         uiOutput("box4")
                         
                         
                       ),
                       
                       tabItems(
                         tabItem(tabName = "chart",
                                 valueBoxOutput("reward", width = 3),
                                 valueBoxOutput("risk", width = 3),
                                 
                                 valueBoxOutput("pe_ratio", width = 3),
                                 valueBoxOutput("rsi", width = 3),
                                 fluidRow(
                                   uiOutput("boxtitle1"),
                                   uiOutput("boxtitle2")
                                 )
                         ),
                         # Second tab content
                         tabItem(tabName = "analysis",
                                 tabsetPanel(type = "tabs",
                                             tabPanel("ARIMA", 
                                                      fluidRow(
                                                        column(12,
                                                               uiOutput("arima_box1") %>% withSpinner(),
                                                               uiOutput("arima_box2") %>% withSpinner()
                                                        ),
                                                        column(12,
                                                               uiOutput("arima_box3") %>% withSpinner(),
                                                               uiOutput("arima_box4") %>% withSpinner()     
                                                        )       
                                                      )
                                             ),
                                             tabPanel("Prophet", 
                                                      fluidRow(
                                                        column(12,
                                                               uiOutput("proph_box1") %>% withSpinner(),
                                                               uiOutput("proph_box2") %>% withSpinner()
                                                        )     
                                                      )
                                             ),
                                 )
                         ),
                         tabItem(tabName = "dcf",
                                 fluidRow(
                                   column(12,
                                          uiOutput("dcf_box1"),
                                          uiOutput("dcf_box2")
                                   ),
                                   
                                   column(12,
                                          uiOutput("dcf_box3"),
                                          uiOutput("dcf_box4")
                                   )
                                 )
                         ),
                         tabItem(tabName = "epv",
                                 fluidRow(
                                   column(12,
                                          #uiOutput()
                                   ),
                                   
                                   column(12,
                                          #uiOutput()
                                   )
                                 )
                         )
                       )
                     )
  ),
  
  #####################################
  
  server = function(input, output, session){
    
    shinyjs::click("button")
    
    observeEvent(input$interval, {
      
      if(input$interval == 1){
        updateDateRangeInput(session, "dateRange",
                             start = Sys.Date() - 30, end = Sys.Date())
      } else if(input$interval == 2){
        updateDateRangeInput(session, "dateRange",
                             start = Sys.Date() - 180, end = Sys.Date())
      } else if(input$interval == 3){
        updateDateRangeInput(session, "dateRange",
                             start = Sys.Date() - 365, end = Sys.Date())
      } else if(input$interval == 4){
        updateDateRangeInput(session, "dateRange",
                             start = Sys.Date() - 1825, end = Sys.Date())
      }
      
    })
    
    ####################################################################
    
    output$boxtitle1 = renderUI({
      req(input$ticker)
      box(title = paste0(getQuote(paste(input$ticker, sep="", collapse=";"), 
                                  what=yahooQF(c("Price/Sales", 
                                                 "P/E Ratio",
                                                 "Price/EPS Estimate Next Year",
                                                 "PEG Ratio",
                                                 "Dividend Yield", 
                                                 "Market Capitalization",
                                                 "Shares Outstanding",
                                                 "Name"))) %>% select("Name"), " Closing Prices"), plotlyOutput("plot", height = "42em"), 
          solidHeader = T)
    })
    
    output$boxtitle2 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," Data Table"), reactableOutput("table2", height = "42em"), solidHeader = T)
    })
    #####################################################
    
    stock <- eventReactive(input$button, { #data from yahoo
      req(input$ticker, input$dateRange)
      
      tq_get(input$ticker, 
             get  = "stock.prices",
             from = input$dateRange,
             to   = today(),
             complete_cases = F)
    }
    )
    
    
    bb <- eventReactive(input$button, { #combine stock data with Bollinger Band columns
      req(input$ticker, input$dateRange)
      
      cbind(as.data.frame(tq_get(input$ticker, 
                                 get  = "stock.prices",
                                 from = input$dateRange,
                                 to   = today(),
                                 complete_cases = F)),
            as.data.frame(tq_get(input$ticker, #add bollinger bands to stock data in dataframe
                                 get  = "stock.prices",
                                 from = input$dateRange,
                                 to   = today(),
                                 complete_cases = F)$close) %>% BBands(n=10,sd=2)#range of 5, sd of 2
      )
    }
    )
    
    #######################################################
    
    nasdaq <- eventReactive(input$button, { #data from yahoo
      req(input$ticker, input$dateRange)
      
      tq_get("^IXIC", 
             get  = "stock.prices",
             from = input$dateRange,
             to   = today(),
             complete_cases = F)
    }
    ) 
    
    dax <- eventReactive(input$button, { #data from yahoo
      req(input$ticker, input$dateRange)
      
      tq_get("^GDAXI", 
             get  = "stock.prices",
             from = input$dateRange,
             to   = today(),
             complete_cases = F)
    }
    )
    
    US_bond <- eventReactive(input$button, { #data from yahoo
      req(input$ticker, input$dateRange)
      
      tq_get("IRLTLT01USM156N", 
             get  = "economic.data",
             from = "2020-01-01", #arbitrary, just show the last two entries
             to   = today(),
             complete_cases = F)
    }
    ) 
    
    ######################################################
    
    range1 <- eventReactive(input$button, {
      
      data.frame(absolute = last(stock()$close) - first(stock()$close),
                 percent = (last(stock()$close) - first(stock()$close))/first(stock()$close)*100,
                 volume = (last(stock()$volume) - first(stock()$volume))/first(stock()$volume))
    }
    )
    
    range2 <- eventReactive(input$button, {
      
      data.frame(absolute = last(nasdaq()$close) - first(nasdaq()$close),
                 percent = (last(nasdaq()$close) - first(nasdaq()$close))/first(nasdaq()$close)*100,
                 volume = (last(nasdaq()$volume) - first(nasdaq()$volume))/first(nasdaq()$volume))
    }
    )
    
    range3 <- eventReactive(input$button, {
      
      data.frame(absolute = last(dax()$close) - first(dax()$close),
                 percent = (last(dax()$close) - first(dax()$close))/first(dax()$close)*100,
                 volume = (last(dax()$volume) - first(dax()$volume))/first(dax()$volume))
    }
    )
    
    range4 <- eventReactive(input$button, {
      
      data.frame(absolute = first(tail(US_bond()$price, n=1) - tail(US_bond()$price, n=2)),
                 percent = first(tail(US_bond()$price, n=1) - tail(US_bond()$price, n=2))/first(tail(US_bond()$price, n=2)))
    }
    )
    
    ########################################################
    
    output$box1 = renderUI({
      req(input$ticker, input$dateRange)
      box(width = 3,
          height = 12,
          title = paste0(getQuote(paste(input$ticker, sep="", collapse=";"), 
                                  what=yahooQF(c("Price/Sales", 
                                                 "P/E Ratio",
                                                 "Price/EPS Estimate Next Year",
                                                 "PEG Ratio",
                                                 "Dividend Yield", 
                                                 "Market Capitalization",
                                                 "Shares Outstanding",
                                                 "Name"))) %>% select("Name")),
          div(style="text-align: center;", 
              h4(paste0(stock()$close %>% 
                          last() %>% 
                          round(digits = 2), " USD"))
          ),
          
          div(style="text-align: center;", 
              paste0(paste(ifelse(range1()$absolute >= 0, "+", "-"), round(abs(range1()$absolute), 2), sep = ""), " (", 
                     paste(ifelse(range1()$percent >= 0, "+", "-"), round(abs(range1()$percent), 2), sep = ""), 
                     "%)"
              ),
              style = ifelse(range1()$absolute < 0, "color: red;", "color: green;"),
              #icon = ifelse(round(range2(), digits = 2) < 0, icon("arrow-down", class = "fa-lg"))
          ),
          
          div(style="text-align: center;", 
              paste0(input$dateRange, collapse = " to ")
          ),
          
          footer = div(style="text-align: right;", 
                       paste0("Volume: ", last(stock()$volume), " (", 
                              paste(ifelse(range1()$volume >= 0, "+", "-"), round(abs(range1()$volume), 2), sep = ""), 
                              "%)")
                       
          )
          
      )
      
      
    })
    
    output$box2 = renderUI({
      req(input$dateRange)
      box(width = 3,
          height = 12,
          title = paste0("NASDAQ (Ticker: ^IXIC)"),
          div(style="text-align: center;", 
              h4(tq_get("^IXIC",
                        get  = "stock.prices",
                        complete_cases = F)$close %>% 
                   last() %>% 
                   round(digits = 2))
          ),
          
          div(style="text-align: center;", 
              paste0(paste(ifelse(range2()$absolute >= 0, "+", "-"), round(abs(range2()$absolute), 2), sep = ""), " (", 
                     paste(ifelse(range2()$percent >= 0, "+", "-"), round(abs(range2()$percent), 2), sep = ""), 
                     "%)"
              ),
              style = ifelse(range2()$absolute < 0, "color: red;", "color: green;"),
              #icon = ifelse(round(range2(), digits = 2) < 0, icon("arrow-down", class = "fa-lg"))
          ),
          
          div(style="text-align: center;", 
              paste0(input$dateRange, collapse = " to ")
          ),
          
          footer = div(style="text-align: right;", 
                       paste0("Volume: ", last(nasdaq()$volume), " (", 
                              paste(ifelse(range2()$volume >= 0, "+", "-"), round(abs(range2()$volume), 2), sep = ""), 
                              "%)")
                       
          )
          
      )
      
      
    })
    
    output$box3 = renderUI({
      req(input$dateRange)
      box(width = 3,
          height = 12,
          title = paste0("DAX (Ticker: ^GDAXI)"),
          div(style="text-align: center;", 
              h4(tq_get("^GDAXI",
                        get  = "stock.prices",
                        complete_cases = F)$close %>% 
                   last() %>% 
                   round(digits = 2))
          ),
          
          div(style="text-align: center;", 
              paste0(paste(ifelse(range3()$absolute >= 0, "+", "-"), round(abs(range3()$absolute), 2), sep = ""), " (", 
                     paste(ifelse(range3()$percent >= 0, "+", "-"), round(abs(range3()$percent), 2), sep = ""), 
                     "%)"
              ),
              style = ifelse(range3()$absolute < 0, "color: red;", "color: green;"),
              #icon = ifelse(round(range2(), digits = 2) < 0, icon("arrow-down", class = "fa-lg"))
          ),
          
          div(style="text-align: center;", 
              paste0(input$dateRange, collapse = " to ")
          ),
          
          footer = div(style="text-align: right;", 
                       paste0("Volume: ", last(dax()$volume), " (", 
                              paste(ifelse(range3()$volume >= 0, "+", "-"), round(abs(range3()$volume), 2), sep = ""), 
                              "%)")
                       
          )
          
      )
      
      
    })
    
    output$box4 = renderUI({
      req(input$dateRange)
      box(width = 3,
          height = 12,
          title = paste0("10-year US Government Bond Yield"),
          div(style="text-align: center;", 
              h4(US_bond()$price %>% last() %>% round(digits = 2))
          ),
          
          div(style="text-align: center;", 
              paste0(paste(ifelse(range4()$absolute >= 0, "+", "-"), round(abs(range4()$absolute), 2), sep = ""), " (", 
                     paste(ifelse(range4()$percent >= 0, "+", "-"), round(abs(range4()$percent), 2), sep = ""), 
                     "%)"
              ),
              style = ifelse(range4()$absolute < 0, "color: red;", "color: green;"),
              #icon = ifelse(round(range2(), digits = 2) < 0, icon("arrow-down", class = "fa-lg"))
          ),
          
          div(style="text-align: center;", 
              paste0(first(tail(US_bond()$date, n=2)), " to ", last(US_bond()$date))
          ),
          
          footer = div(style="text-align: right;", 
                       paste0("FRED ID: IRLTLT01USM156N")
                       
          )
          
      )
      
      
    })
    
    
    ####################################################
    
    plotses <- eventReactive(input$button, {
      #observeEvent(input$type, input$indicator)
      if (input$type == 1 && input$indicator == 1){ #classic chart
        return(stock() %>% 
                 plot_ly(x = ~date,
                         y = ~close,
                         type = "scatter", mode = "lines") %>% 
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = "")) %>%  
                 #layout(xaxis = list(showticklabels = FALSE)) %>% 
                 config(displayModeBar = F) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 1 && input$indicator == 2){ #chart with Moving Avg
        return(stock() %>%
                 plot_ly(x = ~date,
                         y = ~close,
                         type = "scatter", mode = "lines") %>% 
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = "")) %>%  
                 config(displayModeBar = F) %>% 
                 add_lines(x = ~date, y = ~SMA(close, n=10), 
                           line = list(dash="solid", width = 1.5, color=NULL),
                           inherit = F, showlegend = FALSE) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 1 && input$indicator == 3){ #chart with Bollinger Bands
        return(bb() %>%
                 plot_ly(x = ~date,
                         y = ~close,
                         type = "scatter", mode = "lines") %>% 
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = "")) %>%  
                 config(displayModeBar = F) %>% 
                 add_ribbons(x = ~date, ymin = ~dn, ymax = ~up,
                             line = list(dash="solid", width = 1.5, color=NULL),
                             inherit = F, showlegend = FALSE,
                             fillcolor = 'rgba(7, 164, 181, 0.2)') %>% 
                 add_lines(x = ~date, y = ~SMA(close, n=5), 
                           line = list(dash="solid", width = 1.5, color=NULL),
                           inherit = F, showlegend = FALSE) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 1 && input$indicator == 4){ #chart with exponential moving average
        return(stock() %>%
                 plot_ly(x = ~date,
                         y = ~close,
                         type = "scatter", mode = "lines") %>% 
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = "")) %>%   
                 config(displayModeBar = F) %>% 
                 add_lines(x = ~date, y = ~EMA(close, n=10), 
                           line = list(dash="solid", width = 1.5, color=NULL),
                           inherit = F, showlegend = FALSE) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 1 && input$indicator == 5){ #chart with MACD + Signal
        return(stock() %>%
                 plot_ly(x = ~date,
                         y = ~close,
                         type = "scatter", mode = "lines") %>% 
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = "")) %>%  
                 config(displayModeBar = F) %>% 
                 add_lines(x = ~date, y = ~MACD(close)[,1], 
                           line = list(dash="solid", width = 1.5, color=NULL),
                           inherit = F, showlegend = FALSE) %>% 
                 add_lines(x = ~date, y = ~MACD(close)[,2], 
                           line = list(dash="solid", width = 1.5, color=NULL),
                           inherit = F, showlegend = FALSE) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 1 && input$indicator == 6){ #chart with Bollinger Bands
        return(subplot(stock() %>% #volume plot 
                         plot_ly(x=~date, y=~close, type='scatter', mode="lines", name = "Price"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~volume, type='bar', name = "Volume"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~SMA(close,n=5), type='scatter', mode="lines", name = "SMA"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~EMA(close,n=5), type='scatter', mode="lines", name = "EMA"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~MACD(close)[,1], type='scatter', mode="lines", name = "MACD + Signal") %>% 
                         add_lines(x = ~date, y = ~MACD(close)[,2], 
                                   line = list(dash="solid", width = 1.5, color=NULL),
                                   inherit = F, showlegend = FALSE),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~ROC(close), type='scatter', mode="lines", name = "Return"),
                       nrows=6,
                       shareX = TRUE) %>%
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = "")) %>%   #title = paste0(input$ticker), 
                 config(displayModeBar = F) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } 
      ###############################################################################
      if (input$type == 2 && input$indicator == 1){ #Candlestick chart
        return(subplot(stock() %>% #combine candlestick plot with
                         plot_ly(x = ~date,
                                 type = "candlestick", 
                                 open = ~open, 
                                 close = ~close, 
                                 high = ~high,
                                 low = ~low,
                                 name = "price"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~volume, type='bar', name = "Volume"),
                       heights = c(0.7,0.3), nrows=2,
                       shareX = TRUE) %>%
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = ""),
                        showlegend = F) %>%   #title = paste0(input$ticker), 
                 config(displayModeBar = F) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 2 && input$indicator == 2){ #Candlestick with Moving Avg
        return(subplot(stock() %>% #combine candlestick plot with
                         plot_ly(x = ~date,
                                 type = "candlestick", 
                                 open = ~open, 
                                 close = ~close, 
                                 high = ~high,
                                 low = ~low,
                                 name = "price") %>% 
                         add_lines(x = ~date, y = ~SMA(close, n=5), 
                                   line = list(dash="solid", width = 1.5, color="blue"),
                                   inherit = F, showlegend = FALSE),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~volume, type='bar', name = "Volume"),
                       heights = c(0.7,0.3), nrows=2,
                       shareX = TRUE) %>%
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = ""),
                        showlegend = F) %>%   #title = paste0(input$ticker), 
                 config(displayModeBar = F) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 2 && input$indicator == 3){ #Candlestick with Bollinger Bands
        return(subplot(bb() %>% #combine candlestick plot with
                         plot_ly(x = ~date,
                                 type = "candlestick", 
                                 open = ~open, 
                                 close = ~close, 
                                 high = ~high,
                                 low = ~low,
                                 name = "price") %>% 
                         add_ribbons(x = ~date, ymin = ~dn, ymax = ~up,
                                     line = list(dash="solid", width = 1.5, color=NULL),
                                     inherit = F, showlegend = FALSE,
                                     fillcolor = 'rgba(7, 164, 181, 0.2)') %>% 
                         add_lines(x = ~date, y = ~SMA(close, n=5), 
                                   line = list(dash="solid", width = 1.5, color=NULL),
                                   inherit = F, showlegend = FALSE),
                       bb() %>% #volume plot 
                         plot_ly(x=~date, y=~volume, type='bar', name = "Volume"),
                       heights = c(0.7,0.3), nrows=2,
                       shareX = TRUE) %>% 
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = ""),
                        showlegend = F) %>%  
                 config(displayModeBar = F) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 2 && input$indicator == 4){ #Candlestick with Exponential Moving Avg
        return(subplot(stock() %>% #combine candlestick plot with
                         plot_ly(x = ~date,
                                 type = "candlestick", 
                                 open = ~open, 
                                 close = ~close, 
                                 high = ~high,
                                 low = ~low,
                                 name = "price") %>% 
                         add_lines(x = ~date, y = ~EMA(close, n=5), #horizon = 5
                                   line = list(dash="solid", width = 1.5, color="blue"),
                                   inherit = F, showlegend = FALSE),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~volume, type='bar', name = "Volume"),
                       heights = c(0.7,0.3), nrows=2,
                       shareX = TRUE) %>%
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = ""),
                        showlegend = F) %>%   #title = paste0(input$ticker), 
                 config(displayModeBar = F) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 2 && input$indicator == 5){ #Candlestick with Exponential Moving Avg
        return(stock() %>%
                 plot_ly(x = ~date,
                         type = "candlestick", 
                         open = ~open, 
                         close = ~close, 
                         high = ~high,
                         low = ~low,
                         name = "price") %>% 
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = "")) %>%  
                 config(displayModeBar = F) %>% 
                 add_lines(x = ~date, y = ~MACD(close)[,1], 
                           line = list(dash="solid", width = 1.5, color=NULL),
                           inherit = F, showlegend = FALSE, name = "Signal") %>% 
                 add_lines(x = ~date, y = ~MACD(close)[,2], 
                           line = list(dash="solid", width = 1.5, color=NULL),
                           inherit = F, showlegend = FALSE) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } else if (input$type == 2 && input$indicator == 6){ #Candlestick with ALL INDICATORS
        return(subplot(stock() %>% #combine candlestick plot with
                         plot_ly(x = ~date,
                                 type = "candlestick", 
                                 open = ~open, 
                                 close = ~close, 
                                 high = ~high,
                                 low = ~low,
                                 name = "price"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~volume, type='bar', name = "Volume"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~SMA(close,n=10), type='scatter', mode="lines", name = "SMA"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~EMA(close,n=10), type='scatter', mode="lines", name = "EMA"),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~MACD(close)[,1], type='scatter', mode="lines", name = "MACD + Signal") %>% 
                         add_lines(x = ~date, y = ~MACD(close)[,2], 
                                   line = list(dash="solid", width = 1.5, color=NULL),
                                   inherit = F, showlegend = FALSE),
                       stock() %>% #volume plot 
                         plot_ly(x=~date, y=~ROC(close), type='scatter', mode="lines", name = "Return"),
                       nrows=6,
                       shareX = TRUE) %>%
                 layout(xaxis = list(rangeslider = list(visible = F), title = ""),
                        yaxis = list(title = ""),
                        showlegend = F) %>%   #title = paste0(input$ticker), 
                 config(displayModeBar = F) %>% 
                 layout(plot_bgcolor='#00000000') %>% 
                 layout(paper_bgcolor='#00000000'))
      } 
    }
    )
    
    #########################
    ##      Value Boxes    ##
    
    reward_mean <- eventReactive(input$button, { #Mean Return
      req(input$ticker, input$dateRange)
      
      round(as.data.frame(tq_get(input$ticker, 
                                 get  = "stock.prices",
                                 from = input$dateRange,
                                 to   = today(),
                                 complete_cases = F)$close) %>% ROC() %>%  mean(na.rm=T), digits = 3)
      
    }
    )
    
    risk_yz <- eventReactive(input$button, { #Return_Volatility
      req(input$ticker, input$dateRange)
      
      data <- tq_get(input$ticker, 
                     get  = "stock.prices",
                     from = input$dateRange,
                     to   = today(),
                     complete_cases = F) %>% OHLC()
      
      return(volatility(data, n=nrow(data)-1, calc = "yang.zhang") %>% 
               na.omit() %>% 
               last() %>% 
               round(digits = 2))
      
    }
    )
    
    pe_ratio <- eventReactive(input$button, { 
      req(input$ticker)
      
      getQuote(paste(input$ticker, sep="", collapse=";"), what=yahooQF(c("Price/Sales", 
                                                                         "P/E Ratio",
                                                                         "Price/EPS Estimate Next Year",
                                                                         "PEG Ratio",
                                                                         "Dividend Yield", 
                                                                         "Market Capitalization"))) %>% 
        select("P/E Ratio") %>% 
        round(digits = 2)
    }
    )
    
    rsi <- eventReactive(input$button, { 
      req(input$ticker, input$dateRange)
      
      as.data.frame(tq_get(input$ticker, 
                           get  = "stock.prices",
                           from = input$dateRange,
                           to   = today(),
                           complete_cases = F)$close)[,1] %>% RSI(n = 14) %>% last()
      
    }
    )
    
    
    ###########################################
    
    output$plot <- renderPlotly({
      plotses()
    })
    
    output$reward <- renderValueBox({
      if(reward_mean() >= 0){
        valueBox("Reward", paste0(reward_mean()),
                 icon = icon("money-bill"),
                 color = "green")
      } else{
        
        valueBox("Reward", paste0(reward_mean()),
                 icon = icon("ban"),
                 color = "red")
      }
    })
    ##########################################
    output$risk <- renderValueBox({
      
      
      if(risk_yz() <= 0.4){
        valueBox("Risk", paste0(risk_yz()),
                 icon = icon("money-bill"),
                 color = "green")
      } else if(risk_yz() > 0.4 && risk_yz() < 0.5){
        valueBox("Risk", paste0(risk_yz()),
                 icon = icon("exclamation-triangle"),
                 color = "orange")
      } else if(risk_yz() >= 0.5){
        valueBox("Risk", paste0(risk_yz()),
                 icon = icon("ban"),
                 color = "red")
      }
      
    })
    ##########################################
    #add: if(as.numeric(today()-as.Date(input$dateRange)) < 22){print(MORE DATA)}
    output$rsi <- renderValueBox({
      
      #always new value box because icon would not work inside ifelse
      if(rsi() <= 30){
        valueBox("RSI", paste0(ceiling(rsi())),
                 icon = icon("money-bill"),
                 color = "green")
      } else if(rsi() > 30 && rsi() < 70){
        valueBox("RSI", paste0(ceiling(rsi())),
                 icon = icon("exclamation-triangle"),
                 color = "orange")
      } else if(rsi() >= 70){
        valueBox("RSI", paste0(ceiling(rsi())),
                 icon = icon("ban"),
                 color = "red")
      }
      
    })
    ###########################################
    
    output$pe_ratio <- renderValueBox({
      if(is.na(pe_ratio())){
        valueBox("P/E", paste0("NA"),
                 icon = icon("exclamation-triangle"),
                 color = "orange")
      } else if(pe_ratio() < 20){
        valueBox("P/E", paste0(pe_ratio()),
                 icon = icon("money-bill"),
                 color = "green")
      } else{
        valueBox("P/E", paste0(pe_ratio()),
                 icon = icon("ban"),
                 color = "red")
      } 
    })
    ###########################################
    
    #ugly as fuck
    
    output$table2 <- renderReactable({
      
      cbind(tq_get(input$ticker, 
                   get  = "stock.prices",
                   from = input$dateRange,
                   to   = today(),
                   complete_cases = F) %>% 
              as.data.frame() %>% 
              arrange(desc(date)) %>% 
              select(date) %>% 
              `colnames<-`("Date"),
            #bind the remaining columns (OHLC + SMA + EMA) individually in order to round to 2 digits
            tq_get(input$ticker, 
                   get  = "stock.prices",
                   from = input$dateRange,
                   to   = today(),
                   complete_cases = F) %>% 
              as.data.frame() %>% 
              arrange(desc(date)) %>% 
              `colnames<-`(c("Symbol", "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")) %>% 
              select("Open", "High", "Low", "Close", "Volume") %>% 
              round(digits = 2),
            
            tq_get(input$ticker, 
                   get  = "stock.prices",
                   from = input$dateRange,
                   to   = today(),
                   complete_cases = F)$close %>%
              SMA(n=5) %>% 
              round(digits = 2) %>% 
              rev(),
            
            tq_get(input$ticker, 
                   get  = "stock.prices",
                   from = input$dateRange,
                   to   = today(),
                   complete_cases = F)$close %>% 
              EMA(n=5) %>% 
              round(digits = 2) %>% 
              rev()) %>% 
        as.data.frame() %>% 
        `colnames<-`(c("Date", "Open", "High", "Low", "Close", "Volume", "SMA", "EMA")) %>% 
        reactable(defaultPageSize = 14,
                  defaultSorted = list(Date = "desc"),
                  theme = reactableTheme(backgroundColor = "transparent" #color = "black", borderColor = "black",
                                         #paginationStyle = list(color = "black"), 
                                         #selectStyle = list(color = "black"),
                                         #headerStyle = list(color = "#727272"),
                                         #cellStyle = list(color = "#727272") 
                  )
        )
      
      
      #datatable(
      # options = list(paging = T,    ## paginate the output
      #                 pageLength = 8,  ## number of rows to output for each page
      #                 scrollX = F,   ## enable scrolling on X axis
      #                 scrollY = T,   ## enable scrolling on Y axis
      #                 autoWidth = T, ## use smart column width handling
      #                 server = FALSE,   ## use client-side processing
      #                 columnDefs = list(list(targets = '_all', className = 'dt-center'),
      #                                   list(targets = c(0,2,3,4,7), visible = FALSE)),
      #                 searching = FALSE,
      #                 dom = "tp"
      #  ),
      #  selection = 'single', ## enable selection of a single row
      #  rownames = FALSE, ## don't show row numbers/names
      #  style = "bootstrap"
      #)
      
    })
    
    #################################################################################
    
    
    insample_arima <- eventReactive(input$button_predict, {
      req(input$horizon, input$button_predict)
      
      tq_get(input$ticker, 
             get  = "stock.prices",
             complete_cases = F)$close %>% 
        as.data.frame() %>% 
        head(-input$horizon) %>% 
        auto.arima(lambda = "auto", stepwise = F, approximation = F) %>% 
        forecast(h = input$horizon) 
      
    }
    )
    
    output$insample_arima <- renderPlotly({
      req(input$horizon, input$button_predict)
      
      plot_ly() %>% 
        add_lines(x = seq(1, nrow(insample_arima()$x), by = 1),
                  y = insample_arima()$x,
                  color = "blue",
                  name = "observed",
                  type = 'scatter',
                  mode = 'lines') %>% 
        add_lines(x =seq(nrow(insample_arima()$x)+1, nrow(insample_arima()$x)+input$horizon, by = 1),
                  y = insample_arima()$mean,
                  col = "orange", 
                  name = 'predicted'
        ) %>% 
        add_lines(x = seq(1, nrow(arima()$x), by = 1),
                  y = tq_get("AMZN", 
                             get  = "stock.prices",
                             complete_cases = F)$close,
                  col = "red",
                  name = 'observed'
                  
        ) %>% 
        layout(xaxis = list(rangeslider = list(visible = F), title = "",
                            range = c(nrow(insample_arima()$x)- input$horizon - 20,
                                      nrow(insample_arima()$x) + input$horizon)),
               yaxis = list(title = "")) %>%  
        config(displayModeBar = F) %>% 
        layout(plot_bgcolor='#00000000') %>% 
        layout(paper_bgcolor='#00000000')
    })
    
    
    
    arima <- eventReactive(input$button_predict, {
      req(input$horizon, input$button_predict)
      
      tq_get(input$ticker, 
             get  = "stock.prices",
             complete_cases = F)$close %>% 
        as.data.frame() %>% 
        auto.arima(lambda = "auto", stepwise = F, approximation = F) %>% 
        forecast(h = input$horizon) 
      
    }
    )
    
    output$arima <- renderPlotly({
      req(input$horizon, input$button_predict)
      
      plot_ly() %>% 
        add_lines(x = seq(1, nrow(arima()$x), by = 1),
                  y = tq_get(input$ticker, 
                             get  = "stock.prices",
                             complete_cases = F)$close,
                  color = "blue",
                  name = "observed",
                  type = 'scatter',
                  mode = 'lines') %>% 
        add_lines(x = seq(nrow(arima()$x)+1, nrow(arima()$x)+input$horizon, by = 1),
                  y = arima()$mean,
                  color = "red",
                  name = "predicted") %>% 
        add_ribbons(x = seq(nrow(arima()$x)+1, nrow(arima()$x)+input$horizon, by = 1),
                    ymin = arima()$lower[,2],
                    ymax = arima()$upper[,2],
                    color = "grey",
                    name = "95% confidence") %>% 
        add_ribbons(x = seq(nrow(arima()$x)+1, nrow(arima()$x)+input$horizon, by = 1),
                    ymin = arima()$lower[,1],
                    ymax = arima()$upper[,1],
                    color = "orange",
                    name = "80% confidence") %>% 
        layout(xaxis = list(rangeslider = list(visible = F), title = "",
                            range = c(nrow(as.data.frame(tq_get(input$ticker, 
                                                                get  = "stock.prices",
                                                                complete_cases = F)$close)) - 63, #one quarter before pred
                                      nrow(arima()$x)+input$horizon)),
               yaxis = list(title = "")) %>%  
        config(displayModeBar = F) %>% 
        layout(plot_bgcolor='#00000000') %>% 
        layout(paper_bgcolor='#00000000')
    })
    
    output$arima_table <- renderReactable({
      arima() %>% 
        as.data.frame() %>% 
        `colnames<-`(c("Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95")) %>% 
        round(digits = 2) %>% 
        mutate(Date = as.Date(seq(today(), today() + input$horizon - 1, by = 1))) %>% 
        select("Date", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95") %>% 
        reactable(defaultPageSize = 14,
                  theme = reactableTheme(backgroundColor = "transparent" #color = "black", borderColor = "black",
                                         #paginationStyle = list(color = "black"), 
                                         #selectStyle = list(color = "black"),
                                         #headerStyle = list(color = "#727272"),
                                         #cellStyle = list(color = "#727272") 
                  )
        )
    })
    
    
    output$summary <- renderPrint({
      print(accuracy(insample_arima()$mean, tail(arima()$x, input$horizon)))
    })
    
    ##############################################################################
    output$arima_box1 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," ", arima()$method), plotlyOutput("arima", height = "42em"), solidHeader = T)
    })
    
    output$arima_box2 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," ARIMA"), reactableOutput("arima_table", height = "42em"), solidHeader = T)
    })
    
    output$arima_box3 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," Ex-post forecast of length horizon"), plotlyOutput("insample_arima", height = "42em"), solidHeader = T)
    })
    
    output$arima_box4 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," Ex-post Details"), verbatimTextOutput("summary"), solidHeader = T)
    })
    
    
    ###########################################
    
    
    proph <- eventReactive(input$button_predict, {
      req(input$horizon, input$button_predict)
      
      predict(data.frame(ds = tq_get(input$ticker, 
                                     get  = "stock.prices",
                                     complete_cases = F)$date,
                         y = tq_get(input$ticker, 
                                    get  = "stock.prices",
                                    complete_cases = F)$close) %>% 
                prophet(), 
              data.frame(ds = tq_get(input$ticker, 
                                     get  = "stock.prices",
                                     complete_cases = F)$date,
                         y = tq_get(input$ticker, 
                                    get  = "stock.prices",
                                    complete_cases = F)$close) %>% 
                prophet() %>% 
                make_future_dataframe(periods = input$horizon)) %>% 
        mutate(ds = as.Date(ds, formate = "%Y-%m-%d"))
    })
    
    output$proph <-  renderPlotly({
      req(input$horizon, input$button_predict)
      
      plot_ly(x = proph()$ds,
              y = proph()$yhat,
              type = "scatter", mode = "lines",
              name = "predicted") %>% 
        layout(xaxis = list(rangeslider = list(visible = F), title = "",
                            range = c(last(proph()$ds) - input$horizon - 63,
                                      last(proph()$ds)
                            )),
               yaxis = list(title = "")) %>%  
        config(displayModeBar = F) %>% 
        layout(plot_bgcolor='#00000000') %>% 
        layout(paper_bgcolor='#00000000') %>% 
        add_ribbons(x = proph()$ds, 
                    ymin = proph()$yhat_lower, 
                    ymax = proph()$yhat_upper,
                    line = list(dash="solid", width = 1.5, color=NULL),
                    inherit = F,
                    fillcolor = 'rgba(7, 164, 181, 0.2)',
                    name = "80% confidence") %>% 
        add_lines(x = ~tq_get(input$ticker, 
                              get  = "stock.prices",
                              complete_cases = F)$date, 
                  y = ~tq_get(input$ticker, 
                              get  = "stock.prices",
                              complete_cases = F)$close,
                  line = list(dash="solid", width = 1.5, color=NULL),
                  inherit = F,
                  name = "observed") 
    })
    
    output$proph_table <- renderReactable({
      
      proph() %>% 
        tail(input$horizon) %>% 
        as.data.frame() %>% 
        select(c("ds", "yhat", "yhat_upper", "yhat_lower")) %>% 
        reactable(defaultPageSize = 14,
                  theme = reactableTheme(backgroundColor = "transparent" #color = "black", borderColor = "black",
                                         #paginationStyle = list(color = "black"), 
                                         #selectStyle = list(color = "black"),
                                         #headerStyle = list(color = "#727272"),
                                         #cellStyle = list(color = "#727272") 
                  )
        )
    })
    
    
    ##########################################################################
    
    output$proph_box1 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," Prophet"), plotlyOutput("proph", height = "42em"), solidHeader = T)
    })
    
    output$proph_box2 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," Prophet"), reactableOutput("proph_table", height = "42em"), solidHeader = T)
    })
    
    #################################################################################
    #======================== BOX 1 DCF PAGE ========================================
    
    fcf_roc <- eventReactive(input$button_dcf, { 
      req(input$ticker)
      
      yahoo_financials(input$ticker, reporting = "annual", verbose = FALSE) %>% 
        arrange(endDate) %>% 
        select(endDate, totalCashFromOperatingActivities, capitalExpenditures) %>% 
        mutate(totalCashFromOperatingActivities - capitalExpenditures) %>% 
        mutate(as.Date(as.POSIXct(endDate))) %>% 
        mutate(growth_rate((totalCashFromOperatingActivities - capitalExpenditures))) %>% 
        `colnames<-`(c("schmutz", "Cash", "capExp", "FCF", "Date", "ROC")) %>% 
        select(Date, FCF, ROC) %>% 
        arrange(desc(Date))
      
    }
    )
    
    output$fcf <- renderReactable({
      fcf_roc() %>% 
        reactable(defaultPageSize = 6,
                  defaultSorted = list(Date = "asc"),
                  theme = reactableTheme(backgroundColor = "transparent" #color = "black", borderColor = "black",
                                         #paginationStyle = list(color = "black"), 
                                         #selectStyle = list(color = "black"),
                                         #headerStyle = list(color = "#727272"),
                                         #cellStyle = list(color = "#727272") 
                  ),
                  columns = list(
                    FCF = colDef(
                      footer = "Average ROC"
                    ),
                    
                    ROC = colDef(
                      style = function(value) {
                        if (is.na(value)){
                          color = "transparent"
                        } else if (value > 0) {
                          color <- "#008000"
                        } else if (value < 0) {
                          color <- "#e00000"
                        } else {
                          color <- "#777"
                        }
                        list(color = color, fontWeight = "bold")
                      },
                      cell = icon_sets(fcf_roc(), icon_size = 14, icons = "percent", colors = "grey"),
                      footer = function(values) (mean(values, na.rm = T))
                    )
                  )
        ) 
    })
    
    output$dcf_box1 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," FCF"), reactableOutput("fcf", height = "15em"), solidHeader = T)
    })
    
    ####################################################################################
    #========================== BOX 2 DCF PAGE =========================================
    
    
    
    
    output$dcf_box2 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," WACC"), reactableOutput("wacc", height = "15em"), solidHeader = T)
    })
    
    ####################################################################################
    #========================== BOX 3 DCF PAGE =========================================
    
    tv <- eventReactive(input$button_dcf, { 
      req(input$dcf_horizon, input$scenarios, input$discount)
      
      fcfg <- function(){
        set.seed(123)
        
        fcfs <- matrix(nrow = 1+input$dcf_horizon, ncol = 3)
        
        rn_mean = rnorm(input$dcf_horizon, mean = mean((fcf_roc()$ROC)/100, na.rm = T), sd = sd((fcf_roc()$ROC)/100, na.rm = T))
        dc = input$discount/100
        fcfs[1,1] = first(fcf_roc()$FCF)
        fcfs[1,2] = 1
        fcfs[1,3] = NA
        
        
        fcfs[2,1] = round(first(fcf_roc()$FCF)*(1+abs(rn_mean[1])), digits = 0)
        
        for(i in 2:(nrow(fcfs)-1)){
          fcfs[i+1,1] = round(fcfs[i,1]*(1+abs(rn_mean[i])), digits = 0)
        }
        for(i in 2:nrow(fcfs)){
          fcfs[i,2] = round((1+dc)^(i), digits = 2)
        }
        for(i in 2:nrow(fcfs)){
          fcfs[i,3] = round(fcfs[i,1]/fcfs[i,2], digits = 0)
        }
        return(cbind(c(0, seq(1:(input$dcf_horizon))), c(NA, round(rn_mean * 100, digits = 2)), fcfs))
      }
      return(fcfg() %>% as.data.frame())
    }
    )
    
    output$terminal_value <- renderReactable({
      tv() %>% 
        `colnames<-`(c("Years_Ahead", "FCF_Growth", "FCF_Ahead", "Discount_Rate", "DCF")) %>%
        reactable(defaultPageSize = 11,
                  theme = reactableTheme(backgroundColor = "transparent"),
                  columns = list(Discount_Rate = colDef(
                    footer = "Total DCF"),
                    DCF = colDef(
                      footer = function(values) (sum(values, na.rm = T))
                    ),
                    FCF_Growth = colDef(
                      style = function(value) {
                        if (is.na(value)){
                          color = "transparent"
                        } else if (value > 0) {
                          color <- "#008000"
                        } else if (value < 0) {
                          color <- "#e00000"
                        } else {
                          color <- "#777"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    )
                    
                  ) 
        )
    })
    
    
    output$dcf_box3 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," DCF"), reactableOutput("terminal_value", height = "34em"), solidHeader = T)
    })
    
    ####################################################################################
    #========================== BOX 4 DCF PAGE =========================================
    
    shares_out <- eventReactive(input$button_dcf, {
      req(input$ticker)
      getQuote(paste(input$ticker, sep="", collapse=";"), 
               what=yahooQF(c("Price/Sales", 
                              "P/E Ratio",
                              "Price/EPS Estimate Next Year",
                              "PEG Ratio",
                              "Dividend Yield", 
                              "Market Capitalization",
                              "Shares Outstanding",
                              "Name"))) %>% as.data.frame() %>% select(ends_with("ing")) %>% 
        `colnames<-`("Shares")
    }
    )
    
    dcf_analysis0 <- eventReactive(input$button_dcf, { 
      req(input$ticker, input$scenarios, input$industry_growth, input$discount)
      
      round((last(tv()$V5)*(1+input$industry_growth))/(input$discount-input$industry_growth), digits = 0) %>% 
        as.data.frame() 
    }
    )
    
    dcf_analysis1 <- eventReactive(input$button_dcf, { 
      req(input$ticker, input$scenarios, input$industry_growth, input$discount)
      
      (first(yahoo_financials(input$ticker, reporting = "annual", verbose = FALSE)$cash) - 
          first(yahoo_financials(input$ticker, reporting = "annual", verbose = FALSE)$totalLiab)) %>% 
        as.data.frame()
      
    }
    )
    
    dcf_analysis_final <- eventReactive(input$button_dcf, { 
      req(input$mos)
      
      
      data.frame(Analysis = c("Total DCF", "Terminal Value", "Net Cash", "Fair Value", "Margin of Safety Value"),
                 Values = rbind(sum(tv()$V5, na.rm = T), dcf_analysis0(), dcf_analysis1(), 
                                round(
                                  (sum(tv()$V5, na.rm = T) + dcf_analysis0() + dcf_analysis1())/shares_out()$Shares,
                                  digits = 2),
                                round(
                                  (sum(tv()$V5, na.rm = T) + dcf_analysis0() + dcf_analysis1())/shares_out()$Shares,
                                  digits = 2) * (1-input$mos/100)
                 )
      )
    }
    )
    
    
    output$real_dcf <- renderReactable({
      
      dcf_analysis_final() %>% 
        
        reactable(theme = reactableTheme(background = "transparent")) 
      
    })
    
    
    output$dcf_box4 = renderUI({
      req(input$ticker)
      box(title = paste0(input$ticker," DCF"), reactableOutput("real_dcf", height = "34em"), solidHeader = T)
    })
    
    
    
    ##############################################################################################################################################
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
  }
)





