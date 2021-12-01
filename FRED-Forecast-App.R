library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(ggplot2)
library(shiny)
library(shinythemes)
library(ggthemes)
library(fredr)
library(zoo)
library(forecast)
library(dygraphs)
library(scales)
library(DT)
library(formattable)
library(rsconnect)

# My first comment

fredr_set_key("0962c1d1b6316b7c4b6c68ced8211ac9")

ui <- fluidPage(theme = shinytheme("flatly"),
  headerPanel("Graham's Economic Forecasting Device"),             
  sidebarLayout(
    sidebarPanel(
      selectInput("series", 
                  "Choose a series:",
                  choices = c("RGDP", "CPI", "Unemployment")),
      selectInput("interval", 
                  "Choose a forecast interval:",
                  choices = c("80%", "90%", "95%", "99%")),
      sliderInput("periods",
                  "Forecasting periods ahead:",
                  min = 2,
                  max = 12,
                  value = 2),
      submitButton("Udate Forecast", icon("refresh")),
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Forecast",
                    h4("Plot"),
                    plotOutput("forecast.plot"),
                    h4("Summary of Data"),
                    tableOutput("summary.table")
                    ),
                  tabPanel("View FRED Data",dataTableOutput("fred.table"))
                  )
    ),
    position = "left",
    fluid = TRUE
  )  
)

server <- function(input, output, session) {
  
  f1_plot_data <- function(series0) {
  
    fred_data_f1 <- fredr(series_id = "A191RL1Q225SBEA") %>%
      select(date, value) %>%
      mutate(value = value/100)
    
    fred_tsibble_f1 <- fred_data_f1 %>%
      mutate(date = yearquarter(date)) %>%
      as_tsibble(index = date)
    
    fred_data_f1 <- fred_data_f1 %>%
      mutate(date = as.yearqtr((date)))
      data.frame()
    
    return(list(fred_tsibble_f1, fred_data_f1))
    
  }
  
  f2_plot_forecast <- function(series0, periods0) {
    
    fred_tsibble_f2 <- f1_plot_data(series0)[[1]]
    
    h0 <- as.numeric(substr(periods0,1,2))
    
    forecast_f2 <- fred_tsibble_f2 %>%
      model(ets0 = ETS(value),
            arima0 = ARIMA(value),
            snaive0 = SNAIVE(value)) %>%
      mutate(combination = (ets0 + arima0 + snaive0)/3) %>%
      select(combination) %>%
      forecast(h=h0)
    
    return(list(forecast_f2, fred_tsibble_f2))
    
  }
    
  output$forecast.plot <- renderPlot({
    
    plot_forecast_saved <- f2_plot_forecast(input$series, input$periods)
    
    forecast_output <- plot_forecast_saved[[1]]
    fred_tsibble_output <- plot_forecast_saved[[2]]
    length_fred_tsibble_output <- as.numeric(count(fred_tsibble_output))
    
    interval0 <- as.numeric(substr(input$interval,1,2))
    
    autoplot0 <- forecast_output %>%
      autoplot(
        fred_tsibble_output[(length_fred_tsibble_output - 30):(length_fred_tsibble_output) ,],
        level = interval0,
        show_gap = FALSE) + 
      theme_hc() + 
      theme(legend.position = "None", 
            plot.title = element_text(hjust = 0.5)) +
      xlab(label = element_blank()) +
      ylab(label = "% Change Quarter-over-Quarter") +
      scale_y_continuous(label = percent) + 
      ggtitle(label = "Forecasting Real GDP")
    
    return(autoplot0)
    
  })
  
  output$summary.table <- renderTable({
    
    fred_data_summary.table <- f1_plot_data(input$series0)[[2]] %>%
      data.frame()
    
    n0 <- nrow(fred_data_summary.table)
    mean0 <- toString( percent(mean(fred_data_summary.table$value)) )
    lag0 <- toString( percent(fred_data_summary.table$value[n0]) )
    ma0 <- toString( percent(mean(fred_data_summary.table$value[(n0-3):n0])) )
    sd0 <- toString( percent(sd(fred_data_summary.table$value)) )
    
    df_summary.table <- data.frame(mean0,lag0,ma0,sd0)
    
    names(df_summary.table) <- c("% Change: Total",
                                 paste("% Change: ", fred_data_summary.table[n0,"date"]),
                                 paste("% Change: ", fred_data_summary.table[(n0-3),"date"]," to ", fred_data_summary.table[n0,"date"]),
                                 "Standard Deviation: Total")
    
    return(df_summary.table)
    
  })
  
  output$fred.table <- renderDataTable({
    
    fred_data_fred.table <- f1_plot_data(input$series0)[[2]] %>%
      datatable() %>%
      formatPercentage("value", 2)
    
    return(fred_data_fred.table)
    
  })
  
}

shinyApp(ui = ui, server = server)
