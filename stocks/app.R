
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)

tickers <- c("^NSEI","RELIANCE.NS","TCS.NS","HDFCBANK.NS",
             "ASIANPAINT.NS","TITAN.NS","BAJFINANCE.NS","ADANIENT.NS")


prices <- tq_get(tickers, 
                 get  = "stock.prices",
                 from = today()-months(12),
                 to   = today(),
                 complete_cases = F) %>%
  select(symbol,date,close)


# -----------------------------------------------------
# UI
#-------------------------------------------------------

ui <- fluidPage(#theme = shinytheme("cyborg"),
  
  # Title
  titlePanel("NIFTY 50 Companies"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 # Let user pick stocks
                 pickerInput(
                   inputId = "stocks",
                   label = h4("Stocks"),
                   choices = c(
                     "NSEI"       = tickers[1], 
                     "RELIANCE.NS"   = tickers[2],
                     "TCS.NS"      = tickers[3],
                     "HDFCBANK.NS"         = tickers[4],
                     "ASIANPAINT.NS"    = tickers[5],
                     "TITAN.NS"     = tickers[6],
                     "BAJFINANCE.NS"       = tickers[7],
                      "ADANIENT.NS"=tickers[8]),
                   selected = tickers,   
                   options = list(`actions-box` = TRUE), 
                   multiple = T
                 ),
                 
                 # Pick time period
                 radioButtons("period", label = h4("Period"),
                              choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5), 
                              selected = 4
                 ),
                 
                
    ),
    
    # Plot results
    mainPanel(
      plotlyOutput("plot",height=900)
    )
  )
)


# -----------------------------------------------------
# SERVER
#-------------------------------------------------------


server <- function(input, output) {
  
  # server logic based on user input
  observeEvent(c(input$period,input$stocks), {
    
    prices <- prices %>%
      filter(symbol %in% input$stocks)
    
    if (input$period == 1) {
      prices <- prices %>%
        filter(
          date >= today()-months(1)) }
    
    if (input$period == 2) {
      prices <- prices %>%
        filter(date >= today()-months(3)) }
    
    if (input$period == 3) {
      prices <- prices %>%
        filter(date >= today()-months(6)) }
    
    if (input$period == 5) {
      prices <- prices %>%
        filter(year(date) == year(today())) 
        
      }
    
    
    # Create plot
    output$plot <- renderPlotly({
      print(
        ggplotly(prices %>%
                   group_by(symbol) %>%
                   mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
                   mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
                   ungroup() %>%
                   ggplot(aes(date, value,colour = symbol)) +
                   geom_line(size = 1, alpha = .9) +
                   
                   theme_minimal(base_size=16) +
                   theme(axis.title=element_blank(),
                         plot.background = element_rect(fill = "black"),
                         panel.background = element_rect(fill="black"),
                         panel.grid = element_blank(),
                         legend.text = element_text(colour="white"))
        )
      )
    })
  })
}


shinyApp(ui = ui, server = server)




