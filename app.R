library(shiny)
library(quantmod)
library(ggplot2)
library(ggcorrplot)
library(zoo)

ui <- fluidPage(
  titlePanel("The big 7 cryptocurrencies: economic analysis"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("range",
                     helpText("Choose your range.."), start = "2019-01-01",
                     end = as.character(Sys.Date())),
      br(),
      selectInput("select", helpText("..and now your crypto!"),
                  choices = list("Bitcoin" ="BTC-USD","Ethereum" = "ETH-USD","Binance coin" = "BNB-USD","Tether" = "USDT-USD",
                                 "Cardano" ="ADA-USD","Polkadot"="DOT1-USD","Ripple"="XRP-USD"),selected = "BTC-USD"),
      br(),
      checkboxInput("log","Plot on a log scale",value = FALSE),
      checkboxInput("adjust","Adjust prices for inflation", value = FALSE),
      br(),
      numericInput("num", helpText("number of observations"),value = 5, min = 1, max = 30),
      br(),
      numericInput("num2", helpText("time range to compute the yield"),value = 2, min = 1, max = 5),
      br(),
      br(),
      br(),
      selectInput("select1", 
                  helpText("if you would check for correlation.."), 
                  choices = list("Bitcoin" ="BTC-USD","Ethereum" = "ETH-USD","Binance coin" = "BNB-USD","Tether" = "USDT-USD",
                                 "Cardano" ="ADA-USD","Polkadot"="DOT1-USD","Ripple"="XRP-USD"),selected = "BTC-USD"),
      selectInput("select2", 
                  helpText(""), 
                  choices = list("Bitcoin" ="BTC-USD","Ethereum" = "ETH-USD","Binance coin" = "BNB-USD","Tether" = "USDT-USD",
                                 "Cardano" ="ADA-USD","Polkadot"="DOT1-USD","Ripple"="XRP-USD"),selected = "ETH-USD")
    ),
    
    mainPanel(textOutput("selected"),
              br(),
              plotOutput("plot"),
              br(),
              textOutput("table"),
              br(),
              tableOutput("view"),
              br(),
              br(),
              br(),
              textOutput("correlation"),
              br(),
              plotOutput("correlogram"))
  )
)

server <- function(input, output) {
  output$selected <- renderText({
    paste("Time series of", input$select, "price")
  })
  dataInput <- reactive({
    getSymbols(input$select, 
               src = "yahoo",
               from = input$range[1],
               to = input$range[2],
               auto.assign = FALSE)
  })
  
  lastInput <- reactive({
    if (!input$adjust) return(dataInput())
    if (!exists(".inflation")) {
      .inflation <- getSymbols('CPIAUCNS', src = 'FRED', 
                               auto.assign = FALSE)
    }
    adjust <- function(data) {
      
      latestcpi <- last(.inflation)[[1]]
      inf.latest <- time(last(.inflation))
      months <- split(data)               
      
      adjust_month <- function(month) {               
        date <- substr(min(time(month[1]), inf.latest), 1, 7)
        coredata(month) * latestcpi / .inflation[date][[1]]
      }
      adjs <- lapply(months, adjust_month)
      adj <- do.call("rbind", adjs)
      axts <- xts(adj, order.by = time(data))
      axts[ , 5] <- Vo(data)
      axts
    }
    adjust(dataInput())
  })
  
  output$plot <- renderPlot({
    
    chartSeries(lastInput(), theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
    
  })
  
  output$table <- renderText({
    paste("Daily yields computed averaging by", input$num2, "the differences between present and", input$num2, "days ago values. Data start from",2*input$num2-1, "days after", input$range[1],".")
  })
  
  output$view <- renderTable({
    diff <- diff(dataInput(), lag = input$num2)
    yield <- rollapply(diff,width = input$num2, FUN = mean)
    yield <- stats::lag(yield, k = -(2*input$num2-1))
    head(yield, from = input$range[1], n = input$num)
  })
  
  output$correlation <- renderText({
    paste("Correlation matrix of", input$select1,"and", input$select2)
  })
  dataInput2 <- reactive({
    getSymbols(input$select1, 
               src = 'yahoo',
               from = input$range[1],
               to = input$range[2],
               auto.assign = FALSE)
    
  })
  dataInput3 <- reactive({
    getSymbols(input$select2, 
               src = 'yahoo',
               from = input$range[1],
               to = input$range[2],
               auto.assign = FALSE)
  })
  
  output$correlogram <- renderPlot({
    clean <- function(x,y){
      data <- merge(x,y)
      data <- na.omit(data)
      corr <- round(cor(data),1)
      return(corr)
    }
    ggcorrplot(clean(dataInput2(),dataInput3()),hc.order = TRUE,
               type = 'lower', lab = TRUE, method = 'circle')
  })
  
}

shinyApp(ui = ui, server = server)