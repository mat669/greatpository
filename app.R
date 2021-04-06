library(shiny)
library(quantmod)
library(ggplot2)
library(ggcorrplot)

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
      br(),
      br(),
      checkboxGroupInput("checkGroup", 
                         helpText("if you would check for correlation..(two crypto at a time)"), 
                         choices = list("Bitcoin" ="BTC-USD","Ethereum" = "ETH-USD","Binance coin" = "BNB-USD","Tether" = "USDT-USD",
                                        "Cardano" ="ADA-USD","Polkadot"="DOT1-USD","Ripple"="XRP-USD"),selected = c("BTC-USD","ETH-USD"))
      
    ),
    
    mainPanel(textOutput("selected"),
              br(),
              plotOutput("plot"),
              br(),
              textOutput("correlation"),
              br(),
              plotOutput("correlogram"))
  )
)

server <- function(input, output) {
  output$selected <- renderText({
    paste("time series of", input$select, "price")
  })
  dataInput <- reactive({
    getSymbols(input$select, 
               src = "yahoo",
               from = input$range[1],
               to = input$range[2],
               auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
    
    chartSeries(dataInput(), theme = chartTheme("white"),
                type = "line", TA = NULL)
    
  })
  output$correlation <- renderText({
    paste("correlation matrix of", input$checkGroup[1],"and", input$checkGroup[2])
  })
  dataInput2 <- reactive({
    getSymbols(input$checkGroup[1], 
               src = 'yahoo',
               from = input$range[1],
               to = input$range[2],
               auto.assign = FALSE)
    
  })
  dataInput3 <- reactive({
    getSymbols(input$checkGroup[2], 
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