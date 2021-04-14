# greatpository
---
title: "Cryptocurrencies analysis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

*If you want to invest your money in cryptocurrencies, let you be helped by this app!*

## Data structure
This app takes values of the big 7 cryptocurrencies available on the market, listing them by decreasing level of capitalisation. Data are collected from 'Yahoo Finance' using 'quantmod' library.

What you can do with it is:

- *drawing a time series to have a picture of the trend*
- *looking at prices thorugh the table*
- *analyzing correlation between different cryptocurrencies*

### Drawing the time series
You can control time series that you want to display using the data range and the select input box on the sidebar panel. Furthermore, you could check the results on a log scale through the relative check box. As example, I presented below the historic trend of Bitcoin, starting from 1st january 2019 and up to now:
```{r BTC, message=FALSE, warning=FALSE}
library(quantmod)
library(ggplot2)
BTC <- getSymbols(Symbols = 'BTC-USD', src = 'yahoo', auto.assign = FALSE)
Bitcoin <- window(BTC, start = '2019-01-01',end = as.character(Sys.Date()))
chartSeries(Bitcoin,theme = chartTheme("white"),type = "line", TA = NULL)
```
![image](https://user-images.githubusercontent.com/74599961/114732829-21c72100-9d43-11eb-9c89-8a8ea32e74a1.png)


### Looking tables
Choosing the number of observation that you prefer, you can look at the table to check the daily yield on the basis of a time range which you can select. For instance, a table regarding Bitcoin with 5 observations and a daily average yield with 2 lag, interpretable as how much you would have daily earned investing two days ago, is displayed below:


### Checking correlation
Analyzing correlation between cryptocurrencies, you could view in which way they reciprocally behaved along a certain period and imagine how your portfolio could be made, according to your risk preferences. For instance, investing in a portfolio where cryptocurrencies are negative correlated, you will minimize the risk, otherwise you could search other investment profiles.

The chart depends on the data range and the check box group. You can draw as many correlogram as you want by selecting two cryptocurrencies on the check box group a time. The title of the graph will be automatically modified.

As example, the correlogram between Bitcoin and Ethereum which considers a time range from 1st january 2019 to today is reported below:
```{r message=FALSE, warning=FALSE}
library(quantmod)
library(ggplot2)
library(ggcorrplot)
BTC <- getSymbols(Symbols = 'BTC-USD', src = 'yahoo', auto.assign = FALSE)
ETH <- getSymbols(Symbols = 'ETH-USD', src = 'yahoo', auto.assign = FALSE)
merged <- merge(BTC,ETH)
data <- window(merged, start = '2019-01-01', end=as.character(Sys.Date()))
data <- na.omit(data)
ggcorrplot(round(cor(data),1),hc.order = TRUE, type = 'lower', lab = TRUE, method = 'circle')


```
![image](https://user-images.githubusercontent.com/74599961/114732896-2f7ca680-9d43-11eb-9e3c-9d1d445c76fc.png)

As it can easily get by the chart, the results are also clustered in order to detect possible patterns which could be present and different colors correspond to different relationship's intensity level.


**Let's play with the widgets and find useful data for your investment!**
