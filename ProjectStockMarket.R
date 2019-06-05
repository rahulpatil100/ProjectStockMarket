
library(lubridate)  # Date & Time
library(plotly)     # Visualisation
library(TTR)        # Time series
library(tseries)    # Time series
library(forecast)   # Forecasting

train_uni <- read.csv('stocks2012-2016.csv')
test_uni <- read.csv('stocks2017.csv')
summary(train_uni)


sum(is.na(train_uni))

sum(is.na(test_uni))

train_uni$Date <- ymd(train_uni$Date, tz = 'Europe/London')
test_uni$Date <- ymd(test_uni$Date, tz = 'Europe/London')


plot_ly(data = train_uni, x = ~Date, type = 'candlestick', name = 'Uniqlo',
        open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%
  layout(title = "Uniclo market")



pl_ch <- plot_ly(data = train_uni, x = ~Date, type = 'candlestick', name = 'Uniqlo',
                 open = ~Open, close = ~Close, high = ~High, low = ~Low)

pl_vol <- plot_ly(data = train_uni, x = ~Date, y = ~Volume, type = 'bar', name = 'Volume') %>%
  layout(yaxis = list(title = "Volume"))

pl_stock <- plot_ly(data = train_uni, x = ~Date, y = ~Stock.Trading, type = 'bar', name = 'Stock.Trading') %>%
  layout(yaxis = list(title = "Stock"))

subplot(pl_ch, pl_vol, pl_stock, heights = c(0.6,0.2,0.2), nrows=3,
        shareX = TRUE, titleY = TRUE) %>%
  layout(title = 'Uniclo market',
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))


adf.test(train_uni$Close, alternative=c('stationary'))

ndiffs(train_uni$Close) 

adf.test(diff(train_uni$Close), alternative=c('stationary'))


time_fore <- seq.Date(as.Date(train_uni$Date[1]), by = 'days', length.out = 100)
cl <- train_uni[order(train_uni$Date),'Close']


L <- BoxCox.lambda(ts(cl), method="loglik")

plot.ts(SMA(cl, n = 50), col = 'red')
lines(cl)


fit.arima <- auto.arima(ts(cl), lambda=L, seasonal = F) 
fcast.arima <- forecast(fit.arima, 100)

plot_ly() %>%
  add_lines(y = train_uni$Close, x = train_uni$Date, name = 'market', color = I('blue')) %>%
  add_lines(y = fcast.arima$mean, x = time_fore, name = 'prediction', color = I('red')) %>%
  add_ribbons(x = time_fore, ymin = fcast.arima$lower[, 2], ymax = fcast.arima$upper[, 2],
              color = I('MistyRose2'), name = '95% confidence') %>%
  add_ribbons(x = time_fore, ymin = fcast.arima$lower[, 1], ymax = fcast.arima$upper[, 1],
              color = I('MistyRose3'), name = '80% confidence') %>%
  layout(title = paste('Forecasts from', fcast.arima$method),
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Close'))


fit.nn <- nnetar(ts(cl), lambda=L, size=3)
fcast.nn <- forecast(fit.nn, h=100, lambda=L)

plot_ly() %>%
  add_lines(y = train_uni$Close, x = train_uni$Date, name = 'market', color = I('blue')) %>%
  add_lines(y = fcast.nn$mean, x = time_fore, name = 'prediction', color = I('red')) %>%
  layout(title = paste('Forecasts from', fcast.nn$method),
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Close'))

fit.tbats <-tbats(ts(cl), lambda=L)
fcast.tbats <- forecast(fit.tbats, h = 100, lambda=L)

plot_ly() %>%
  add_lines(y = train_uni$Close, x = train_uni$Date, name = 'market', color = I('blue')) %>%
  add_lines(y = fcast.tbats$mean, x = time_fore, name = 'prediction', color = I('red')) %>%
  add_ribbons(x = time_fore, ymin = fcast.tbats$lower[, 2], ymax = fcast.tbats$upper[, 2],
              color = I('MistyRose2'), name = '95% confidence') %>%
  add_ribbons(x = time_fore, ymin = fcast.tbats$lower[, 1], ymax = fcast.tbats$upper[, 1],
              color = I('MistyRose3'), name = '80% confidence') %>%
  layout(title = paste('Forecasts from', fcast.tbats$method),
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Close'))



mape <- function(real, forecast){                      
  len <- length(real)
  return(sum( abs(real - forecast$mean[1:len]) / real) / len * 100)
}

paste0('Error ARIMA: ', mape(test_uni$Close, fcast.arima), '%')

paste0('Error Neural Network: ', mape(test_uni$Close, fcast.nn), '%')

paste0('Error Exponential smoothing: ', mape(test_uni$Close, fcast.tbats), '%')