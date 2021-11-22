library(tidyquant)
library(DT)
library(plotly)

spy_price <- tq_get('SUSA',
                    from = '2020-03-01',
                    to = "2021-11-15",
                    get = 'stock.prices')

spy_price %>%
  plot_ly(x = ~date,
          type = 'candlestick',
          open = ~open,
          close = ~close,
          high = ~high,
          low = ~low) %>%
  layout(title = 'SUSA Price since March 2019',
         xaxis = list(rangeslider = list(visible = F)))
