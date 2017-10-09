library(quantmod)
library(tidyquant)
currency<-tq_get(x = c("BTCUSD=X"),from="2014-01-01", complete_cases = FALSE,get="stock.prices")
gg<-ggplot(currency,aes(x=date,y=close))
gg<-gg+geom_line()
gg
