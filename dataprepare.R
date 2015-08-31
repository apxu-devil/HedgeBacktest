

require(quantmod, quietly = T)
library(dplyr, quietly = T)
require(foreach)
require(tidyr)
require(ggplot2)


#
# Download and meerge USDRUB data
#

allUSDRUB = NULL

foreach (i = 1:15) %do% {
  
  getFX('USD/RUB', from = paste(2000 + i, '-01-01', sep=''), to = paste(2000 + i + 1, '-01-01', sep=''),  )
  
  if(is.null(allUSDRUB)) {
    
    allUSDRUB = USDRUB
    } else {
      
      allUSDRUB = c(allUSDRUB, USDRUB)
    }
}


#
# Get data and split swaps data
# 

swap3m = read.csv(text=readClipboard(), sep = '\t')
names(swap3m) = c('Date', 'swap3m')
swap3m$Date = as.Date(swap3m$Date, '%d.%m.%Y')

swap1y = read.csv(text=readClipboard(), sep = '\t')
names(swap1y) = c('Date', 'swap1y')
swap1y$Date = as.Date(swap1y$Date, '%d.%m.%Y')

swaps = full_join(swap1y, swap3m, by = 'Date')
swaps = as.xts(x = swaps[,2:3,drop=F], order.by = swaps$Date) %>% na.locf


rubswap = cbind.xts(allUSDRUB, swaps, all = c(T,T)) %>% na.locf %>% na.omit


rubswap = (rubswap %>% mutate(swap3m_perc = swap3m/1000000/UsdRub*90, swap1y_perc = swap1y/1000000/UsdRub*365))
which(is.na(rubswap$swap3m))


plotswap = gather(rubswap, 'period', 'swap', 5:6) %>% filter(Date>as.Date('2006-01-01'))
ggplot(plotswap, aes(x = Date, y=swap, color=period)) + geom_line()



