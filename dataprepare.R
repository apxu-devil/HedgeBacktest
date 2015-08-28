

require(quantmod, quietly = T)
library(dplyr, quietly = T)
require(foreach)
require(tidyr)
require(ggplot2)


allUSDRUB = NULL

foreach (i = 1:15) %do% {
  
  getFX('USD/RUB', from = paste(2000 + i, '-01-01', sep=''), to = paste(2000 + i + 1, '-01-01', sep=''),  )
  
  if(is.null(allUSDRUB)) {
    
    allUSDRUB = USDRUB
    } else {
      
      allUSDRUB = c(allUSDRUB, USDRUB)
    }
}


# allUSDRUB = (allUSDRUB[, c('Date', 'USD.RUB')])
# allUSDRUB = as.data.frame(allUSDRUB)
# allUSDRUB$Date = as.character(allUSDRUB$Date)
# 
# allUSDRUB = .xts(x = allUSDRUB[, 2, drop=F], index = as.Date(allUSDRUB[,1]))
# 
# plot(allUSDRUB)



swap3m = read.csv(text=readClipboard(), sep = '\t')
names(swap3m) = c('Date', 'swap3m')
swap3m$Date = as.Date(swap3m$Date, '%d.%m.%Y')

swap1y = read.csv(text=readClipboard(), sep = '\t')
names(swap1y) = c('Date', 'swap1y')
swap1y$Date = as.Date(swap1y$Date, '%d.%m.%Y')

swaps = full_join(swap1y, swap3m, by = 'Date')

swaptest = as.xts(x = swaps[,2:3,drop=F], order.by = swaps$Date) %>% na.locf

swaps = na.locf(swaps) 

swaps$Date = as.Date(swaps$Date)
swaps = as.data.frame(swaps)


plotswap = gather(swaps, 'period', 'swap', 2:3) %>% filter(Date>as.Date('2005-01-01'))
ggplot(plotswap, aes(x = Date, y=swap, color=period)) + geom_line()


rub = read.csv(text=readClipboard())

rub = rub %>% select(3, 8)
names(rub) = c('Date', 'UsdRub')

head(rub)

rub$Dates = as.Date(as.character(rub$Dates), format='%Y%m%d')

allUSDRUB = as.data.frame(allUSDRUB)
allUSDRUB$Date = row.names(allUSDRUB)
allUSDRUB$Date = as.Date(allUSDRUB$Date)
rubswap = left_join(allUSDRUB, swaps, by='Date')

rubswap = (rubswap %>% mutate(swap3m_perc = swap3m/1000000/UsdRub*90, swap1y_perc = swap1y/1000000/UsdRub*365))
which(is.na(rubswap$swap3m))


plotswap = gather(rubswap, 'period', 'swap', 5:6) %>% filter(Date>as.Date('2006-01-01'))
ggplot(plotswap, aes(x = Date, y=swap, color=period)) + geom_line()



