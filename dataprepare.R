

require(quantmod, quietly = T)
library(dplyr, quietly = T)
require(foreach)
require(tidyr)
require(ggplot2)

options(scipen = 999)

#
# Download and meerge USDRUB data
#

allUSDRUB = NULL
foreach (i = 1:15) %do% {
  
  getFX('USD/RUB', from = paste(2000 + i, '-01-01', sep=''), to = paste(2000 + i + 1, '-01-01', sep=''),)
  if(is.null(allUSDRUB)) allUSDRUB=USDRUB else allUSDRUB=c(allUSDRUB, USDRUB)
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

# Merge rub and swap data
rubswap = cbind.xts(allUSDRUB, swaps, all = c(T,T)) %>% na.locf %>% na.omit

# Calc swaps in % annual
rubswap$swap3m_perc = (as.data.frame(rubswap) %>% mutate(swp3 = swap3m/1000000/USD.RUB*90) %>% select(swp3))[[1]]
rubswap$swap1y_perc = (as.data.frame(rubswap) %>% mutate(swp1 = swap1y/1000000/USD.RUB*365) %>% select(swp1))[[1]]

# Plot swaps
autoplot.zoo(window(rubswap[,4:5], start = '2006-01-01'), facets = NULL)

# Save data
save(rubswap, file = 'rubswap.RData')


