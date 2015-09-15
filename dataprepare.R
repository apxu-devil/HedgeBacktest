

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

rm(USDRUB)
allUSDRUB = allUSDRUB[which(ROC(allUSDRUB$USD.RUB)!=0), ]


#
# Get historical IV
# 

iv = read.csv('usdrub_iv.csv', sep=';') 
iv = xts(x = iv[,2:4], order.by = as.Date(as.character(iv[,1]), format='%d.%m.%Y'))
names(iv) = c('iv1m', 'iv3m', 'iv1y')

#
# Get data and split swaps data
# 

### TODO: Change data source to files

swap3m = read.csv(file='swap3m.csv', sep = ';')
names(swap3m) = c('Date', 'swap3m')
swap3m$Date = as.Date(swap3m$Date, '%d.%m.%Y')

swap1y = read.csv(file='swap1y.csv', sep = ';')
names(swap1y) = c('Date', 'swap1y')
swap1y$Date = as.Date(swap1y$Date, '%d.%m.%Y')


swaps = full_join(swap1y, swap3m, by = 'Date')
swaps = as.xts(x = swaps[,2:3,drop=F], order.by = swaps$Date) %>% na.locf

# Merge rub and swap data
rubswap = cbind.xts(allUSDRUB, swaps, all = c(F,T)) %>% na.locf %>% na.omit

rubmrtk = cbind.xts(rubswap, iv, all = c(T,T)) %>% na.omit

# Calc swaps in % annual
rubmrtk$swap3m_perc = (as.data.frame(rubmrtk) %>% mutate(swp3 = swap3m/1000000/USD.RUB*90) %>% select(swp3))[[1]]
rubmrtk$swap1y_perc = (as.data.frame(rubmrtk) %>% mutate(swp1 = swap1y/1000000/USD.RUB*365) %>% select(swp1))[[1]]

# Save data
save(rubmrtk, file = 'rubswap.RData')


