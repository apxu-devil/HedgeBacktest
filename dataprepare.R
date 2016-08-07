

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
  
  getFX('USD/RUB', from = paste0(2000 + i, '-01-01'), to = paste0(2000 + i + 1, '-01-01'))
  if(is.null(allUSDRUB)) allUSDRUB=USDRUB else allUSDRUB=c(allUSDRUB, USDRUB)
}

rm(USDRUB)
allUSDRUB = allUSDRUB[which(ROC(allUSDRUB$USD.RUB)!=0), ]




#
# Get data and split swaps data
# 


swap3m = read.csv(file='swap3m.csv', sep = ';')
names(swap3m) = c('Date', 'swap3m')
swap3m$Date = as.Date(swap3m$Date, '%d.%m.%Y')

swap1y = read.csv(file='swap1y.csv', sep = ';')
names(swap1y) = c('Date', 'swap1y')
swap1y$Date = as.Date(swap1y$Date, '%d.%m.%Y')


swaps = full_join(swap1y, swap3m, by = 'Date')
swaps = as.xts(x = swaps[,2:3,drop=F], order.by = swaps$Date) %>% na.locf

# temp
#allUSDRUB = rub3m$rub

# Merge rub and swap data
rub = cbind.xts(allUSDRUB, swaps, all = c(F,T)) %>% na.locf %>% na.omit

# Swaps in % annual

rub$swap3m = (as.data.frame(rub) %>% mutate(swap3m = swap3m/10000/rub/90*365))$swap3m
rub$swap1y = (as.data.frame(rub) %>% mutate(swap1y = swap1y/10000/rub))$swap1y
#
# Get historical IV
# 

iv = read.csv('usdrub_iv.csv', sep=';') 
iv = xts(x = iv[,2:4]/100, order.by = as.Date(as.character(iv[,1]), format='%d.%m.%Y'))
names(iv) = c('iv1m', 'iv3m', 'iv1y')
rub = cbind.xts(rub, iv, all = c(T,T)) %>% na.omit

# Save data
save(rubmrtk, file = 'rub.RData')


