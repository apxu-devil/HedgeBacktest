
require(fOptions)
require(xts)
require(dplyr)
require(ggplot2)
require(scales)

# Final data preparations
load('rubswap.RData')
rub3m = rubmrtk[, c('USD.RUB', 'swap3m_perc', 'iv3m')]
names(rub3m) = c('rub', 'swap', 'iv')
rub3m$iv = rub3m$iv/100

#
# 3m options prices every day
# 
rub3m$call = (as.data.frame(rub3m) %>% mutate(call = GBSOption('c', rub, rub, 90/365, swap, 0, iv)@price))[, c('call')]
rub3m$put  = (as.data.frame(rub3m) %>% mutate(put = GBSOption('p', rub, rub, 90/365, swap, 0, iv)@price))[, c('put')]
#autoplot(rub3m$call)


# Option price as base asset ratio
local({
  
  callperc = rub3m$call / rub3m$rub
  hist(callperc, breaks=30, main = 'Call/Usd distribution')
  cat('Average call price: ', percent(mean(callperc)))
  
})


# What if we do not hedge?

local({
  
  usd_diff = lag(diff(rub3m$rub, 90*5/7), k=-90*5/7)
  usd_roc = usd_diff /  rub3m$rub %>% na.omit
  autoplot(usd_roc)
  #hist(usd_diff, breaks=30)
  cat('Average 90-days USD price change: ', format(mean(usd_roc, na.rm = T), digits=4))
  
})

# Does the rise of USD covers call premium?

  usd_up = usd_diff; usd_up[usd_up<0, ]=0
  
  call_result = (usd_up - rub3m$call) / rub3m$rub
  data.frame(
    Period = c('All', '2010 - 2013', '2014+'), 
    Result = format(c(mean(call_result, na.rm = T), 
               mean(call_result['2010/2013']), 
               mean(call_result['2014'])
               ), digits=2)
    )
  

  # If USD rises, average coverage
  mean(call_result[call_result>0,])
  
  # In all cases, average coverage
  mean(na.omit(call_result))



usd_down = usd_diff; usd_down[usd_down>0, ]=0
mean(-usd_diff[usd_diff<0, ]/rub3m$rub)
mean((-usd_diff[usd_diff<0, ] - rub3m$call)/rub3m$rub, na.rm = T)

 # Price change to call price
rub3m$difftocall = rub3m$diff / rub3m$call 

absDiffToCall = abs(rub3m$diff) / rub3m$call
summary(absDiffToCall)
autoplot(absDiffToCall)

 autoplot(
   merge(call_result, -usd_diff/rub3m$rub), facets=NULL
  )

mean(call_result-usd_diff/rub3m$rub, na.rm=T)



