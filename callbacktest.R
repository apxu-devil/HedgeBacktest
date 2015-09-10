
require(fOptions)
require(xts)
require(dplyr)
require(ggplot2)

# Final data preparations
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
rub3m$callperc = rub3m$call / rub3m$rub
hist(rub3m$callperc, breaks=30)
summary(rub3m$callperc)


# USD price change during hedge period
rub3m$diff = lag(diff(rub3m$rub, 90*5/7), k=-90*5/7)

  # Only risings
  rub3m$diffup = rub3m$diff
  rub3m$diffup[which(rub3m$diffup<0),]=0

  # Only falling
  rub3m$diff_down = rub3m$diff
  rub3m$diff_down[which(rub3m$diff_down>0),]=0

head(rub3m)
tail(rub3m)

rub3m$difftocall = rub3m$diff / rub3m$call 

summary(rub3m$calltodiff[which(rub3m$calltodiff>0),])
summary(rub3m$calltodiff[which(rub3m$calltodiff<0),])

hist(rub3m$calltodiff, breaks = 100)

summary(rub3m$calltodiff)




# Financial result at the start point
# USD change minus option price
rub3m$result = (lag(rub3m$rubup, k = -90*5/7) - rub3m$call) # result for option only
 
rub3m$result1 = (lag(rub3m$absdiff, k = -90*5/7) - rub3m$call) # result for option and usd-fall

autoplot(rub3m[,c('result', 'result1')], facet=NULL)

head(rub3m$resultperc)
tail(rub3m$resultperc)

#test
rub3m$rollror = diff(rub3m$rub, k = 90*5/7)/rub3m$rub)

rub3m$normal = diff(rub3m$rub)/rub3m$rub+1 

autoplot(
  cumprod(rub3m$normal[-1,] )
  )
#--=


rub3m$resultperc = rub3m$result / rub3m$rub
rub3m$rubupperc = rub3m$rubup / rub3m$rub

rub3m$rubup[which(is.na( rub3m$rubup))] = 0

autoplot(rub3m['2010/2013', c('rubupperc', 'resultperc')], geom='area')

summary(rub3m$resultperc['2010/2013'])
hist(rub3m$resultperc['2010/2013'], breaks = 30)

qqnorm(na.omit(rub3m$resultperc[,1]))


