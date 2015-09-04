
require(fOptions)


rub3m = rubmrtk[, c('USD.RUB', 'swap3m_perc', 'iv3m')]
names(rub3m) = c('rub', 'swap', 'iv')
rub3m$iv = rub3m$iv/100

rub3m$call = (as.data.frame(rub3m) %>% mutate(call = GBSOption('c', rub, rub, 90/365, swap, 0, iv)@price))[, c('call')]
autoplot(rub3m$call)


rub3m$rubup = diff(rub3m$rub, 90*5/7) 

rub3m$rubup[which(rub3m$rubup<0),]=0

# Financial result at the start point
rub3m$result = (lag(rub3m$rubup, k = -90*5/7) - rub3m$call)

head(rub3m$resultperc)
tail(rub3m$resultperc)

rub3m$resultperc = rub3m$result / rub3m$rub
rub3m$rubupperc = rub3m$rubup / rub3m$rub

rub3m$rubup[which(is.na( rub3m$rubup))] = 0

autoplot(rub3m['2010/2013', c('rubupperc', 'resultperc')], geom='area')

summary(rub3m$resultperc['2010/2013'])
hist(rub3m$resultperc['2010/2013'], breaks = 30)

qqnorm(na.omit(rub3m$resultperc[,1]))


