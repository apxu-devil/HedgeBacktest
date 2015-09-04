
require(fOptions)


rub3mp = rubmrtk[, c('USD.RUB', 'swap3m_perc', 'iv3m')]
names(rub3mp) = c('rub', 'swap', 'iv')
rub3mp$iv = rub3mp$iv/100

rub3mp$put = (as.data.frame(rub3mp) %>% mutate(put = GBSOption('p', rub, rub, 90/365, swap, 0, iv)@price))[, c('put')]
autoplot(rub3mp$put)


rub3mp$rubdown = diff(rub3mp$rub, 90*5/7) 

rub3mp$rubdown[which(rub3mp$rubdown>0),]=0

# Financial result at the start point
rub3mp$result = (-lag(rub3mp$rubdown, k = -90*5/7) - rub3mp$put)

rub3mp$resultperc = rub3mp$result / rub3mp$rub
rub3mp$rubdownperc = rub3mp$rubdown / rub3mp$rub

head(rub3mp)
tail(rub3mp)

rub3mp$rubdown[which(is.na( rub3mp$rubdown))] = 0
rub3mp$rubdownperc[which(is.na( rub3mp$rubdownperc))] = 0
rub3mp$resultperc[which(is.na( rub3mp$resultperc))] = 0

autoplot(rub3mp['2010/2015', c('rubdownperc', 'resultperc')], geom='area')

summary(rub3mp$resultperc['2010/2015'])
hist(rub3mp$resultperc['2010/2015'], breaks = 30)

qqnorm(na.omit(rub3mp$resultperc[,1]))
